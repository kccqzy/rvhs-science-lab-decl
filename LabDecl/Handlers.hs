{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module LabDecl.Handlers where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.State (get, put, evalStateT)
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Control.Concurrent
import Control.Error
import Control.Lens ((^.))
import qualified Data.Char as Char
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Attoparsec.ByteString.Char8 as PC
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Aeson as JSON
import qualified Data.Acid as Acid
import qualified Data.Acid.Advanced as Acid
import qualified Network.HTTP.Types as HTTP
import qualified Network.WebSockets as WS
import Network.Wai.Parse (lbsBackEnd)
import Text.Cassius (cassiusFile, cassiusFileReload)
import Text.Hamlet (hamletFile, hamletFileReload)
import Text.Julius (juliusFile, juliusFileReload)
import Text.Jasmine (minifym)
import Yesod.Core
import Yesod.Form
import Yesod.WebSockets (webSockets, WebSocketsT, sendTextData)
import Yesod.EmbeddedStatic

import LabDecl.Utilities
import LabDecl.Types
import LabDecl.AcidicModels
import LabDecl.StudentCSV
import LabDecl.Models

-- | The foundation data type.
data LabDeclarationApp = LabDeclarationApp {
  getStatic :: EmbeddedStatic,
  getAcid :: Acid.AcidState Database,
  getNotifyChan :: TChan ()
  }

$(mkEmbeddedStatic False "eStatic" [embedDir "static"])

-- | The routing. Routes of the form /api/items allow GET and POST,
-- which are list* and add* respectively. Routes of the form
-- /api/items/#Int allow GET PUT DELETE, which are lookup*ById,
-- replace* and remove* respectively. GET requests of /api/items allow
-- searching through query args. Most of them just does simple
-- marshalling and then invoke the events in LabDecl.AcidicModels.
$(mkYesod "LabDeclarationApp" [parseRoutes|
/api/ccas                 CcasR          GET POST DELETE
/api/ccas/#CcaId          CcaR           GET PUT DELETE
/api/subjects             SubjectsR      GET POST DELETE
/api/subjects/#SubjectId  SubjectR       GET PUT DELETE
/api/teachers             TeachersR      GET POST DELETE
/api/teachers/#TeacherId  TeacherR       GET PUT DELETE
/api/students             StudentsR      GET POST DELETE
/api/students/#StudentId  StudentR       GET PUT DELETE
/admin                    AdminHomeR     GET
/admin/ccas               AdminCcasR     GET
/admin/subjects           AdminSubjectsR GET
/admin/teachers           AdminTeachersR GET
/admin/students           AdminStudentsR GET
/static                   StaticR        EmbeddedStatic getStatic
|])

instance Yesod LabDeclarationApp where
  -- | Static files.
  addStaticContent = embedStaticContent getStatic StaticR minifym

  -- | Use JSON for InvalidArgs error, which happends during form submission.
  errorHandler (InvalidArgs ia) = return . toTypedContent $ object [ "meta" .= object [ "code" .= (400 :: Int), "details" .= ia ] ]
  errorHandler other = defaultErrorHandler other

  -- | Always upload to memory.
  fileUpload _ _ = FileUploadMemory lbsBackEnd

instance RenderMessage LabDeclarationApp FormMessage where
  renderMessage _ _ = defaultFormMessage

-- | Generically handles a database query. Sends either a normal JSON
-- response, or establishes a WebSocket connection for push
-- notifications. Implementation note: there are lifts everywhere: the
-- mtl approach of monad transformers broke because there are two
-- Readers in the stack now.
acidQueryHandler :: (ToHTTPStatus (Acid.MethodResult ev),
                     Acid.QueryEvent ev,
                     ToJSON (Acid.MethodResult ev),
                     Acid.MethodState ev ~ Database) => ev -> Handler Value
acidQueryHandler event = do
  acid <- getAcid <$> ask
  let genResponse = do
        result <- liftIO $ Acid.query acid event
        let httpStatus = toHttpStatus result
        let jsonResponse = object [ "meta" .= object [ "code" .= HTTP.statusCode httpStatus ], "data" .= result ]
        return (httpStatus, jsonResponse)

  -- WebSockets push
  webSockets $ do
    notifyChan <- getNotifyChan <$> lift ask
    readChan <- liftIO . atomically . dupTChan $ notifyChan

    -- use StateT to handle cases when the database is updated but the data the client is interested in did not change
    (`evalStateT` JSON.Null) $ do
      let sendResponse = do
            (_, response) <- lift2 genResponse
            prevResponse <- get
            when (response /= prevResponse) $ do
              lift $ sendTextData $ JSON.encode response
              put response
      sendResponse
      forever $ do
        r <- liftIO $ race  -- we either wait for 10s to send a ping to keepalive the connection, or wait for a notification
             (atomically (readTChanAll_ readChan))
             (threadDelay 10000000)
        case r of
         Left _ -> sendResponse
         Right _ -> lift sendPing

  -- non WebSocket alternative
  (httpStatus, jsonResponse) <- genResponse
  if HTTP.statusIsSuccessful httpStatus
    then return jsonResponse
    else sendResponseStatus httpStatus jsonResponse

  where sendPing :: (MonadIO m) => WebSocketsT m ()
        sendPing = ReaderT $ liftIO . flip WS.sendPing CL.empty
        readTChanAll_ = liftM2 (>>) readTChan tryReadTChanAll_
        tryReadTChanAll_ chan = whileJust_ (tryReadTChan chan) return

-- | Generically handles a database update. When an update is
-- successful, send a notification to all listening query handlers.
acidUpdateHandler :: (Acid.UpdateEvent ev,
                      ToJSON a, ToJSON e,
                      Acid.MethodResult ev ~ Either e a,
                      Acid.MethodState ev ~ Database) => ev -> Handler Value
acidUpdateHandler event = do
  acid <- getAcid <$> ask
  notifyChan <- getNotifyChan <$> ask
  result <- liftIO $ Acid.update acid event
  let httpStatus = toHttpStatus result
  let jsonResponse = case result of
        Left e  -> object [ "meta" .= object ["code" .= HTTP.statusCode httpStatus, "details" .= e ], "data" .= jsonNull ]
        Right _ -> object [ "meta" .= object ["code" .= HTTP.statusCode httpStatus                 ], "data" .= jsonNull ]
  if HTTP.statusIsSuccessful httpStatus
    then do
         liftIO . atomically $ writeTChan notifyChan ()
         return jsonResponse
    else sendResponseStatus httpStatus jsonResponse
  where jsonNull :: Maybe ()
        jsonNull = Nothing

-- | Just return the result in the context of checkMMap.
checkMMapOk :: a -> Handler (Either T.Text a)
checkMMapOk = return . return

-- | Validate an email address, the format of which follows WHATWG
-- HTML5.
validateEmailBS :: C.ByteString -> Bool
validateEmailBS = (isRight .) . PC.parseOnly $ do
  username
  PC.char '@'
  domainPart `PC.sepBy1` PC.char '.'
  PC.endOfInput
  where username = PC.takeWhile1 $ PC.inClass "a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-"
        domainPart = do
          r <- PC.takeWhile1 (PC.inClass "a-zA-Z0-9") `PC.sepBy1` PC.char '-'
          guard $ C.length (C.intercalate "-" r) <= 63

-- | A stronger version of FormInput that allows coupled
-- fields. Monads assume later computations depend on results of
-- earlier ones, so only one error message can be provided.
newtype MFormInput m a = MFormInput { unMFormInput :: FormInput m a }
instance (Monad m) => Monad (MFormInput m) where
  return = MFormInput . pure
  (MFormInput (FormInput x)) >>= f = MFormInput . FormInput $ \c d e e' -> do
    r <- x c d e e'
    case r of
     Left e -> return $ Left e
     Right a -> do
       let (MFormInput (FormInput y)) = f a
       y c d e e'
  fail = MFormInput . FormInput . const . const . const . const . return . Left . (:) . T.pack
instance (Monad m) => Functor (MFormInput m) where
  fmap = liftM
instance (Monad m) => Applicative (MFormInput m) where
  pure = return
  (<*>) = ap

runMInputPost :: MonadHandler m => MFormInput m a -> m a
runMInputPost = runInputPost . unMFormInput

-- | The CCA form request expected by POST and PUT. No validation
-- here.
ccaForm :: CcaId -> FormInput Handler Cca
ccaForm cid = Cca
              <$> pure cid
              <*> ireq textField "name"
              <*> ireq textField "category"

-- | The subject form request expected by POST and PUT. Duplicates in
-- level are auto-removed.
subjectForm :: SubjectId -> FormInput Handler Subject
subjectForm sid = Subject
                  <$> pure sid
                  <*> iopt textField "code"
                  <*> ireq textField "name"
                  <*> ireq checkBoxField "science"
                  <*> ireq levelField "level"
  where levelField = checkMMap fw bw $ checkboxesFieldList $ map (liftM2 (,) (T.pack . show) id) [1..6]
          where fw = checkMMapOk . IntSet.fromList
                bw = IntSet.toList

teacherForm :: TeacherId -> FormInput Handler Teacher
teacherForm tid = Teacher
                  <$> pure tid
                  <*> ireq checkBoxField "admin"
                  <*> ireq textField "unit"
                  <*> ireq textField "name"
                  <*> ireq textField "witness"
                  <*> ireq emailField "email"
  where emailField = checkMMap fw bw (checkBool validateEmail ("email wrong format" :: T.Text) textField)
          where fw = checkMMapOk . Email
                bw (Email e) = e
                validateEmail = validateEmailBS . T.encodeUtf8

studentForm :: StudentId -> FormInput Handler Student
studentForm sid = unMFormInput $ do
                  a <- mireq textField "name"
                  b <- mireq textField "chinesename"
                  c <- miopt teacherIdField "witness"
                  d <- mireq classField "class"
                  e <- mireq intField "indexno"
                  f <- mireq (subjCombiField d) "subj"
                  g <- mireq nricField "nric"
                  return $ Student sid a b c d e f g SubmissionNotOpen
  where mireq = (MFormInput .) . ireq
        miopt = (MFormInput .) . iopt
        classField = checkMMap fw bw textField
          where bw (Class (l, c)) = T.pack $ show l ++ [c]
                fw = return . parseClass . T.encodeUtf8
                parseClass :: C.ByteString -> Either T.Text Class
                parseClass = ((note "wrong class format" . hush) .) . PC.parseOnly $ do
                  l <- Char.digitToInt <$> PC.digit
                  guard $ 1 <= l && l <= 6
                  c <- PC.satisfy $ PC.inClass "A-NP-Z"
                  PC.endOfInput
                  return $ Class (l, c)
        nricField = checkMMap fw bw (checkBool validatePartialNric ("wrong nric format" :: T.Text) textField)
          where bw (Nric s) = s
                fw = checkMMapOk . Nric
                validatePartialNric = validatePartialNricBS . T.encodeUtf8
                validatePartialNricBS = (isRight .) . PC.parseOnly $ do
                  PC.count 4 PC.digit
                  PC.satisfy $ PC.inClass "JZIHGFEDCBAXWUTRQPNMLK"
                  PC.endOfInput
        teacherIdField = checkMMap fw bw intField
          where bw (TeacherId i) = i
                fw i = do
                  acid <- getAcid <$> ask
                  let rv = TeacherId i
                  e <- liftIO $ Acid.query acid $ LookupTeacherById (TeacherId i)
                  case e of
                   Nothing -> return . Left $ ("no such teacher" :: T.Text)
                   Just _ -> return . Right $ rv
        subjCombiField klass = checkMMap fw bw (checkboxesField optlist)
          where fw = checkMMapOk . Set.fromList
                bw = Set.toList
                optlist = do
                  acid <- getAcid <$> ask
                  let (Class (level, _)) = klass
                  e <- liftIO $ Acid.query acid $ ListSubjectsByLevel level
                  optionsPairs . map (liftM2 (,) (T.pack . show . unSubjectId) id . (^. subjectId)) . Set.toList $ e
                unSubjectId (SubjectId i) = i

-- | Enumerate all CCAs with all information. Public.
getCcasR :: Handler Value
getCcasR = acidQueryHandler ListCcas

-- | Add new CCA. Requires admin.
postCcasR :: Handler Value
postCcasR = acidUpdateHandler . AddCca =<< runInputPost (ccaForm (CcaId 0))

-- | Get information about a single Cca. Public.
getCcaR :: CcaId -> Handler Value
getCcaR = acidQueryHandler . LookupCcaById

-- | Edit CCA. Requires admin.
putCcaR :: CcaId -> Handler Value
putCcaR = (acidUpdateHandler . ReplaceCca =<<) . runInputPost . ccaForm

-- | Delete CCA. Requires Admin.
deleteCcaR :: CcaId -> Handler Value
deleteCcaR = acidUpdateHandler . RemoveCca

deleteCcasR :: Handler Value
deleteCcasR = acidUpdateHandler RemoveAllCcas

getSubjectsR :: Handler Value
getSubjectsR = acidQueryHandler ListSubjects

postSubjectsR :: Handler Value
postSubjectsR = do
  subject <- runInputPost (subjectForm (SubjectId 0))
  force <- runInputPost (ireq checkBoxField "force")
  acidUpdateHandler $ AddSubject force subject

getSubjectR :: SubjectId -> Handler Value
getSubjectR = acidQueryHandler . LookupSubjectById

putSubjectR :: SubjectId -> Handler Value
putSubjectR = (acidUpdateHandler . ReplaceSubject =<<) . runInputPost . subjectForm

deleteSubjectR :: SubjectId -> Handler Value
deleteSubjectR = acidUpdateHandler . RemoveSubject

deleteSubjectsR :: Handler Value
deleteSubjectsR = acidUpdateHandler RemoveAllSubjects

getTeachersR :: Handler Value
getTeachersR = acidQueryHandler ListTeachers

postTeachersR :: Handler Value
postTeachersR = do
  teacher <- runInputPost (teacherForm (TeacherId 0))
  force <- runInputPost (ireq checkBoxField "force")
  acidUpdateHandler $ AddTeacher force teacher

getTeacherR :: TeacherId -> Handler Value
getTeacherR = acidQueryHandler . LookupTeacherById

putTeacherR :: TeacherId -> Handler Value
putTeacherR = (acidUpdateHandler . ReplaceTeacher =<<) . runInputPost . teacherForm

deleteTeacherR :: TeacherId -> Handler Value
deleteTeacherR = acidUpdateHandler . RemoveTeacher

deleteTeachersR :: Handler Value
deleteTeachersR = acidUpdateHandler RemoveAllTeachers

getStudentsR :: Handler Value
getStudentsR = undefined

postStudentsR :: Handler Value
postStudentsR = undefined

getStudentR :: StudentId -> Handler Value
getStudentR = undefined

putStudentR :: StudentId -> Handler Value
putStudentR = undefined

deleteStudentR :: StudentId -> Handler Value
deleteStudentR = undefined

deleteStudentsR :: Handler Value
deleteStudentsR = acidUpdateHandler RemoveAllStudents

-- |
-- = HTML handlers

adminSite :: Widget
adminSite = do
  addStylesheet $ StaticR bootstrap_min_css
  addStylesheet $ StaticR bootstrapt_min_css
  addScript $ StaticR jquery_js
  addScript $ StaticR underscore_js
  addScript $ StaticR react_dev_js
  addScript $ StaticR bootstrap_js
  toWidget $(juliusFileReload "templates/admin.js")

getAdminHomeR :: Handler Html
getAdminHomeR = defaultLayout $ do
  setTitle "RVHS Science Lab Undertaking :: Admin Console"
  adminSite

getAdminCcasR :: Handler Html
getAdminCcasR = defaultLayout $ do
  setTitle "RVHS Science Lab Undertaking :: Admin Console :: Manage CCAs"
  adminSite

getAdminTeachersR :: Handler Html
getAdminTeachersR = defaultLayout $ do
  setTitle "RVHS Science Lab Undertaking :: Admin Console :: Manage Teachers"
  adminSite

getAdminSubjectsR :: Handler Html
getAdminSubjectsR = defaultLayout $ do
  setTitle "RVHS Science Lab Undertaking :: Admin Console :: Manage Subjects"
  adminSite

getAdminStudentsR :: Handler Html
getAdminStudentsR = defaultLayout $ do
  setTitle "RVHS Science Lab Undertaking :: Admin Console :: Manage Students"
  adminSite
