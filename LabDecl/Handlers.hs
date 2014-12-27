{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module LabDecl.Handlers where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.State (get, put, evalStateT, execStateT, StateT)
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async hiding (race)
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
import qualified Data.Attoparsec.Text as PT
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Aeson as JSON
import qualified Data.Acid as Acid
import qualified Data.Acid.Advanced as Acid
import Data.Conduit (($$))
import Data.Conduit.Binary (sinkLbs)
import qualified Network.HTTP.Types as HTTP
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import Network.Wai.Parse (lbsBackEnd)
import Text.Cassius (cassiusFile, cassiusFileReload)
import Text.Hamlet (hamletFile, hamletFileReload)
import Text.Julius (juliusFile, juliusFileReload)
import Text.Jasmine (minifym)
import Codec.Text.Detect (detectEncodingName)
import qualified Data.Text.ICU.Convert as ICU
import Yesod.Core
import Yesod.Form hiding (emailField)
import Yesod.WebSockets (webSockets, WebSocketsT, sendTextData, receiveData, race)
import Yesod.EmbeddedStatic

import LabDecl.Utilities
import LabDecl.Types
import LabDecl.AcidicModels
import LabDecl.StudentCSV
import LabDecl.Models
import LabDecl.ErrMsg

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
/api/subjects/test-decode TestDecodeR    GET
!/api/subjects/#SubjectId SubjectR       GET PUT DELETE
/api/teachers             TeachersR      GET POST DELETE
/api/teachers/#TeacherId  TeacherR       GET PUT DELETE
/api/students             StudentsR      GET POST DELETE
/api/students/many        ManyStudentsR  POST
/api/students/submit      StudentSubmitR POST
!/api/students/#StudentId StudentR       GET PUT DELETE
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
        r <- race
             (lift receiveNoData) -- wait for client data or close request (exception will be thrown)
             (liftIO $ race
              (atomically (readTChanAll_ readChan)) -- wait for update notification
              (threadDelay 10000000)) -- wait for 10s
        case r of
         Left _ -> return () -- ignore client data
         Right r' -> case r' of
           Left _ -> sendResponse
           Right _ -> lift sendPing

  -- non WebSocket alternative
  (httpStatus, jsonResponse) <- genResponse
  if HTTP.statusIsSuccessful httpStatus
    then return jsonResponse
    else sendResponseStatus httpStatus jsonResponse

  where sendPing :: (MonadIO m) => WebSocketsT m ()
        sendPing = ReaderT $ liftIO . flip WS.sendPing CL.empty
        receiveNoData = void $ (receiveData :: (MonadIO m) => WebSocketsT m T.Text)
        readTChanAll_ = liftM2 (>>) readTChan tryReadTChanAll_
        tryReadTChanAll_ chan = whileJust_ (tryReadTChan chan) return

-- | Generically handles a database update. When an update is
-- successful, send a notification to all listening query handlers.
acidUpdateHandler :: (Acid.UpdateEvent ev,
                      ToJSON r, ToJSON e,
                      Acid.MethodResult ev ~ Either e r,
                      Acid.MethodState ev ~ Database) =>
                     ev -> Handler Value
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

-- | Parses a subject code string into a set of subjects.
parseSubjectCode :: Map T.Text Subject -> T.Text -> [Set Subject]
parseSubjectCode allSubjects = map snd . filter (T.null . fst) . execStateT (parseSubjects allSubjects) . (,Set.empty)
  where parseSubjects = many . subjectCodeParser
        subjectCodeParser :: Map T.Text Subject -> StateT (T.Text, Set Subject) [] ()
        subjectCodeParser candidates = do
          (str, parsed) <- get
          (code, subject) <- lift $ Map.toList candidates
          let str' = T.dropWhile (PT.inClass " \t,;.&/\\+") str
          case T.stripPrefix code str' of
           Nothing -> mzero
           Just remaining -> put (remaining, Set.insert subject parsed)

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

mireq :: Field Handler a -> T.Text -> MFormInput Handler a
mireq = (MFormInput .) . ireq

miopt :: Field Handler a -> T.Text -> MFormInput Handler (Maybe a)
miopt = (MFormInput .) . iopt

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

levelField :: Field Handler IntSet
levelField = checkMMap fw bw $ checkboxesFieldList $ map (liftM2 (,) (T.pack . show) id) [1..6]
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

emailField :: Field Handler Email
emailField = checkMMap fw bw (checkBool validateEmail ("email wrong format" :: T.Text) textField)
  where fw = checkMMapOk . Email
        bw (Email e) = e
        validateEmail = validateEmailBS . T.encodeUtf8
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

studentForm :: StudentId -> FormInput Handler Student
studentForm sid = unMFormInput $ do
                  a <- mireq textField "name"
                  b <- mireq textField "chinesename"
                  c <- miopt teacherIdField "witness"
                  d <- mireq classField "class"
                  e <- mireq intField "indexno"
                  f <- miopt (subjCombiField d) "subj"
                  let f' = maybe Set.empty id f
                  g <- mireq nricField "nric"
                  return $ Student sid a b c d e f' g SubmissionNotOpen

classField :: Field Handler Class
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

nricField :: Field Handler Nric
nricField = checkMMap fw bw (checkBool validatePartialNric ("wrong nric format" :: T.Text) textField)
  where bw (Nric s) = s
        fw = checkMMapOk . Nric

validatePartialNric :: T.Text -> Bool
validatePartialNric = validatePartialNricBS . T.encodeUtf8
  where validatePartialNricBS = (isRight .) . PC.parseOnly $ do
          PC.option "XXXX" (PC.stringCI "XXXX")
          PC.count 4 PC.digit
          PC.satisfy $ PC.inClass "JZIHGFEDCBAXWUTRQPNMLK"
          PC.endOfInput

teacherIdField :: Field Handler TeacherId
teacherIdField = checkMMap fw bw intField
  where bw (TeacherId i) = i
        fw i = do
          acid <- getAcid <$> ask
          let rv = TeacherId i
          e <- liftIO $ Acid.query acid $ LookupTeacherById (TeacherId i)
          case e of
           Nothing -> return . Left $ ("no such teacher" :: T.Text)
           Just _ -> return . Right $ rv

subjCombiField :: Class -> Field Handler (Set SubjectId)
subjCombiField klass = checkMMap fw bw (checkboxesField optlist)
  where fw = checkMMapOk . Set.fromList
        bw = Set.toList
        optlist = do
          acid <- getAcid <$> ask
          let (Class (level, _)) = klass
          e <- liftIO $ Acid.query acid $ ListSubjectsByLevel level
          optionsPairs . map (liftM2 (,) (T.pack . show . unSubjectId) id . (^. subjectId)) . Set.toList $ e
        unSubjectId (SubjectId i) = i

-- | Generically parses forms and handles a database update.
acidFormUpdateHandler :: (Acid.UpdateEvent ev,
                          ToJSON r, ToJSON e,
                          Acid.MethodResult ev ~ Either e r,
                          Acid.MethodState ev ~ Database,
                          HasPrimaryKey a i) =>
                         (i -> FormInput Handler a) ->
                         (Bool -> a -> ev) ->
                         i -> Handler Value
acidFormUpdateHandler form eventCon eid = do
  entity <- runInputPost (form eid)
  force <- runInputPost (ireq checkBoxField "force")
  acidUpdateHandler $ eventCon force entity

-- | Enumerate all CCAs with all information. Public.
getCcasR :: Handler Value
getCcasR = acidQueryHandler ListCcas

-- | Add new CCA. Requires admin.
postCcasR :: Handler Value
postCcasR = acidFormUpdateHandler ccaForm AddCca (CcaId 0)

-- | Get information about a single Cca. Public.
getCcaR :: CcaId -> Handler Value
getCcaR = acidQueryHandler . LookupCcaById

-- | Edit CCA. Requires admin.
putCcaR :: CcaId -> Handler Value
putCcaR = acidFormUpdateHandler ccaForm ReplaceCca

-- | Delete CCA. Requires Admin.
deleteCcaR :: CcaId -> Handler Value
deleteCcaR = acidUpdateHandler . RemoveCca

deleteCcasR :: Handler Value
deleteCcasR = acidUpdateHandler RemoveAllCcas

getSubjectsR :: Handler Value
getSubjectsR = acidQueryHandler ListSubjects

postSubjectsR :: Handler Value
postSubjectsR = acidFormUpdateHandler subjectForm AddSubject (SubjectId 0)

getSubjectR :: SubjectId -> Handler Value
getSubjectR = acidQueryHandler . LookupSubjectById

putSubjectR :: SubjectId -> Handler Value
putSubjectR = acidFormUpdateHandler subjectForm ReplaceSubject

deleteSubjectR :: SubjectId -> Handler Value
deleteSubjectR = acidUpdateHandler . RemoveSubject

deleteSubjectsR :: Handler Value
deleteSubjectsR = acidUpdateHandler RemoveAllSubjects

getTestDecodeR :: Handler Value
getTestDecodeR = do
  (level, str) <- runInputGet $ (,) <$> ireq levelField "level" <*> ireq textField "str"
  acid <- getAcid <$> ask
  allSubjects <- liftIO $ Acid.query acid $ ListSubjectsByLevel level
  let parsed = Set.toList <$> parseSubjectCode (subjectsToMap allSubjects) str
  return $ object [ "meta" .= object ["code" .=  (200 :: Int) ], "data" .= parsed ]
  where levelField = radioFieldList $ map (liftM2 (,) (T.pack . show) id) [1..6]

getTeachersR :: Handler Value
getTeachersR = acidQueryHandler ListTeachers

postTeachersR :: Handler Value
postTeachersR = acidFormUpdateHandler teacherForm AddTeacher (TeacherId 0)

getTeacherR :: TeacherId -> Handler Value
getTeacherR = acidQueryHandler . LookupTeacherById

putTeacherR :: TeacherId -> Handler Value
putTeacherR = acidFormUpdateHandler teacherForm ReplaceTeacher

deleteTeacherR :: TeacherId -> Handler Value
deleteTeacherR = acidUpdateHandler . RemoveTeacher

deleteTeachersR :: Handler Value
deleteTeachersR = acidUpdateHandler RemoveAllTeachers

data QueryEvent = forall ev. (ToHTTPStatus (Acid.MethodResult ev),
                              Acid.QueryEvent ev,
                              ToJSON (Acid.MethodResult ev),
                              Acid.MethodState ev ~ Database) => QueryEvent T.Text ev

searchByField :: Field Handler QueryEvent
searchByField = checkMMap fw bw textField
  where bw :: QueryEvent -> T.Text
        bw (QueryEvent tag _) = tag
        fw crit = do
          case crit of
           "none" -> checkMMapOk . QueryEvent crit $ ListNothing
           "all" -> checkMMapOk . QueryEvent crit $ ListStudents
           "class" -> do
             klass <- runInputGet (ireq classField "class")
             checkMMapOk . QueryEvent crit $ ListStudentsFromClass klass
           "cca" -> do
             cid <- runInputGet (ireq intField "id")
             checkMMapOk . QueryEvent crit $ ListStudentsFromCca (CcaId cid)
           "subject" -> do
             sid <- runInputGet (ireq intField "id")
             checkMMapOk . QueryEvent crit $ ListStudentsWithSubject (SubjectId sid)
           "teacher" -> do
             tid <- runInputGet (ireq intField "id")
             checkMMapOk . QueryEvent crit $ ListStudentsWithWitnesser (TeacherId tid)
           _ -> return $ Left ("no such criteria" :: T.Text)

getStudentsR :: Handler Value
getStudentsR = do
  QueryEvent _ ev <- runInputGet (ireq searchByField "searchby")
  acidQueryHandler ev

postStudentsR :: Handler Value
postStudentsR = acidFormUpdateHandler studentForm AddStudent (StudentId 0)

getStudentR :: StudentId -> Handler Value
getStudentR = acidQueryHandler . LookupStudentById

putStudentR :: StudentId -> Handler Value
putStudentR = acidFormUpdateHandler studentForm ReplaceStudent

deleteStudentR :: StudentId -> Handler Value
deleteStudentR = acidUpdateHandler . RemoveStudent

deleteStudentsR :: Handler Value
deleteStudentsR = acidUpdateHandler RemoveAllStudents

postStudentSubmitR :: Handler Value
postStudentSubmitR = do
  req <- waiRequest
  body <- liftIO $ Wai.strictRequestBody req
  case JSON.decode body of
   Nothing -> invalidArgs ["no parse"]
   Just s -> acidUpdateHandler $ PublicStudentDoSubmission s

postManyStudentsR :: Handler Value
postManyStudentsR = do
  acid <- getAcid <$> ask
  fileinfo <- runInputPost (ireq fileField "csv")
  force <- runInputPost (ireq checkBoxField "force")
  bs <- fileSource fileinfo $$ sinkLbs
  result <- runEitherT $ do
    let maybeEncoding = detectEncodingName bs -- ^ unsafePerformIO here
    encoding <- hoistEither $ note errCSVTextDecodeFailed maybeEncoding
    converter <- liftIO $ ICU.open encoding Nothing
    let csvText = ICU.toUnicode converter (CL.toStrict bs)
    csvData <- hoistEither $ parseCSV csvText
    allSubjects <- liftIO $ mapM (fmap subjectsToMap . Acid.query acid . ListSubjectsByLevel) [1..6]
    allTeachers <- liftIO $ teachersToMap <$> Acid.query acid ListTeachers
    V.forM csvData $ \(rowNumber, rawStudent@CsvStudent{..}) -> do
      -- attempt to convert rawStudent to a Student
      klass@(Class (level, _)) <- hoistEither . parseClass' . T.encodeUtf8 $ _klass
      indexNo <- hoistEither . parseInt . T.encodeUtf8 $ _indexNo
      hoistEither . note "nric wrong format" . guard . validatePartialNric $ _nric
      witness <- hoistEither $ parseWitnessName allTeachers _witness
      let subjects = parseSubjectCode (allSubjects !! (level-1)) _subjCombi
      case subjects of
       [] -> hoistEither $ Left "subject code no parse"
       [subjects] -> do
         let subjectIds = Set.mapMonotonic (^. idField) subjects
         return $ Student (StudentId 0) _name _chinese witness klass indexNo subjectIds (Nric _nric) SubmissionNotOpen
       _ -> hoistEither $ Left "subject code ambiguous"
  case result of
   Left e -> invalidArgs [TL.toStrict e]
   Right vs -> acidUpdateHandler $ AddStudents force vs
  where parseClass' = fmapL TL.fromStrict . parseClass
        parseInt :: C.ByteString -> Either TL.Text Int
        parseInt = ((note "not integer" . hush) .) . PC.parseOnly $ PC.decimal <* PC.endOfInput
        parseWitnessName :: Map T.Text Teacher -> T.Text -> Either TL.Text (Maybe TeacherId)
        parseWitnessName teacherMap s =
          case Map.lookup s teacherMap of
           Just t -> return . Just $ t ^. idField
           Nothing -> case s `elem` [ "-", "--", "---", "\8210", "\8211", "\8212", "\65112" ] of
             True -> return Nothing
             False -> Left "no parse"


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
  toWidget $(cassiusFile "templates/hover.cassius")
  adminSite
