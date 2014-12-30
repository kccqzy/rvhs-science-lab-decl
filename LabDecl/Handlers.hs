{-# LANGUAGE CPP #-}
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
import Data.List
import Data.Function
import Data.Default
import qualified Data.Char as Char
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64 as C64
import qualified Data.ByteString.Base64.Lazy as CL64
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
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (utctDay, getCurrentTime, secondsToDiffTime)
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import Network.Wai.Parse (lbsBackEnd)
import Text.Jasmine (minifym)
import Codec.Text.Detect (detectEncodingName)
import qualified Codec.Picture.Png as Png
import Web.Cookie
import Yesod.Core
import Yesod.Form hiding (emailField)
import Yesod.WebSockets (webSockets, WebSocketsT, sendTextData, receiveData, race)
import Yesod.EmbeddedStatic
import Yesod.Auth
import Yesod.Auth.GoogleEmail2

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
  getNotifyChan :: TChan (),
  getHttpManager :: HTTP.Manager
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
/api/classes              ClassesR       GET
/api/classes/#Class       ClassR         GET
/api/classes/#Class/#Int  PublicStudentR GET
/api/students             StudentsR      GET POST DELETE
/api/students/many        ManyStudentsR  POST
!/api/students/#StudentId StudentR       GET PUT DELETE
/api/students/#StudentId/submit      StudentSubmitR POST
/api/students/#StudentId/unlock      UnlockSubmissionR POST
/api/students/#StudentId/lock        LockSubmissionR POST
/admin                    AdminHomeR     GET
/admin/ccas               AdminCcasR     GET
/admin/subjects           AdminSubjectsR GET
/admin/teachers           AdminTeachersR GET
/admin/students           AdminStudentsR GET
/admin/logout             AdminLogoutR   GET
/                         HomepageR      GET
/static                   StaticR        EmbeddedStatic getStatic
/auth                     AuthR          Auth getAuth
/authstatus AuthStatusR GET
|])

instance Yesod LabDeclarationApp where

#ifdef DEVELOPMENT

  isAuthorized _ _ = do
    mbCookies <- runInputGet (iopt textField "sendcookie")
    case mbCookies of
     Nothing -> return ()
     Just cookies -> do
       let cookies' = filter ((>=2) . length) . map T.words $ T.lines cookies
       forM_ cookies' $ \(n:v:[]) ->
         setCookie (def { setCookieName = T.encodeUtf8 n, setCookieValue = T.encodeUtf8 v})
    return Authorized
  approot = ApprootStatic "http://localhost:8080"

#else

  -- | Authorisation table
  isAuthorized CcasR                 w = requirePrivilege (if w then PrivAdmin else PrivNone)
  isAuthorized (CcaR _)              w = requirePrivilege (if w then PrivAdmin else PrivNone)
  isAuthorized SubjectsR             w = requirePrivilege (if w then PrivAdmin else PrivNone)
  isAuthorized TestDecodeR           _ = requirePrivilege PrivTeacher
  isAuthorized (SubjectR _)          w = requirePrivilege (if w then PrivAdmin else PrivNone)
  isAuthorized TeachersR             w = requirePrivilege (if w then PrivAdmin else PrivTeacher)
  isAuthorized (TeacherR _)          w = requirePrivilege (if w then PrivAdmin else PrivTeacher)
  isAuthorized ClassesR              _ = requirePrivilege PrivNone
  isAuthorized (ClassR _)            _ = requirePrivilege PrivNone
  isAuthorized (PublicStudentR _ _)  _ = requirePrivilege PrivNone
  isAuthorized StudentsR             w = requirePrivilege (if w then PrivAdmin else PrivTeacher)
  isAuthorized ManyStudentsR         _ = requirePrivilege PrivAdmin
  isAuthorized (StudentR _)          w = requirePrivilege (if w then PrivAdmin else PrivTeacher)
  isAuthorized (StudentSubmitR _)    _ = requirePrivilege PrivNone
  isAuthorized (LockSubmissionR _)   _ = requirePrivilege PrivTeacher
  isAuthorized (UnlockSubmissionR _) _ = requirePrivilege PrivTeacher
  isAuthorized AdminHomeR            _ = requirePrivilege PrivTeacher
  isAuthorized AdminCcasR            _ = requirePrivilege PrivTeacher
  isAuthorized AdminSubjectsR        _ = requirePrivilege PrivTeacher
  isAuthorized AdminTeachersR        _ = requirePrivilege PrivTeacher
  isAuthorized AdminStudentsR        _ = requirePrivilege PrivTeacher
  isAuthorized AdminLogoutR          _ = requirePrivilege PrivNone
  isAuthorized HomepageR             _ = requirePrivilege PrivNone
  isAuthorized (StaticR _)           _ = requirePrivilege PrivNone
  isAuthorized (AuthR _)             _ = requirePrivilege PrivNone
  isAuthorized AuthStatusR           _ = requirePrivilege PrivNone

  -- | App root
  approot = ApprootStatic "http://gce.qzy.st"

#endif

  -- | Static files.
  addStaticContent = embedStaticContent getStatic StaticR minifym

  -- | Use JSON for InvalidArgs error, which happends during form submission.
  errorHandler (InvalidArgs ia) = return . toTypedContent $ object [ "meta" .= object [ "code" .= (400 :: Int), "details" .= ia ] ]
  errorHandler other = defaultErrorHandler other

  -- | Always upload to memory.
  fileUpload _ _ = FileUploadMemory lbsBackEnd

  -- | Route for authentication
  authRoute _ = Just $ AuthR LoginR

instance YesodAuth LabDeclarationApp where
  type AuthId LabDeclarationApp = T.Text
  authHttpManager = getHttpManager
  authPlugins _ = [ authGoogleEmail "950258003533-9tcea77akv60t4h2t94747eh48bdqcuo.apps.googleusercontent.com" "camnLwKj_w2wtwmWYBqliwdv"]
  loginDest _ = AuthStatusR
  logoutDest _ = AdminLogoutR
  getAuthId = return . Just . credsIdent
  maybeAuthId = lookupSession "_ID"

instance RenderMessage LabDeclarationApp FormMessage where
  renderMessage _ _ = defaultFormMessage

data Privilege = PrivNone
               | PrivTeacher
               | PrivAdmin
               deriving (Show, Eq, Ord)

requirePrivilege :: Privilege -> Handler AuthResult
requirePrivilege privReq
  | privReq == PrivNone = return Authorized
  | otherwise = do
      mbPriv <- getPrivilege
      case mbPriv of
       Nothing -> return AuthenticationRequired
       Just priv -> do
         setCookie (def { setCookieName = "priv", setCookieValue = C.pack (show priv) })
         return . bool (Unauthorized "Insufficient privileges.") Authorized . (>= privReq) $ priv

getPrivilege :: Handler (Maybe Privilege)
getPrivilege = do
  mu <- maybeAuthId
  case mu of
   Nothing -> return Nothing
   Just aid -> do
     setCookie (def { setCookieName = "user", setCookieValue = T.encodeUtf8 aid })
     if aid == "qzy@qzy.io"
       then return $ Just PrivAdmin
       else do
        acid <- getAcid <$> ask
        mbIdentity <- liftIO $ Acid.query acid $ LookupTeacherByEmail (Email aid)
        case mbIdentity of
         Nothing -> return $ Just PrivNone
         Just identity -> do
           case identity ^. teacherIsAdmin of
            True -> return $ Just PrivAdmin
            False -> return $ Just PrivTeacher

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

data HumanFriendlyParseResult = ParseSuccess (Set Subject)
                              | ParseAmbiguous [Set Subject]
                              | ParseIncomplete (T.Text, (Set Subject))
                              | ParseNothing T.Text
                              | ParseInternalError
                              deriving (Show, Eq)

parseSubjectCodeFriendly :: Map T.Text Subject -> T.Text -> HumanFriendlyParseResult
parseSubjectCodeFriendly allSubjects str =
  case execStateT (subjectCodesParser allSubjects) (str, Set.empty) of
   [] -> ParseInternalError -- unreachable
   [(st, su)] | st == str && Set.null su -> ParseNothing st -- single element
   (_:[]) -> ParseInternalError -- unreachable
   possibilities -> -- more than one element: last one is where nothing is parsed
     case filter (T.null . fst) possibilities of
      [] -> ParseIncomplete . head $ sortBy (compare `on` (T.length . fst)) possibilities
      [(_, subjects)] -> ParseSuccess subjects
      completedPossibilities -> ParseAmbiguous $ map snd completedPossibilities

-- | Parses a subject code string into a set of subjects.
parseSubjectCode :: Map T.Text Subject -> T.Text -> [Set Subject]
parseSubjectCode allSubjects = map snd . filter (T.null . fst) . execStateT (subjectCodesParser allSubjects) . (,Set.empty)

subjectCodesParser :: Map T.Text Subject -> StateT (T.Text, Set Subject) [] ()
subjectCodesParser = void . many . subjectCodeParser
  where subjectCodeParser candidates = do
          (str, parsed) <- get
          (code, subject) <- lift $ Map.toList candidates
          case T.stripPrefix code str of
           Nothing -> mzero
           Just remaining -> put (T.dropWhile (PT.inClass " \t,;.&/\\+") remaining, Set.insert subject parsed)

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

sgPhoneField :: Field Handler Phone
sgPhoneField = checkMMap fw bw (checkBool validatePhone ("phone wrong format" :: T.Text) textField)
  where fw = checkMMapOk . Phone
        bw (Phone p) = p
        validatePhone = validatePhoneBS . T.encodeUtf8
        validatePhoneBS = (isRight .) . PC.parseOnly $ do
          PC.string "+65 "
          PC.count 4 PC.digit
          PC.char ' '
          PC.count 4 PC.digit
          PC.endOfInput

studentSubmitForm :: StudentId -> FormInput Handler (StudentId, Nric, StudentSubmission)
studentSubmitForm sid = unMFormInput $ do
  nric <- mireq nricField "nric"
  phone <- mireq sgPhoneField "phone"
  email <- mireq emailField "email"
  cca1 <- miopt ccaIdField "cca1"
  cca2 <- miopt ccaIdField "cca2"
  cca3 <- miopt ccaIdField "cca3"
  let cca = catMaybes [cca1, cca2, cca3]
  hasError <- mireq checkBoxField "haserror"
  sig <- mireq pngField "sig"
  today <- mireq todayField "today"
  ua <- mireq textField "ua"
  return $ (sid, nric, SubmissionCompleted phone email cca hasError (Just sig) Nothing today ua)

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
  where bw = toPathPiece
        fw = return . parseClass . T.encodeUtf8

parseClass :: C.ByteString -> Either T.Text Class
parseClass = ((note "wrong class format" . hush) .) . PC.parseOnly $ do
  l <- Char.digitToInt <$> PC.digit
  guard $ 1 <= l && l <= 6
  c <- PC.satisfy $ PC.inClass "A-NP-Z"
  PC.endOfInput
  return $ Class (l, c)

instance PathPiece Class where
  toPathPiece (Class (l, c)) = T.pack $ show l ++ [c]
  fromPathPiece = hush . parseClass . T.encodeUtf8

nricField :: Field Handler Nric
nricField = checkMMap fw bw textField
  where bw (Nric s) = s
        fw = return . parseNric . T.encodeUtf8

parseNric :: C.ByteString -> Either T.Text Nric
parseNric = ((note "wrong nric format" . hush) . ) . PC.parseOnly $ do
  many (PC.char 'X')
  prefix <- PC.option Nothing (Just <$> PC.satisfy (PC.inClass "SFTG"))
  digits <- PC.many1 PC.digit
  guard $ length digits >= 4 && length digits <= 7
  suffix <- case (prefix, digits) of
    (Just c, [d1,d2,d3,d4,d5,d6,d7]) -> do
      let prefixNum = if c `C.elem` "SF" then 0 else 4
      let check = (`rem` 11) . (+ prefixNum) . sum . zipWith (*) [2,7,6,5,4,3,2] . map Char.digitToInt $ digits
      PC.char $ "JZIHGFEDCBAXWUTRQPNMLK" !! (prefixNum `div` 4 * 11 + check)
    _ -> PC.satisfy $ PC.inClass "JZIHGFEDCBAXWUTRQPNMLK"
  PC.endOfInput
  return . Nric . T.pack $ case prefix of
   Nothing -> digits ++ [suffix]
   Just p  -> p : digits ++ [suffix]

todayField :: Field Handler Day
todayField = checkMMap fw bw checkBoxField
  where bw _ = False
        fw _ = do
          today <- liftIO $ utctDay <$> getCurrentTime
          checkMMapOk today

pngField :: Field Handler ByteString64
pngField = checkMMap fw bw textField
  where bw = undefined -- XXX
        fw = return . parsePngData . T.encodeUtf8

parsePngData :: C.ByteString -> Either T.Text ByteString64
parsePngData bs = note "invalid image data" . hush $ do
  data64 <- PC.parseOnly (PC.string "data:image/png;base64," *> PC.takeByteString) bs
  d <- C64.decode data64
  Png.decodePng d
  return $ ByteString64 d

ccaIdField :: Field Handler CcaId
ccaIdField = checkMMap fw bw intField
  where bw (CcaId i) = i
        fw i = do
          acid <- getAcid <$> ask
          let rv = CcaId i
          e <- liftIO $ Acid.query acid $ LookupCcaById (CcaId i)
          case e of
           Nothing -> return . Left $ ("no such cca" :: T.Text)
           Just _ -> return . Right $ rv

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
          subjects <- liftIO $ Acid.query acid $ ListSubjectsByLevel level
          let noncompulsorySubjects = filter (isJust . (^. subjectCode)) . Set.toAscList $ subjects
          optionsPairs . map (liftM2 (,) (T.pack . show . unSubjectId) id . (^. subjectId)) $ noncompulsorySubjects
        unSubjectId (SubjectId i) = i

-- | Generically parses forms and handles a database update.
acidFormUpdateHandler :: (Acid.UpdateEvent ev,
                          ToJSON r, ToJSON e,
                          Acid.MethodResult ev ~ Either e r,
                          Acid.MethodState ev ~ Database) =>
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

getClassesR :: Handler Value
getClassesR = acidQueryHandler PublicListClasses

getClassR :: Class -> Handler Value
getClassR = acidQueryHandler . PublicListStudentsFromClass

getPublicStudentR :: Class -> Int -> Handler Value
getPublicStudentR klass index = do
  nric <- runInputGet (ireq nricField "nric")
  acidQueryHandler $ PublicLookupStudentByClassIndexNumber klass index nric

postStudentSubmitR :: StudentId -> Handler Value
postStudentSubmitR = acidFormUpdateHandler studentSubmitForm (const PublicStudentDoSubmission)

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
           "name" -> do
             name <- runInputGet (ireq textField "name")
             checkMMapOk . QueryEvent crit $ SearchStudentsByName name
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

postUnlockSubmissionR :: StudentId -> Handler Value
postUnlockSubmissionR = acidUpdateHandler . TeacherUnlockSubmission

postLockSubmissionR :: StudentId -> Handler Value
postLockSubmissionR = acidUpdateHandler . TeacherLockSubmission

postManyStudentsR :: Handler Value
postManyStudentsR = do
  acid <- getAcid <$> ask
  fileinfo <- runInputPost (ireq fileField "csv")
  force <- runInputPost (ireq checkBoxField "force")
  bs <- fileSource fileinfo $$ sinkLbs
  result <- runEitherT $ do
    maybeText <- liftIO . tryDecodeAllEncodings . CL.toStrict $ bs
    csvText <- hoistEither $ note errCSVTextDecodeFailed maybeText
    csvData <- hoistEither $ parseCSV csvText
    allSubjects <- liftIO $ mapM (fmap subjectsToMap . Acid.query acid . ListSubjectsByLevel) [1..6]
    allTeachers <- liftIO $ teachersToMap <$> Acid.query acid ListTeachers
    V.forM csvData $ \(rowNumber', rawStudent@CsvStudent{..}) -> do
      let rowNumber = succ rowNumber'
      -- attempt to convert rawStudent to a Student
      let name = T.toTitle _name
      klass@(Class (level, _)) <- hoistEither . parseClass' rowNumber . T.encodeUtf8 $ _klass
      indexNo <- hoistEither . parseIndex rowNumber . T.encodeUtf8 $ _indexNo
      nric <- hoistEither . parseNric' rowNumber . T.encodeUtf8 $ _nric
      witness <- hoistEither $ parseWitnessName rowNumber allTeachers _witness
      subjectIds <- case _subjCombi `elem` emptyFieldDesig of
        True -> return Set.empty
        False -> do
          let possibilities = parseSubjectCodeFriendly (allSubjects !! (level-1)) _subjCombi
          case possibilities of
           ParseSuccess s -> return $ Set.mapMonotonic (^. idField) s
           ParseAmbiguous ss -> do
             let (i1:i2:_) = map (T.intercalate ", " . map (^. subjectName) . Set.toList) ss
             hoistEither . Left $ errCSVSubjectCodeAmbiguous rowNumber _subjCombi i1 i2
           ParseIncomplete (rem, ps) -> do
             let psf = T.intercalate ", " . map (^. subjectName) . Set.toList $ ps
             hoistEither . Left $ errCSVSubjectCodeIncomplete rowNumber _subjCombi rem psf
           ParseNothing _ -> hoistEither . Left $ errCSVSubjectCodeNothing rowNumber _subjCombi
           ParseInternalError -> hoistEither . Left $ errCSVSubjectCodeInternalError rowNumber _subjCombi
      return $ Student (StudentId 0) name _chinese witness klass indexNo subjectIds nric SubmissionNotOpen
  case result of
   Left e -> invalidArgs [TL.toStrict e]
   Right vs -> acidUpdateHandler $ AddStudents force vs
  where parseClass' row bs = note (errCSVClassNoParse row (T.decodeUtf8 bs)) $ hush $ parseClass bs
        parseNric' row bs = note (errCSVNricNoParse row (T.decodeUtf8 bs)) $ hush $ parseNric bs
        parseIndex :: Int -> C.ByteString -> Either TL.Text Int
        parseIndex row bs = note (errCSVIndexNumNoParse row (T.decodeUtf8 bs)) $ hush $ PC.parseOnly (PC.decimal <* PC.endOfInput) bs
        parseWitnessName :: Int -> Map T.Text Teacher -> T.Text -> Either TL.Text (Maybe TeacherId)
        parseWitnessName row teacherMap s =
          case Map.lookup s teacherMap of
           Just t -> return . Just $ t ^. idField
           Nothing -> case s `elem` emptyFieldDesig of
             True -> return Nothing
             False -> Left $ errCSVWitnessNoParse row s
        emptyFieldDesig = [ "", "-", "--", "---", "\8210", "\8211", "\8212", "\65112" ]


-- |
-- = HTML handlers

getAuthStatusR :: Handler Html
getAuthStatusR = do -- will be removed
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            $maybe e <- maid
                <p>You are logged in as #{e}.
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>You are not logged in.
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

adminSite :: Widget
adminSite = do
  addStylesheet $ StaticR bootstrap_min_css
  addStylesheet $ StaticR bootstrapt_min_css
  addScript $ StaticR jquery_js
  addScript $ StaticR underscore_js
  addScript $ StaticR react_dev_js
  addScript $ StaticR bootstrap_js
  toWidget $(juliusFileAuto "templates/admin.js")

getAdminLogoutR :: Handler Html
getAdminLogoutR = defaultLayout $ do
  setTitle "RVHS Science Lab Undertaking :: Admin Console :: Logout Successful"
  addStylesheet $ StaticR bootstrap_min_css
  addStylesheet $ StaticR bootstrapt_min_css
  addScript $ StaticR jquery_js
  addScript $ StaticR bootstrap_js
  toWidget $(hamletFileAuto "templates/didlogout.hamlet")

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
  toWidget $(cassiusFileAuto "templates/hover.cassius")
  adminSite

-- | The user-facing frontend.
getHomepageR :: Handler Html
getHomepageR = defaultLayout $ do
  setTitle "River Valley High School Science Lab Declaration"
  toWidgetHead $(hamletFileAuto "templates/app.head.hamlet")
  addScript $ StaticR jquery_js
  addScript $ StaticR jquery_mobile_custom_js
  addScript $ StaticR underscore_js
  toWidget $(juliusFileAuto "templates/app.julius")
  toWidget $(hamletFileAuto "templates/app.hamlet")
  toWidget $(cassiusFileAuto "templates/app.cassius")
