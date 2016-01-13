{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LabDecl.Handlers where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader (ask)
import Control.Arrow (second)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async.Lifted
import Control.Error
import Control.Lens ((^.))
import Data.Default
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Vector (Vector)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Aeson as JSON
import qualified Data.Acid as Acid
import qualified Data.Acid.Advanced as Acid
import Data.IxSet (Proxy(..))
import Data.Conduit (($$))
import Data.Conduit.Binary (sinkLbs)
import Network.Wai.Conduit (sourceRequestBody)
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import Network.Wai.Parse (lbsBackEnd)
import Yesod.Core
import Yesod.Form hiding (emailField)
import Yesod.EmbeddedStatic
import Yesod.Auth
import Yesod.Auth.GoogleEmail2 hiding (Email)
import Text.Hamlet (hamletFile, hamletFileReload)
import Text.Julius (juliusFileReload)
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import LabDecl.Utilities
import LabDecl.Types
import LabDecl.AcidicModels
import LabDecl.AcidicModelInstances
import LabDecl.EntityCsv
import LabDecl.Models
import LabDecl.ErrMsg
import LabDecl.FieldParsers
import LabDecl.FormFields
import LabDecl.SubjectCodes
import LabDecl.PushNotifications
import LabDecl.AsyncQueue
import LabDecl.PDFServices

operatorEmail :: T.Text
operatorEmail = "qzy@qzy.io"

cspViolationReportTo :: T.Text
cspViolationReportTo = "CSP Violation Investigator <labdecl-csp@qzy.io>"

-- | The foundation data type.
data LabDeclarationApp = LabDeclarationApp {
  getStatic :: EmbeddedStatic,
  getAcid :: Acid.AcidState Database,
  getNotifyChan :: TChan (),
  getHttpManager :: HTTP.Manager,
  getPDFService :: AsyncInput -> IO (),
  getAsyncQueue :: TQueue InternalQueueTask,
  getCanBeShutdown :: TVar Bool,
  getShutdownSignal :: TMVar Bool,
  getGoogleCredentials :: (T.Text, T.Text),
  getApproot :: T.Text,
  isDevelopment :: Bool
  }

$(mkEmbeddedStatic False "eStatic" [embedDir "static"])

-- | The routing. Routes of the form /api/items allow GET and POST,
-- which are list* and add* respectively. Routes of the form
-- /api/items/#Int allow GET PUT DELETE, which are lookup*ById,
-- replace* and remove* respectively. GET requests of /api/items allow
-- searching through query args. Most of them just does simple
-- marshalling and then invoke the events in LabDecl.AcidicModels.
-- The exclamation mark means overlap checking is disabled.
$(mkYesod "LabDeclarationApp" [parseRoutes|
/api/ccas                 CcasR          GET POST DELETE
/api/ccas/csv             ManyCcasR      POST
!/api/ccas/#CcaId         CcaR           GET PUT DELETE
/api/subjects             SubjectsR      GET POST DELETE
/api/subjects/csv         ManySubjectsR  POST
/api/subjects/test-decode TestDecodeR    GET
!/api/subjects/#SubjectId SubjectR       GET PUT DELETE
/api/teachers             TeachersR      GET POST DELETE
/api/teachers/csv         ManyTeachersR  POST
!/api/teachers/#TeacherId TeacherR       GET PUT DELETE
/api/classes              ClassesR       GET
/api/classes/#Class       ClassR         GET
/api/classes/#Class/#Int  PublicStudentR GET
/api/students             StudentsR      GET POST DELETE
/api/students/csv         ManyStudentsR  POST
!/api/students/#StudentId StudentR       GET PUT DELETE
!/api/students/many/lock   LockManySubmissionsR POST
!/api/students/many/unlock UnlockManySubmissionsR POST
/api/students/#StudentId/submit      StudentSubmitR POST
/api/students/#StudentId/unlock      UnlockSubmissionR POST
/api/students/#StudentId/lock        LockSubmissionR POST
/api/decltext/test        TestDeclTextR  POST
/api/decltext             DeclTextR      GET POST
/api/canShutdown          CanShutdownR   GET
/api/shutdown             ShutdownR      POST
/admin                    AdminHomeR     GET
/admin/ccas               AdminCcasR     GET
/admin/subjects           AdminSubjectsR GET
/admin/teachers           AdminTeachersR GET
/admin/students           AdminStudentsR GET
/admin/logout             AdminLogoutR   GET
/                         HomepageR      GET
/robots.txt               RobotsR        GET
/cspreport                CSPReportR     POST
/static                   StaticR        EmbeddedStatic getStatic
/auth                     AuthR          Auth getAuth
|])

instance Yesod LabDeclarationApp where

  -- | The app root.
  approot = ApprootMaster getApproot

  -- | The privilege table.
  isAuthorized CcasR                 w = requirePrivilege (if w then PrivAdmin else PrivNone)
  isAuthorized ManyCcasR             _ = requirePrivilege PrivAdmin
  isAuthorized (CcaR _)              w = requirePrivilege (if w then PrivAdmin else PrivNone)
  isAuthorized SubjectsR             w = requirePrivilege (if w then PrivAdmin else PrivNone)
  isAuthorized ManySubjectsR         _ = requirePrivilege PrivAdmin
  isAuthorized TestDecodeR           _ = requirePrivilege PrivTeacher
  isAuthorized (SubjectR _)          w = requirePrivilege (if w then PrivAdmin else PrivNone)
  isAuthorized TeachersR             w = requirePrivilege (if w then PrivAdmin else PrivTeacher)
  isAuthorized ManyTeachersR         _ = requirePrivilege PrivAdmin
  isAuthorized (TeacherR _)          w = requirePrivilege (if w then PrivAdmin else PrivTeacher)
  isAuthorized ClassesR              _ = requirePrivilege PrivNone
  isAuthorized (ClassR _)            _ = requirePrivilege PrivNone
  isAuthorized (PublicStudentR _ _)  _ = requirePrivilege PrivNone
  isAuthorized StudentsR             w = requirePrivilege (if w then PrivAdmin else PrivTeacher)
  isAuthorized ManyStudentsR         _ = requirePrivilege PrivAdmin
  isAuthorized (StudentR _)          w = requirePrivilege (if w then PrivAdmin else PrivTeacher)
  isAuthorized LockManySubmissionsR  _ = requirePrivilege PrivTeacher
  isAuthorized UnlockManySubmissionsR  _ = requirePrivilege PrivTeacher
  isAuthorized (StudentSubmitR _)    _ = requirePrivilege PrivNone
  isAuthorized (UnlockSubmissionR _) _ = requirePrivilege PrivTeacher
  isAuthorized (LockSubmissionR _)   _ = requirePrivilege PrivTeacher
  isAuthorized TestDeclTextR         _ = requirePrivilege PrivAdmin
  isAuthorized DeclTextR             w = requirePrivilege (if w then PrivAdmin else PrivNone)
  isAuthorized CanShutdownR          _ = requirePrivilege PrivOperator
  isAuthorized ShutdownR             _ = requirePrivilege PrivOperator
  isAuthorized AdminHomeR            _ = requirePrivilege PrivTeacher
  isAuthorized AdminCcasR            _ = requirePrivilege PrivTeacher
  isAuthorized AdminSubjectsR        _ = requirePrivilege PrivTeacher
  isAuthorized AdminTeachersR        _ = requirePrivilege PrivTeacher
  isAuthorized AdminStudentsR        _ = requirePrivilege PrivTeacher
  isAuthorized AdminLogoutR          _ = requirePrivilege PrivNone
  isAuthorized HomepageR             _ = requirePrivilege PrivNone
  isAuthorized RobotsR               _ = requirePrivilege PrivNone
  isAuthorized CSPReportR            _ = requirePrivilege PrivNone
  isAuthorized (StaticR _)           _ = requirePrivilege PrivNone
  isAuthorized (AuthR _)             _ = requirePrivilege PrivNone

  -- | Static files.
  addStaticContent = embedStaticContent getStatic StaticR Right

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
  authPlugins = (:[]) . uncurry authGoogleEmail <$> getGoogleCredentials
  loginDest _ = AdminHomeR
  logoutDest _ = AdminLogoutR
  getAuthId = return . Just . credsIdent
  maybeAuthId = lookupSession "_ID"

instance RenderMessage LabDeclarationApp FormMessage where
  renderMessage _ _ = defaultFormMessage

data Privilege = PrivNone
               | PrivTeacher
               | PrivAdmin
               | PrivOperator
               deriving (Show, Read, Eq, Ord)

requirePrivilege :: Privilege -> Handler AuthResult
requirePrivilege privReq
  | privReq == PrivNone = return Authorized
  | otherwise = do
      dev <- isDevelopment <$> ask
      if dev
        then do
        setSession "user" operatorEmail
        setSession "priv" (T.pack (show PrivOperator))
        return Authorized
        else maybe AuthenticationRequired
             (\(u, p) -> if p >= privReq
                         then Authorized
                         else Unauthorized $ T.concat ["Insufficient privileges. Your account ", u, " is not allowed to access this."])
             <$> getPrivilege

getPrivilege :: Handler (Maybe (T.Text, Privilege))
getPrivilege = do
  mbUserPriv <- (liftM2.liftM2) (,) (lookupSession "user") (lookupSession "priv")
  case mbUserPriv of
   Just v -> return . Just $ second (read . T.unpack) v
   Nothing -> do
     mbAid <- maybeAuthId
     case mbAid of
      Nothing -> return Nothing
      Just aid -> do
        let returnOk priv = do
              setSession "user" aid
              setSession "priv" (T.pack (show priv))
              return $ Just (aid, priv)
        if aid == operatorEmail
          then returnOk PrivOperator
          else do
          acid <- getAcid <$> ask
          mbIdentity <- liftIO $ Acid.query acid $ LookupTeacherByEmail (Email aid)
          case mbIdentity of
           Nothing -> returnOk PrivNone
           Just identity -> do
             let priv = bool PrivTeacher PrivAdmin (identity ^. teacherIsAdmin)
             returnOk priv

-- | Generically handles a database query. Sends either a normal JSON
-- response, or establishes an SSE event stream or WebSocket connection for push
-- notifications.
acidQueryHandler :: (ToHTTPStatus (Acid.MethodResult ev),
                     Acid.QueryEvent ev,
                     ToJSON (Acid.MethodResult ev),
                     Acid.MethodState ev ~ Database) => ev -> Handler TypedContent
acidQueryHandler event = do
  addHeader "Cache-Control" "no-store, no-cache, must-revalidate"
  acid <- getAcid <$> ask
  let genResponse = do
        result <- liftIO $ Acid.query acid event
        let httpStatus = toHttpStatus result
        let jsonResponse = object [ "meta" .= object [ "code" .= HTTP.statusCode httpStatus ], "data" .= result ]
        return (httpStatus, jsonResponse)

  notifyChan <- getNotifyChan <$> ask
  theChan <- liftIO . atomically . dupTChan $ notifyChan

  -- Here we start a new thread that listens to `theChan`, performs the
  -- request, compares the request, and possibly sends the request to a new
  -- TQueue.
  resultTQueue <- liftIO . atomically $ newTQueue
  responseTVar <- liftIO . atomically $ newTVar JSON.Null
  (_, firstResponse) <- genResponse
  liftIO . atomically . writeTVar responseTVar $ firstResponse
  liftIO . atomically . writeTQueue resultTQueue . JSON.encode $ firstResponse

  -- Investigation shows using async here will cause a resource leak i.e. the
  -- thread is never killed, leading to accumulation of items in the TQueue, and
  -- wasting time generating responses, which is why we need another watchDog
  -- thread below. If the queue is not empty for 6 consecutive times, at
  -- intervals of 10 seconds, it is considered that the thread responsible for
  -- sending responses has died due to connection being closed. However, another
  -- minor problem surfaces, in which a thread that never writes anything is
  -- also not terminated. This is therefore solved by sending something to the
  -- TQueue every 60 seconds regardless of whether there is such a need.

  sendOnChange <- async . forever $ do
    liftIO . atomically $ readTChan theChan
    (_, response) <- genResponse
    liftIO . atomically $ do
      prevResponse <- readTVar responseTVar
      when (response /= prevResponse) $ do
        writeTVar responseTVar response
        writeTQueue resultTQueue . JSON.encode $ response
  sendOnTimer <- async . liftIO . forever $ do
    threadDelay 60000000
    atomically $ do
      prevResponse <- readTVar responseTVar
      writeTQueue resultTQueue . JSON.encode $ prevResponse

  -- We need a watchdog thread here to cancel the thread when the queue is not
  -- empty for 6 consecutive times, at intervals of 10 seconds.
  let watchDog previousNotEmptyTimes = do
        liftIO $ threadDelay 10000000
        isEmptyThisTime <- liftIO . atomically . isEmptyTQueue $ resultTQueue
        let notEmptyTimes = if isEmptyThisTime then 0 else 1 + previousNotEmptyTimes
        if notEmptyTimes == 6
          then do
          cancel sendOnTimer
          cancel sendOnChange
          else watchDog notEmptyTimes

  async $ watchDog (0 :: Int)

  let pushConnTimeouts = TimeoutSpec { retryTimeoutMilliseconds = 3000, keepAliveTimeoutMicroseconds = 10000000}
  beginSSE pushConnTimeouts resultTQueue . beginWS pushConnTimeouts resultTQueue $ do
    (httpStatus, jsonResponse) <- genResponse
    if HTTP.statusIsSuccessful httpStatus
      then return (toTypedContent jsonResponse)
      else sendResponseStatus httpStatus (toTypedContent jsonResponse)


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

teacherForm :: TeacherId -> FormInput Handler Teacher
teacherForm tid = Teacher
                  <$> pure tid
                  <*> ireq checkBoxField "admin"
                  <*> ireq textField "unit"
                  <*> ireq textField "name"
                  <*> ireq textField "witness"
                  <*> ireq emailField "email"


studentSubmitForm :: StudentId -> FormInput Handler (StudentId, Nric, StudentSubmission)
studentSubmitForm sid = unMFormInput $ do
  nric <- mireq nricField "nric"
  phone <- mireq sgPhoneField "phone"
  email <- mireq emailField "email"
  cca1 <- miopt ccaIdField "cca1"
  cca2 <- miopt ccaIdField "cca2"
  cca3 <- miopt ccaIdField "cca3"
  let cca = Set.fromList $ catMaybes [cca1, cca2, cca3]
  hasError <- mireq checkBoxField "haserror"
  today <- mireq todayField "today"
  ua <- mireq textField "ua"
  return (sid, nric, SubmissionCompleted phone email cca hasError Nothing today ua)

studentSubmitPngForm :: FormInput Handler CL.ByteString
studentSubmitPngForm = ireq pngField "sig"

studentForm :: StudentId -> FormInput Handler Student
studentForm sid = unMFormInput $ do
                  a <- mireq textField "name"
                  b <- mireq textField "chinesename"
                  c <- miopt teacherIdField "witness"
                  d <- mireq classField "class"
                  e <- mireq intField "indexno"
                  f <- miopt (subjCombiField d) "subj"
                  let f' = fromMaybe Set.empty f
                  g <- mireq nricField "nric"
                  return $ Student sid a b c d e f' g SubmissionNotOpen

instance PathPiece Class where
  toPathPiece (Class (l, c)) = T.pack $ show l ++ [c]
  fromPathPiece = hush . parseClass . T.encodeUtf8

existingIdField :: (HasCRUDEvents a i le re ae ase, RenderMessage site FormMessage,
                    Acid.QueryEvent le,
                    Acid.MethodResult le ~ Maybe a) =>
                   (site -> Acid.AcidState (Acid.MethodState le)) -> T.Text -> Field (HandlerT site IO) i
existingIdField getAcid errMsg = checkMMap fw bw intField
  where bw = idDestructor
        fw i = do
          let rv = idConstructor i
          acid <- getAcid <$> ask
          e <- liftIO $ Acid.query acid $ lookupByIdEvent rv
          case e of
           Nothing -> return . Left $ errMsg
           Just _ -> return . Right $ rv

ccaIdField :: Field Handler CcaId
ccaIdField = existingIdField getAcid "no such cca"

teacherIdField :: Field Handler TeacherId
teacherIdField = existingIdField getAcid "no such teacher"

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
                         (Bool -> a -> ev) ->
                         FormInput Handler a ->
                         Handler Value
acidFormUpdateHandler eventCon form = do
  entity <- runInputPost form
  force <- runInputPost (ireq checkBoxField "force")
  acidUpdateHandler $ eventCon force entity

-- | Enumeration handlers. The one for student is specially handled.
getCcasR     :: Handler TypedContent
getSubjectsR :: Handler TypedContent
getTeachersR :: Handler TypedContent
getCcasR     = acidQueryHandler ListCcas
getSubjectsR = acidQueryHandler ListSubjects
getTeachersR = acidQueryHandler ListTeachers

-- | Add-thing handlers. Too lazy to make it generic and write HasForm class and instances.
postCcasR     :: Handler Value
postSubjectsR :: Handler Value
postTeachersR :: Handler Value
postStudentsR :: Handler Value
postCcasR     = acidFormUpdateHandler AddCca     $ ccaForm (CcaId 0)
postSubjectsR = acidFormUpdateHandler AddSubject $ subjectForm (SubjectId 0)
postTeachersR = acidFormUpdateHandler AddTeacher $ teacherForm (TeacherId 0)
postStudentsR = acidFormUpdateHandler AddStudent $ studentForm (StudentId 0)

-- | Get information about a single thing.
getOneHandler :: forall a i le re ae ase.
                 (HasCRUDEvents a i le re ae ase,
                  Acid.QueryEvent le,
                  Acid.MethodResult le ~ Maybe a,
                  Acid.MethodState le ~ Database
                 ) => Proxy a -> i -> Handler TypedContent
getOneHandler _ = acidQueryHandler . lookupByIdEvent'
  where lookupByIdEvent' = lookupByIdEvent :: i -> le

getCcaR     :: CcaId     -> Handler TypedContent
getSubjectR :: SubjectId -> Handler TypedContent
getTeacherR :: TeacherId -> Handler TypedContent
getStudentR :: StudentId -> Handler TypedContent
getCcaR     = getOneHandler (Proxy :: Proxy Cca)
getSubjectR = getOneHandler (Proxy :: Proxy Subject)
getTeacherR = getOneHandler (Proxy :: Proxy Teacher)
getStudentR = getOneHandler (Proxy :: Proxy Student)

-- | Edit CCA. Requires admin.
putCcaR :: CcaId -> Handler Value
putCcaR = acidFormUpdateHandler ReplaceCca . ccaForm

-- | Delete CCA. Requires Admin.
deleteCcaR :: CcaId -> Handler Value
deleteCcaR = acidUpdateHandler . RemoveCca

deleteCcasR :: Handler Value
deleteCcasR = acidFormUpdateHandler (const RemoveCcas) (iopt rawIdsField "ids")

putSubjectR :: SubjectId -> Handler Value
putSubjectR = acidFormUpdateHandler ReplaceSubject . subjectForm

deleteSubjectR :: SubjectId -> Handler Value
deleteSubjectR = acidUpdateHandler . RemoveSubject

deleteSubjectsR :: Handler Value
deleteSubjectsR = acidFormUpdateHandler (const RemoveSubjects) (iopt rawIdsField "ids")

getTestDecodeR :: Handler Value
getTestDecodeR = do -- TODO use parseSubjectCodeFriendly
  (level, str) <- runInputGet $ (,) <$> ireq oneLevelField "level" <*> ireq textField "str"
  acid <- getAcid <$> ask
  allSubjects <- liftIO $ Acid.query acid $ ListSubjectsByLevel level
  let parsed = Set.toList <$> parseSubjectCode (subjectsToMap allSubjects) str
  return $ object [ "meta" .= object ["code" .=  (200 :: Int) ], "data" .= parsed ]
  where oneLevelField = radioFieldList $ map (liftM2 (,) (T.pack . show) id) [1..6]

putTeacherR :: TeacherId -> Handler Value
putTeacherR = acidFormUpdateHandler ReplaceTeacher . teacherForm

deleteTeacherR :: TeacherId -> Handler Value
deleteTeacherR = acidUpdateHandler . RemoveTeacher

deleteTeachersR :: Handler Value
deleteTeachersR = acidFormUpdateHandler (const RemoveTeachers) (iopt rawIdsField "ids")

getClassesR :: Handler TypedContent
getClassesR = acidQueryHandler PublicListClasses

getClassR :: Class -> Handler TypedContent
getClassR = acidQueryHandler . PublicListStudentsFromClass

getPublicStudentR :: Class -> Int -> Handler TypedContent
getPublicStudentR klass index = do
  nric <- runInputGet (ireq nricField "nric")
  acidQueryHandler $ PublicLookupStudentByClassIndexNumber klass index nric

-- This is so wrong. We need to write the thing to the queue before actually
-- handling the request.
postStudentSubmitR :: StudentId -> Handler Value
postStudentSubmitR sid = do
  pngData <- runInputPost studentSubmitPngForm
  rv <- acidFormUpdateHandler (const PublicStudentDoSubmission) $ studentSubmitForm sid
  acid <- getAcid <$> ask
  Just student <- liftIO $ Acid.query acid $ LookupStudentById sid
  allSubjects <- liftIO $ Acid.query acid $ ListSubjectsByLevel $ let (Class (l, _)) = student ^. studentClass in l
  let subjects = (`Set.filter` allSubjects) $
                 \s -> (s ^. subjectIsScience) &&
                       (isNothing (s ^. subjectCode) ||
                        (s ^. subjectId) `Set.member` (student ^. studentSubjectCombi))
  mbWitness <- maybe (return Nothing) (liftIO . Acid.query acid . LookupTeacherById) (student ^. studentWitnesser)
  pdfService <- getPDFService <$> ask
  liftIO . pdfService $ (student, mbWitness, subjects, pngData)
  return rv

data QueryEvent = forall ev. (ToHTTPStatus (Acid.MethodResult ev),
                              Acid.QueryEvent ev,
                              ToJSON (Acid.MethodResult ev),
                              Acid.MethodState ev ~ Database) => QueryEvent T.Text ev

searchByField :: Field Handler QueryEvent
searchByField = checkMMap fw bw textField
  where bw :: QueryEvent -> T.Text
        bw (QueryEvent tag _) = tag
        fw crit =
          case crit of
           "none" -> checkMMapOk . QueryEvent crit $ ListNothing
           "all" -> checkMMapOk . QueryEvent crit $ ListStudents
           "class" -> do
             klass <- runInputGet (ireq classField "class")
             checkMMapOk . QueryEvent crit $ ListStudentsFromClass klass
           "level" -> do
             level <- runInputGet (ireq intField "level")
             checkMMapOk . QueryEvent crit $ ListStudentsByLevel level
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

getStudentsR :: Handler TypedContent
getStudentsR = do
  QueryEvent _ ev <- runInputGet (ireq searchByField "searchby")
  acidQueryHandler ev

putStudentR :: StudentId -> Handler Value
putStudentR = acidFormUpdateHandler ReplaceStudent . studentForm

deleteStudentR :: StudentId -> Handler Value
deleteStudentR = acidUpdateHandler . RemoveStudent

deleteStudentsR :: Handler Value
deleteStudentsR = acidFormUpdateHandler (const RemoveStudents) (iopt rawIdsField "ids")

postUnlockSubmissionR :: StudentId -> Handler Value
postUnlockSubmissionR = acidUpdateHandler . TeacherChangeSubmissionStatus SubmissionOpen

postLockSubmissionR :: StudentId -> Handler Value
postLockSubmissionR = acidUpdateHandler . TeacherChangeSubmissionStatus SubmissionNotOpen

postLockManySubmissionsR :: Handler Value
postLockManySubmissionsR = acidFormUpdateHandler (const (TeacherChangeManySubmissionStatus SubmissionNotOpen)) (ireq rawIdsField "ids")

postUnlockManySubmissionsR :: Handler Value
postUnlockManySubmissionsR = acidFormUpdateHandler (const (TeacherChangeManySubmissionStatus SubmissionOpen)) (ireq rawIdsField "ids")


postManyHandler :: forall a i le re ae ase.
                   (HasCsvProcessor a i le re ae ase,
                    Acid.UpdateEvent ase,
                    Acid.MethodResult ase ~ Either TL.Text (),
                    Acid.MethodState ase ~ Database
                   ) => Proxy a -> Handler Value
postManyHandler _ = do
  acid <- getAcid <$> ask
  fileinfo <- runInputPost (ireq fileField "csv")
  force <- runInputPost (ireq checkBoxField "force")
  bs <- fileSource fileinfo $$ sinkLbs
  result <- runExceptT $ do
    maybeText <- liftIO . tryDecodeAllEncodings . CL.toStrict $ bs
    csvText <- hoistEither $ note errCSVTextDecodeFailed maybeText
    csvProcessor' acid csvText
  case result of
   Left e -> invalidArgs [TL.toStrict e]
   Right vs -> acidUpdateHandler' $ addEntitiesEvent' force vs
  where csvProcessor' = csvProcessor :: Acid.AcidState Database -> T.Text -> ExceptT TL.Text Handler (Vector a)
        addEntitiesEvent' = addEntitiesEvent :: Bool -> Vector a -> ase
        acidUpdateHandler' = acidUpdateHandler :: (Acid.UpdateEvent ase, Acid.MethodResult ase ~ Either TL.Text (), Acid.MethodState ase ~ Database) => ase -> Handler Value

postManyStudentsR :: Handler Value
postManySubjectsR :: Handler Value
postManyTeachersR :: Handler Value
postManyCcasR     :: Handler Value
postManyStudentsR = postManyHandler (Proxy :: Proxy Student)
postManySubjectsR = postManyHandler (Proxy :: Proxy Subject)
postManyTeachersR = postManyHandler (Proxy :: Proxy Teacher)
postManyCcasR     = postManyHandler (Proxy :: Proxy Cca)

getCanShutdownR :: Handler Value
getCanShutdownR = do
  canBeShutDown <- getCanBeShutdown <$> ask
  asyncQueue <- getAsyncQueue <$> ask
  r <- liftIO . atomically $ do
    isQueueEmpty <- isEmptyTQueue asyncQueue
    cbsd <- readTVar canBeShutDown
    return $ cbsd && isQueueEmpty
  return $ object [ "meta" .= object ["code" .= (200 :: Int)], "data" .= r ]

postShutdownR :: Handler TypedContent
postShutdownR = do
  shutdownSignal <- getShutdownSignal <$> ask
  liftIO . atomically $ putTMVar shutdownSignal False
  sendResponseStatus HTTP.status204 T.empty

-- | Declaration text handlers
postTestDeclTextR :: Handler Value
postTestDeclTextR = do
  md <- runInputPost (ireq textField "markdown")
  case writeHtmlString def <$> readMarkdown def (T.unpack md) of
    Left _ -> return $ object [ "meta" .= object [ "code" .= (400 :: Int) ], "data" .= JSON.Null ]
    Right r -> return $ object [ "meta" .= object [ "code" .= (200 :: Int) ], "data" .= r ]

getDeclTextR :: Handler TypedContent
getDeclTextR = acidQueryHandler GetDeclarationText

postDeclTextR :: Handler Value
postDeclTextR = do
  md <- runInputPost (ireq textField "markdown")
  case readMarkdown def (T.unpack md) of
    Left _ -> return $ object [ "meta" .= object [ "code" .= (400 :: Int) ], "data" .= JSON.Null ]
    Right _ -> acidUpdateHandler (SetDeclarationText md)

postCSPReportR :: Handler ()
postCSPReportR = do
  req <- waiRequest
  body <- sourceRequestBody req $$ sinkLbs
  case (JSON.decode body :: Maybe Value) of
    Nothing -> sendResponseStatus HTTP.status400 ()
    Just _ -> do
      asyncQueue <- getAsyncQueue <$> ask
      dev <- isDevelopment <$> ask
      manager <- getHttpManager <$> ask
      let reportMail = GAEMail {
            mailSender = "River Valley High School Science Department <rvhs.science.oracle@gmail.com>",
            mailTo = cspViolationReportTo,
            mailSubject = "CSP Violation Report",
            mailBody = TL.decodeUtf8 body,
            mailAttachments = []
            }
      liftIO . atomically . writeTQueue asyncQueue $ def {
        taskName = "Send CSP Violation Report to Operator",
        task = sendMail dev manager reportMail
        }
      sendResponseStatus HTTP.status204 ()

-- |
-- = HTML handlers

getRobotsR :: Handler T.Text
getRobotsR = return "User-agent: *\nDisallow: /\n"

baseSite :: Bool -> Widget
baseSite dev = do
  addHeader "X-XSS-Protection" "1; mode=block"
  addHeader "X-Frame-Options" "SAMEORIGIN"
  addHeader "Content-Security-Policy" "default-src 'none'; script-src 'self'; connect-src 'self'; img-src 'self'; style-src 'self' 'unsafe-inline'; font-src 'self'; report-uri /cspreport"
  toWidgetHead $ [shamlet|<meta charset=utf-8>|]
  toWidgetHead $ [shamlet|<meta http-equiv=X-UA-Compatible content=IE=edge>|]
  addStylesheet $ StaticR bootstrap_min_css
  addStylesheet $ StaticR bootstrapt_min_css
  addScript $ StaticR jquery_js
  addScript $ StaticR bootstrap_js
  toWidget $ [shamlet|<div #body>|]
  if dev
    then do
    addScript $ StaticR react_js
    addScript $ StaticR reactdom_js
    else do
    addScript $ StaticR react_min_js
    addScript $ StaticR reactdom_min_js

-- | The user-facing frontend.
getHomepageR :: Handler Html
getHomepageR = do
  dev <- isDevelopment <$> ask
  defaultLayout $ do
    baseSite dev
    setTitle "River Valley High School Science Lab Declaration"
    toWidgetHead $ [shamlet|<meta name=viewport content=width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=no>|]
    addScript $ StaticR jquery_mobile_custom_js
    addScript $ StaticR immutable_js
    addScript $ StaticR pako_deflate_min_js
    addScript $ StaticR b64_min_js
    if dev
      then toWidget $(juliusFileReload "templates/appv2.dev.js")
      else addScript $ StaticR appv2_min_js

generateAdminPages :: Html -> Handler Html
generateAdminPages pageTitle = do
  dev <- isDevelopment <$> ask
  actualUserPriv <- (liftM2.liftM2) (,) (lookupSession "user") (lookupSession "priv")
  simulateUserPriv <- if dev then (liftM2.liftM2) (,) (runInputGet . iopt textField $ "simuser") (runInputGet . iopt textField $ "simpriv") else return Nothing
  defaultLayout $ do
    case simulateUserPriv `mplus` actualUserPriv of
      Nothing -> return ()
      Just (u, p) -> do
        toWidgetHead $ [shamlet|<meta #meta-user name=x-labdecl-user value=#{u}>|]
        toWidgetHead $ [shamlet|<meta #meta-priv name=x-labdecl-priv value=#{p}>|]
    baseSite dev
    setTitle pageTitle
    toWidgetHead $ [shamlet|<meta name=viewport content=width=1200>|]
    addScript $ StaticR underscore_js
    if dev
      then toWidget $(juliusFileReload "static/admin.min.js")
      else addScript $ StaticR admin_min_js

getAdminLogoutR :: Handler Html
getAdminLogoutR = do
  dev <- isDevelopment <$> ask
  defaultLayout $ do
    setTitle "RVHS Science Lab Undertaking :: Admin Console :: Logout Successful"
    baseSite dev
    if dev
      then toWidget $(hamletFileReload "templates/didlogout.hamlet")
      else toWidget $(hamletFile "templates/didlogout.hamlet")

getAdminHomeR :: Handler Html
getAdminHomeR = generateAdminPages "RVHS Science Lab Undertaking :: Admin Console"

getAdminCcasR :: Handler Html
getAdminCcasR = generateAdminPages "RVHS Science Lab Undertaking :: Admin Console :: Manage CCAs"

getAdminTeachersR :: Handler Html
getAdminTeachersR = generateAdminPages "RVHS Science Lab Undertaking :: Admin Console :: Manage Teachers"

getAdminSubjectsR :: Handler Html
getAdminSubjectsR = generateAdminPages "RVHS Science Lab Undertaking :: Admin Console :: Manage Subjects"

getAdminStudentsR :: Handler Html
getAdminStudentsR = generateAdminPages "RVHS Science Lab Undertaking :: Admin Console :: Manage Students"
