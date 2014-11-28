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
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import Control.Error
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Aeson as JSON
import qualified Data.Acid as Acid
import qualified Data.Acid.Advanced as Acid
import qualified Network.HTTP.Types as HTTP
import qualified Network.WebSockets as WS
import Network.Wai.Parse (lbsBackEnd)
import Yesod.Core
import Yesod.Form
import Yesod.WebSockets (webSockets, WebSocketsT, sendTextData)

import LabDecl.Utilities
import LabDecl.Types
import LabDecl.AcidicModels
import LabDecl.StudentCSV
import LabDecl.Models

-- | The foundation data type.
data LabDeclarationApp = LabDeclarationApp {
  getAcid :: Acid.AcidState Database,
  getNotifyChan :: TChan ()
  }

-- | The routing. Routes of the form /api/items allow GET and POST,
-- which are list* and add* respectively. Routes of the form
-- /api/items/#Int allow GET PUT DELETE, which are lookup*ById,
-- replace* and remove* respectively. GET requests of /api/items allow
-- searching through query args. Most of them just does simple
-- marshalling and then invoke the events in LabDecl.AcidicModels.
$(mkYesod "LabDeclarationApp" [parseRoutes|
/api/ccas                 CcasR     GET POST
/api/ccas/#CcaId          CcaR      GET PUT DELETE
/api/subjects             SubjectsR GET POST
/api/subjects/#SubjectId  SubjectR  GET PUT DELETE
/api/teachers             TeachersR GET POST
/api/teachers/#TeacherId  TeacherR  GET PUT DELETE
/api/students             StudentsR GET POST
/api/students/#StudentId  StudentR  GET PUT DELETE
|])

instance Yesod LabDeclarationApp where
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

-- | Enumerate all CCAs with all information. Public.
getCcasR :: Handler Value
getCcasR = acidQueryHandler ListCcas

-- | Add new CCA. Requires admin.
postCcasR :: Handler Value
postCcasR = do
  request <- runInputPost $ Cca
             <$> pure (CcaId 0)
             <*> ireq textField "name"
             <*> ireq textField "category"
  acidUpdateHandler $ AddCca request

-- | Get information about a single Cca. Public.
getCcaR :: CcaId -> Handler Value
getCcaR ccaId = acidQueryHandler $ LookupCcaById ccaId

-- | Edit CCA. Requires admin.
putCcaR :: CcaId -> Handler Value
putCcaR ccaId = do
  request <- runInputPost $ Cca -- TODO refactor and share code with post
             <$> pure ccaId
             <*> ireq textField "name"
             <*> ireq textField "category"
  acidUpdateHandler $ ReplaceCca request

deleteCcaR :: CcaId -> Handler Value
deleteCcaR ccaId = acidUpdateHandler $ RemoveCca ccaId

getSubjectsR :: Handler Value
getSubjectsR = undefined

postSubjectsR :: Handler Value
postSubjectsR = undefined

getSubjectR :: SubjectId -> Handler Value
getSubjectR = undefined

putSubjectR :: SubjectId -> Handler Value
putSubjectR = undefined

deleteSubjectR :: SubjectId -> Handler Value
deleteSubjectR = undefined

getTeachersR :: Handler Value
getTeachersR = undefined

postTeachersR :: Handler Value
postTeachersR = undefined

getTeacherR :: TeacherId -> Handler Value
getTeacherR = undefined

putTeacherR :: TeacherId -> Handler Value
putTeacherR = undefined

deleteTeacherR :: TeacherId -> Handler Value
deleteTeacherR = undefined

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
