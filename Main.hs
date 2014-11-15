{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Exception (bracket, SomeException)
import Control.Arrow (first, second)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, waitCatch)
import Control.Concurrent.STM.TQueue
import Control.Monad.STM (atomically)
import Data.Maybe
import Data.Either
import Data.String (fromString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TLB (toLazyText)
import qualified Data.Attoparsec.ByteString.Char8 as PC
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import qualified Data.SafeCopy as SafeCopy
import qualified Data.Acid as Acid
import qualified Data.Acid.Local as Acid
import qualified Data.Acid.Advanced as Acid
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.Mail.Mime as MIME
import qualified Network.Mail.Mime.SES as SES
import System.Environment (lookupEnv)
import System.Log.Logger (Priority(..), logM) -- TODO use System.Log.FastLogger instead
import System.Posix.Files (setFileMode, ownerExecuteMode)
import System.IO (openFile, hClose, IOMode(..))
import System.IO.Temp (withTempDirectory, withSystemTempDirectory, createTempDirectory)
import qualified System.Process as Process
import System.Exit (ExitCode(..))
import Text.Shakespeare.Text (textFile)
import Text.Cassius (cassiusFile)
import Text.Hamlet (hamletFile)
import Text.Julius (juliusFile)
import Text.Jasmine (minifym)
import qualified Codec.Picture.Png as Png
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Yesod.Core
import Yesod.Form
import Yesod.EmbeddedStatic

import TeXRenderAssets

-- |
-- = Basic Data Structures & Operations
-- The basic data structures used in the project. All data structures
-- are prefix by LD. A small set of permitted operations are also
-- provided that uses the Update and Query monad.

-- | The class, i.e. "5N".
type LDClassName = T.Text

-- | The science subject, combined science (Y1-2), physics, chemistry
-- or bio. Implementatio note: we could have made a newtype instead:
-- > newtype LDScienceSubject = LDScienceSubject { describeSubject :: T.Text }
-- but this means set operations would require traversing the text
-- instead of just the symbol.
data LDScienceSubject = LDScience
                      | LDDnT
                      | LDPhysics
                      | LDChemistry
                      | LDBiomedicalScience
                      | LDH2Physics
                      | LDH2Chemistry
                      | LDH2Biology
                      deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Give a human readable name for a science subject.
describeSubject :: LDScienceSubject -> T.Text
describeSubject LDScience = "Combined Science"
describeSubject LDDnT = "Design & Technology"
describeSubject LDPhysics = "Physics"
describeSubject LDChemistry = "Chemistry"
describeSubject LDBiomedicalScience = "Biomedical Science"
describeSubject LDH2Physics = "H2 Physics"
describeSubject LDH2Chemistry = "H2 Chemistry"
describeSubject LDH2Biology = "H2 Biology"

instance ToJSON LDScienceSubject where
  toJSON = toJSON . describeSubject
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''LDScienceSubject)

-- | All the information submitted through the UI: phone, email and
-- the submitted signature.
data LDSubmissionStatus = LDNotSubmittedYet
                        | LDSubmitted
                          { getSubmittedPhone :: T.Text,
                            getSubmittedEmail :: T.Text,
                            getSubmittedSignature :: T.Text -- TODO store raw data, not base64
                          } deriving (Show, Eq, Data, Typeable, Generic)
instance ToJSON LDSubmissionStatus
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''LDSubmissionStatus)

-- | The student record.
data LDStudent = LDStudent
                 { getName :: !T.Text,
                   getSubjectCombination :: Set LDScienceSubject,
                   getSubmissionStatus :: LDSubmissionStatus
                 } deriving (Show, Eq, Data, Typeable, Generic)
instance ToJSON LDStudent
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''LDStudent)

type LDIndexNumber = Int
type LDClass = IntMap LDStudent
type LDClasses = Map LDClassName LDClass

newtype LDStorage = LDStorage { getClasses :: LDClasses } deriving (Show, Data, Typeable, ToJSON)
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''LDStorage)

data GenericMail = GenericMail {
  mailFrom :: MIME.Address,
  mailTo :: MIME.Address,
  mailSubject :: T.Text,
  mailBody :: TL.Text,
  mailAttachents :: [(T.Text, T.Text, CL.ByteString)]
  }

-- |
-- == Permitted Operations

-- | A wrapper that makes a simple function into a Query monad.
wrapQuery :: (LDClasses -> a) -> Acid.Query LDStorage a
wrapQuery theQuery = liftM (theQuery . getClasses) ask

-- | A wrapper that makes a simple function into an Update monad.
wrapUpdate :: (LDClasses -> (LDClasses, a)) -> Acid.Update LDStorage a
wrapUpdate theUpdate = liftM2 (>>) (put . LDStorage . fst) (return . snd) . theUpdate . getClasses =<< get

-- TODO make all the Maybe in the op* functions Either T.Text

-- | Lists available classes.
opGetClasses :: LDClasses -> Maybe [LDClassName]
opGetClasses = Just . Map.keys
-- TODO should filter out classes without students? without students who haven't completed submission?

-- | Lists all students in a particular class.
opGetAllStudentsInClass :: LDClassName -> LDClasses -> Maybe [(LDIndexNumber, LDStudent)]
opGetAllStudentsInClass className s = do
  klass <- Map.lookup className s
  return $ IntMap.assocs klass

-- | Lists all students in a particular class that did not complete
-- submission.
opGetStudentsWithoutSubmissionInClass :: LDClassName -> LDClasses -> Maybe [(LDIndexNumber, LDStudent)]
opGetStudentsWithoutSubmissionInClass className s = do
  students <- opGetAllStudentsInClass className s
  return $ filter (not . hasSubmitted . getSubmissionStatus . snd) students
  where hasSubmitted LDNotSubmittedYet = False
        hasSubmitted _ = True

-- | Get full information for a particular student.
opGetStudentInfo :: LDClassName -> LDIndexNumber -> LDClasses -> Maybe LDStudent
opGetStudentInfo className indexNumber s = do
  klass <- Map.lookup className s
  IntMap.lookup indexNumber klass

-- | Submit information for a student.
opDoSubmission :: LDSubmissionStatus -> LDClassName -> LDIndexNumber -> LDClasses -> (LDClasses, Maybe ())
opDoSubmission submission className indexNumber = (, Just ()) . updateClasses
  where updateClasses = Map.adjust updateClass className
        updateClass = IntMap.adjust updateStudent indexNumber
        updateStudent student = student { getSubmissionStatus = submission }
        -- TODO consider using updateLookupWithKey to determine whether it has indeed been changed; don't fail silently

-- | Add a new student.
opAddStudent :: LDClassName -> LDIndexNumber -> LDStudent -> LDClasses -> (LDClasses, Maybe ())
opAddStudent className indexNumber student = (, Just ()) . updateClasses
  where updateClasses = Map.insertWith (const updateClass) className defaultClass
        defaultClass = IntMap.singleton indexNumber student
        updateClass = IntMap.insert indexNumber student

#include "TestData.hs"

-- | The query and update functions wrapped in Query and Update
-- monads.
-- TODO investigate using TH to auto-generate things
queryGetClasses = wrapQuery opGetClasses
queryGetAllStudentsInClass = wrapQuery . opGetAllStudentsInClass
queryGetStudentsWithoutSubmissionInClass = wrapQuery . opGetStudentsWithoutSubmissionInClass
queryGetStudentInfo = (wrapQuery .) . opGetStudentInfo
updateDoSubmission = ((wrapUpdate .) .) . opDoSubmission
updateAddStudent = ((wrapUpdate .) .) . opAddStudent

-- | Control structures for making transactions acidic.
$(Acid.makeAcidic ''LDStorage ['queryGetClasses,
                               'queryGetAllStudentsInClass,
                               'queryGetStudentsWithoutSubmissionInClass,
                               'queryGetStudentInfo,
                               'updateDoSubmission,
                               'updateAddStudent])

-- | The embeded static files subsite.
mkEmbeddedStatic False "eStatic" [embedDir "static"]

-- | The main webapp.
data LabDeclarationApp = LabDeclarationApp
                         { getStatic :: EmbeddedStatic,
                           getAcid :: Acid.AcidState LDStorage,
                           getRenderQueue :: TQueue (LDClassName, LDIndexNumber, LDStudent)
                         }

mkYesod "LabDeclarationApp" [parseRoutes|
/static StaticR EmbeddedStatic getStatic
/api/classes ClassesR GET
/api/classes/#LDClassName StudentsR GET
/api/classes/#LDClassName/#LDIndexNumber StudentInfoR GET POST
/ HomepageR GET
|]

instance Yesod LabDeclarationApp where
  addStaticContent = embedStaticContent getStatic StaticR minifym
  errorHandler (InvalidArgs ia) = return . toTypedContent $ object [ "meta" .= object [ "code" .= (400 :: Int), "details" .= ia ] ]
  errorHandler other = defaultErrorHandler other

instance RenderMessage LabDeclarationApp FormMessage where
  renderMessage _ _ = defaultFormMessage

-- |
-- = Handlers
-- The actual handlers that handle requests.

-- |
-- == API Handlers
-- These handlers may receive www-urlencoded data and return JSON
-- response.

-- | A generic handler that interfaces with Acid queries and updates.
acidHandler
  :: (Acid.Method ev, ToJSON a, Acid.MethodResult ev ~ Maybe t,
      Acid.MethodState ev ~ LDStorage) =>
     (Acid.AcidState (Acid.EventState ev) -> ev -> IO (Acid.EventResult ev))
     -> (t -> a) -> ev -> Handler Value
acidHandler action f theAction = do
  acid <- getAcid <$> ask
  result <- lift $ action acid theAction
  let responseEnvelope status = [ "meta" .= object [ "code" .= HTTP.statusCode status ] ]
  case result of
   Nothing -> sendResponseStatus HTTP.status400 $ object $ responseEnvelope HTTP.status400
   Just successResult -> return $ object $ responseEnvelope HTTP.status200 ++ [ "data" .= f successResult ]

getClassesR :: Handler Value
getClassesR = acidHandler Acid.query id QueryGetClasses

getStudentsR :: LDClassName -> Handler Value
getStudentsR = acidHandler Acid.query (map (second getName)) . QueryGetStudentsWithoutSubmissionInClass

getStudentInfoR :: LDClassName -> LDIndexNumber -> Handler Value
getStudentInfoR = (acidHandler Acid.query id .) . QueryGetStudentInfo
-- TODO this is insecure; should filter out submission status based on auth result

postStudentInfoR :: LDClassName -> LDIndexNumber -> Handler Value
postStudentInfoR className indexNumber = do
  request <- runInputPost $ LDSubmitted
             <$> ireq sgPhoneField "phone"
             <*> ireq whatWgEmailField "email"
             <*> ireq sigPngField "signature"
  response <- acidHandler Acid.update id $ UpdateDoSubmission request className indexNumber
  renderQueue <- getRenderQueue <$> ask
  acid <- getAcid <$> ask
  Just newStudent <- lift $ Acid.query acid $ QueryGetStudentInfo className indexNumber -- TODO refactor
  liftIO . atomically $ writeTQueue renderQueue (className, indexNumber, newStudent)
  return response
  where sgPhoneField = checkBool isValidSgPhone ("Invalid phone number" :: T.Text) textField
        whatWgEmailField = checkBool isValidWhatWgEmail ("Invalid email" :: T.Text) textField
        sigPngField = checkBool isValidPngDataUrl ("Invalid signature" :: T.Text) textField

-- |
-- === Data validation

-- | Tests whether a given text string is a valid email. This uses the
-- validation found in WHATWG HTML standard, not RFC 5322.
isValidWhatWgEmail :: T.Text -> Bool
isValidWhatWgEmail = isValidWhatWgEmail' . T.encodeUtf8
  where isValidWhatWgEmail' = (isRight .) . PC.parseOnly $ do
          username
          PC.char '@'
          domainPart `PC.sepBy1` PC.char '.'
          PC.endOfInput
        username = PC.takeWhile1 $ PC.inClass "a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-"
        domainPart = do
          r <- PC.takeWhile1 (PC.inClass "a-zA-Z0-9") `PC.sepBy1` PC.char '-'
          guard $ C.length (C.intercalate "-" r) <= 63

-- | Tests whether a given text is a Singaporean phone number, with
-- stringent format requirements.
isValidSgPhone :: T.Text -> Bool
isValidSgPhone = isValidSgPhone' . T.encodeUtf8
  where isValidSgPhone' = (isRight .) . PC.parseOnly $ do
          PC.string "+65 "
          PC.count 4 PC.digit
          PC.char ' '
          PC.count 4 PC.digit
          PC.endOfInput

-- | Decode a base64-encoded data URL containing a PNG image to a
-- bytestring containing the PNG image.
decodePngDataUrl :: T.Text -> Either String C.ByteString
decodePngDataUrl = (Base64.decode =<<) .  getPngBase64 . T.encodeUtf8
  where getPngBase64 = PC.parseOnly $ do
          PC.string "data:image/png;base64,"
          PC.takeByteString

-- | Tests whether the submitted data URL is a valid PNG image.
isValidPngDataUrl :: T.Text -> Bool
isValidPngDataUrl = isRight . (Png.decodePng =<<) . decodePngDataUrl

-- |
-- == HTML Handlers
-- These handlers return HTML responses.

-- | The user-facing frontend.
getHomepageR :: Handler Html
getHomepageR = defaultLayout $ do
  setTitle "River Valley High School Science Lab Declaration"
  toWidgetHead $(hamletFile "templates/app.head.hamlet")
  addScript $ StaticR jquery_js
  addScript $ StaticR jquery_mobile_custom_js
  toWidget $(juliusFile "templates/app.julius")
  toWidget $(hamletFile "templates/app.hamlet")
  toWidget $(cassiusFile "templates/app.cassius")

-- |
-- = Asynchronous Services
-- The LaTeX renderer and the email sender.

-- | The TeX Render environment is an environment that can take a job
-- name, a list of files and render into a PDF bytestring. It is
-- caller's responsibility to ensure jobname.tex exists.
newtype PDFRenderer = PDFRenderer {
  getPDFRenderer :: String -> [(FilePath, CL.ByteString)] -> IO CL.ByteString
  }

-- | Prepare a small TeX Live 2014 distribution and the templates.
-- Takes a directory to place the TeX distribution, the templates
-- files and returns a TeXRenderEnv.
makePDFRenderer :: FilePath -> IO PDFRenderer
makePDFRenderer dir = do
  Tar.unpack dir  . Tar.read . GZip.decompress $ texliveTarGz
  let lualatex = dir ++ "/texlive-2014-portable/bin/x86_64-linux/lualatex"
  setFileMode lualatex ownerExecuteMode
  return . PDFRenderer $ \jobname files -> withTempDirectory dir "latexjob" $ \dir -> do
    Tar.unpack dir  . Tar.read . GZip.decompress $ reportTarGz
    let reportDir = dir ++ "/report/"
    mapM_ (uncurry CL.writeFile . first (reportDir++)) $ files
    runTeX lualatex "report" reportDir
    ec <- runTeX lualatex "report" reportDir
    case ec of
     ExitFailure e -> error $ "lualatex returned nonzero exit code " ++ show e
     ExitSuccess -> CL.readFile $ reportDir ++ "/" ++ jobname ++ ".pdf"
  where runTeX tex jobname cwd = do
          devNull <- openFile "/dev/null" ReadWriteMode
          let texProcess = (Process.proc tex ["-interaction=batchmode", jobname]) {
                Process.cwd = Just cwd,
                Process.env = Just [("PATH", ":")], -- used by luaotfload to detect system type
                Process.std_in = Process.UseHandle devNull,
                Process.std_out = Process.UseHandle devNull,
                Process.std_err = Process.UseHandle devNull }
          (_, _, _, ph) <- Process.createProcess texProcess
          ec <- Process.waitForProcess ph
          hClose devNull
          return ec

-- | Generate a TeX file to render into PDF.
generateTeX :: LDStudent -> LDClassName -> LDIndexNumber -> CL.ByteString
generateTeX (LDStudent name subj _) className indexNumber = TL.encodeUtf8 . TLB.toLazyText $ $(textFile "templates/report.tex") id

renderMail :: PDFRenderer -> (LDClassName, LDIndexNumber, LDStudent) -> IO GenericMail
renderMail renderer (className, indexNumber, student) = do
  let tex = generateTeX student className indexNumber
  pdf <- (getPDFRenderer renderer) "report" [("sig.png", signature), ("report.tex", tex)]
  return GenericMail {
    mailFrom = MIME.Address (Just "Chow Ban Hoe") "chow_ban_hoe@qzy.st",
    mailTo = MIME.Address (Just . getName $ student) "lab-decl-test@qzy.st",
    mailSubject = "Science Lab Undertaking Form",
    mailBody = "Thank you for taking part in the science lab undertaking exercise. The completed declaration form is in the attachment.",
    mailAttachents = [("application/pdf", "Completed_Declaration_Form.pdf", pdf)]
    }
  where signature = case getSubmissionStatus student of
                     LDNotSubmittedYet -> error "renderMail: Cannot render for a student who has not completed submission"
                     LDSubmitted _ _ s -> forceDecode s
        forceDecode s = CL.fromChunks $ rights [ decodePngDataUrl s ]

-- | Sends an email from SES.
sendMail :: HTTP.Manager -> GenericMail -> IO ()
sendMail manager mail = do
  credentials <- (liftM2 . liftM2) (,) (lookupEnv "AWSAccessKeyId") (lookupEnv "AWSSecretKey")
  case credentials of
   Nothing -> error "Cannot send email because no credentials found"
   Just (key, secret) -> SES.renderSendMailSES manager (ses (C.pack key) (C.pack secret) SES.usEast1) mailFinal
  where mailNoAttachments = liftM4 MIME.simpleMail' mailTo mailFrom mailSubject mailBody mail
        addAttachment (ct, fn, content) = MIME.addPart [MIME.Part ct MIME.Base64 (Just fn) [] content]
        mailFinal = foldr addAttachment mailNoAttachments . mailAttachents $ mail
        ses = SES.SES (getAddrBS mailFrom) [getAddrBS mailTo]
        getAddrBS which = T.encodeUtf8 . MIME.addressEmail . which $ mail

-- | A worker thread that consumes values from a TQueue.
queuedThread :: TQueue a -> ContT () IO (Either (a, SomeException) a)
queuedThread queue = ContT $ \continue -> forever $ do
  input <- atomically $ readTQueue queue
  asyncOp <- async . continue . Right $ input
  result <- waitCatch asyncOp
  either (continue . Left . (input,)) return result

-- | A thread that renders emails asynchronously (but not concurrently).
renderMailThread :: PDFRenderer -> TQueue (LDClassName, LDIndexNumber, LDStudent) -> ContT () IO GenericMail
renderMailThread renderer = queuedThread >=> ContT . flip (either logError . (renderMail renderer >=>))
  where logError (student, exc) = logM "renderMailThread" ERROR $ "LaTeX render failed (details = " ++ show exc ++ ") when rendering for student " ++ show student

-- | A thread that sends emails asynchronously (but not concurrently).
sendMailThread :: HTTP.Manager -> TQueue GenericMail -> ContT () IO ()
sendMailThread httpManager = queuedThread >=> ContT . flip (either logError . (sendMail httpManager >=>))
  where logError (mail, exc) = logM "sendMailThread" ERROR $ "sendMail failed (details = " ++ show exc ++ ") when sending mail to " ++ (T.unpack . MIME.addressEmail $ mailTo mail)

main = withSystemTempDirectory "labdecld" $ \dir -> do
  -- configuration from environment variables
  host <- liftM fromString <$> lookupEnv "HOST"
  port <- (>>= either (const Nothing) Just . PC.parseOnly PC.decimal . C.pack) <$> lookupEnv "PORT"
  let setSettings = foldr (.) id $ catMaybes [Warp.setPort <$> port, Warp.setHost <$> host]

  -- queues
  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  mailQueue <- atomically newTQueue
  forkIO $ runContT (sendMailThread httpManager mailQueue) return

  rendererTempDir <- createTempDirectory dir "renderer"
  renderer <- makePDFRenderer rendererTempDir
  renderQueue <- atomically newTQueue
  forkIO $ runContT (renderMailThread renderer renderQueue) (atomically . writeTQueue mailQueue)

  -- acid state
  bracket acidBegin acidFinally $ \acid ->
    toWaiApp (LabDeclarationApp eStatic acid renderQueue) >>= Warp.runSettings (setSettings Warp.defaultSettings)
  where acidBegin = Acid.openLocalState $ LDStorage ldClassesTestData
        acidFinally = Acid.createCheckpointAndClose
