{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module LabDecl.AsyncServ where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Arrow (first, second)
import Control.Exception (SomeException)
import Control.Lens
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import Data.Char
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64 as C64
import qualified Data.ByteString.Base64.Lazy as CL64
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Aeson as JSON
import qualified Data.Acid as Acid
import qualified Data.Acid.Advanced as Acid
import Data.Time
import Data.Aeson.TH
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import Text.Shakespeare.Text (textFile)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import System.Log.Logger (Priority(..), logM) -- TODO use System.Log.FastLogger instead
import System.Posix.Files (setFileMode, ownerExecuteMode)
import System.IO (openFile, hClose, IOMode(..))
import System.IO.Temp (withTempDirectory, withSystemTempDirectory, createTempDirectory)
import qualified System.Process as Process
import System.Exit (ExitCode(..))
import System.Locale (defaultTimeLocale)

import qualified RNCryptor

import LabDecl.TeXRenderAssets
import LabDecl.Types
import LabDecl.Utilities
import LabDecl.AcidicModels

data GAEMail = GAEMail {
  mailSender :: T.Text,
  mailTo :: T.Text,
  mailSubject :: T.Text,
  mailBody :: TL.Text,
  mailAttachments :: [(T.Text, ByteString64)]
  } deriving (Show, Eq)

data GAEFile = GAEFile {
  fileName :: T.Text,
  fileContentType :: T.Text,
  fileContent :: ByteString64
  } deriving (Show, Eq)

-- | ToJSON and FromJSON instances for use when returning structured
-- data.
$(liftM concat . mapM (deriveJSON defaultOptions {
  fieldLabelModifier = jsonLabel
  }) $ [''GAEMail, ''GAEFile])

type AsyncPipeline a b = a -> IO b

sendGae :: JSON.ToJSON a => String -> HTTP.Manager -> a -> IO ()
sendGae endpoint manager payload = do
  let url = hostname ++ endpoint
  let encoded = CL.toStrict $ JSON.encode payload
  Right encrypted <- RNCryptor.encrypt credentials encoded
  origRequest <- HTTP.parseUrl url
  let request = HTTP.urlEncodedBody [("payload", C64.encode encrypted)] origRequest
  void $ HTTP.httpNoBody request manager
  where credentials = RNCryptor.Password "TXyT-oqOs-Jwq0-knne-Vui8-1ABd-R1b5-56jL-zhph-LcwL-PzfZ-HIkv-4sLt-AHMK-3LO3-mXEj"
#ifdef DEVELOPMENT
        hostname = "http://localhost:8080/"
#else
        hostname = "https://rvhs-sci-lab-undertaking.appspot.com/"
#endif

-- | Sends an email using the Python daemon.
sendMail :: HTTP.Manager -> GAEMail -> IO ()
sendMail = sendGae "mail"

uploadFile :: HTTP.Manager -> GAEFile -> IO ()
uploadFile = sendGae "storage"

formatClass :: Class -> T.Text
formatClass klass = let (Class (l, c)) = klass in T.pack $ show l ++ [c]

generateTeX :: Student -> Set Subject -> CL.ByteString
generateTeX student scienceSubjects = TL.encodeUtf8 . TLB.toLazyText $ $(textFile "templates/report.tex") id
  where name = escapeLaTeX $ student ^. studentName
        indexNumber = student ^. studentIndexNumber
        className = formatClass (student ^. studentClass)
        subjects = escapeLaTeX . T.intercalate ", " . map (^. subjectName) . Set.toList $ scienceSubjects

escapeLaTeX :: T.Text -> T.Text
escapeLaTeX = foldr1 (.) . map (uncurry T.replace) $ [("~", "\\textasciitilde "),
                                                      ("^", "\\textasciicircum "),
                                                      ("&", "\\&"),
                                                      ("%", "\\%"),
                                                      ("$", "\\$"),
                                                      ("#", "\\#"),
                                                      ("_", "\\_"),
                                                      ("{", "\\{"),
                                                      ("}", "\\}"),
                                                      ("\\", "\\textbackslash ")]

-- | Render a PDF file, given a binary to lualatex, a working
-- directory, a jobname and other supporting files.
generatePDF :: FilePath -> FilePath -> String -> [(FilePath, CL.ByteString)] -> IO C.ByteString
generatePDF lualatex dir jobname files = withTempDirectory dir "latexjob" $ \dir -> do
    Tar.unpack dir  . Tar.read . GZip.decompress $ reportTarGz
    let reportDir = dir ++ "/report/"
    mapM_ (uncurry CL.writeFile . first (reportDir++)) $ files
    runTeX lualatex "report" reportDir
    runTeX lualatex "report" reportDir
    C.readFile $ reportDir ++ jobname ++ ".pdf"
  where runTeX tex jobname cwd = do
          devNullR <- openFile "/dev/null" ReadMode
          devNullW <- openFile "/dev/null" WriteMode
          let texProcess = (Process.proc tex ["-interaction=batchmode", jobname]) {
                Process.cwd = Just cwd,
                Process.env = Just [("PATH", ":")], -- used by luaotfload to detect system type
                Process.std_in = Process.UseHandle devNullR,
                Process.std_out = Process.UseHandle devNullW,
                Process.std_err = Process.UseHandle devNullW }
          (_, _, _, ph) <- Process.createProcess texProcess
          timer <- async (threadDelay 300000000 >> Process.terminateProcess ph)
          ec <- Process.waitForProcess ph
          cancel timer
          hClose devNullR
          hClose devNullW
          return ec

generateFileName :: Student -> IO T.Text
generateFileName student = do
  year <- T.pack . formatTime defaultTimeLocale "%Y" <$> getCurrentTime
  let klass = formatClass (student ^. studentClass)
  let index = T.pack . show $ student ^. studentIndexNumber
  let name = T.filter (liftM2 (||) isAsciiUpper isAsciiLower) $ student ^. studentName
  return $ T.append (T.intercalate "_" [year, klass, index, name]) ".pdf"

generateFileUpload :: Student -> C.ByteString -> IO GAEFile
generateFileUpload student pdf = do
  fileName <- generateFileName student
  let fileContent = ByteString64 pdf
  let fileContentType = "application/pdf"
  return $ GAEFile{..}

generateMail :: Student -> C.ByteString -> IO GAEMail
generateMail student pdf = do
  let mailTo =
        case student ^? studentSubmission . ssEmail of
         Nothing -> error "renderMail: Cannot render for a student who has not completed submission."
         Just (Email e) -> e
  let mailSender = "River Valley High School Science Department <rvhs.science.oracle@gmail.com>"
  let mailSubject = "Completion of Science Lab Undertaking Form"
  let mailBody = "Thank you for taking part in the science lab undertaking exercise. The completed declaration form is in the attachment for your reference."
  fileName <- generateFileName student
  let mailAttachments = [(fileName, ByteString64 pdf)]
  return GAEMail {..}

type AsyncInput = (Student, Set Subject, CL.ByteString)

asyncMain :: Acid.AcidState Database -> HTTP.Manager -> FilePath -> FilePath -> TChan () -> TQueue AsyncInput -> IO ()
asyncMain acid manager lualatex dir notifyChan queue = forever $ do
  (student, subjects, signaturePng) <- atomically $ readTQueue queue
  let tex = generateTeX student subjects
  genPDFOp <- async $ generatePDF lualatex dir "report" [("sig.png", signaturePng), ("report.tex", tex)]
  eitherPDF <- waitCatch genPDFOp
  case eitherPDF of
   Left e -> logM "generatePDF" ERROR $ "LaTeX render failed (details = " ++ show e ++ ") when rendering for student " ++ show student
   Right pdf -> do
     filename <- generateFileName student
     fileUploadOp <- liftIO . async $ generateFileUpload student pdf >>= uploadFile manager
     sendMailOp <- liftIO . async $ generateMail student pdf >>= sendMail manager
     eitherFileUpload <- waitCatch fileUploadOp
     eitherSendMail <- waitCatch sendMailOp
     case eitherFileUpload of
      Left e -> logM "uploadFile" ERROR $ "Upload PDF failed (details = " ++ show e ++ ") when rendering for student " ++ show student
      _ -> return ()
     case eitherSendMail of
      Left e -> logM "sendMail" ERROR $ "Send mail failed (details = " ++ show e ++ ") when rendering for student " ++ show student
      _ -> return ()
     Acid.update acid $ PublicStudentSubmissionPdfRendered (student ^. studentId) filename
     liftIO . atomically $ writeTChan notifyChan ()
