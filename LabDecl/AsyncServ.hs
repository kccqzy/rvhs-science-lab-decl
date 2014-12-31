{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LabDecl.AsyncServ where

import Control.Monad
import Control.Arrow (first, second)
import Control.Exception (SomeException)
import Control.Lens
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64 as C64
import qualified Data.ByteString.Base64.Lazy as CL64
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Aeson as JSON
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

import qualified RNCryptor

import LabDecl.TeXRenderAssets
import LabDecl.Types
import LabDecl.Utilities

data GAEMail = GAEMail {
  mailSender :: T.Text,
  mailTo :: T.Text,
  mailSubject :: T.Text,
  mailBody :: TL.Text,
  mailAttachments :: [(T.Text, ByteString64)]
  } deriving (Show, Eq)

-- | ToJSON and FromJSON instances for use when returning structured
-- data.
$(deriveJSON defaultOptions {
  fieldLabelModifier = jsonLabel
  } ''GAEMail)

-- | Package up an email in the format expected by Python mail daemon.
packageMail :: GAEMail -> IO HTTP.Request
packageMail mail = do
  let encoded = CL.toStrict $ JSON.encode mail
  Right encrypted <- RNCryptor.encrypt credentials encoded
  origRequest <- HTTP.parseUrl url
  let request = HTTP.urlEncodedBody [("payload", C64.encode encrypted)] origRequest
  return request
  where credentials = RNCryptor.Password "TXyT-oqOs-Jwq0-knne-Vui8-1ABd-R1b5-56jL-zhph-LcwL-PzfZ-HIkv-4sLt-AHMK-3LO3-mXEj"
#ifdef DEVELOPMENT
        url = "http://localhost:8080/"
#else
        url = "https://rvhs-sci-lab-undertaking.appspot.com/"
#endif

-- | Sends an email using the Python mailer.
sendMail :: HTTP.Manager -> GAEMail -> IO ()
sendMail manager mail = do
  packagedMail <- packageMail mail
  void $ HTTP.httpNoBody packagedMail manager

-- | Render a PDF file, given a binary to lualatex, a working
-- directory, a jobname and other supporting files.
renderPDF :: FilePath -> FilePath -> String -> [(FilePath, CL.ByteString)] -> IO C.ByteString
renderPDF lualatex dir jobname files = withTempDirectory dir "latexjob" $ \dir -> do
    Tar.unpack dir  . Tar.read . GZip.decompress $ reportTarGz
    let reportDir = dir ++ "/report/"
    mapM_ (uncurry CL.writeFile . first (reportDir++)) $ files
    runTeX lualatex "report" reportDir
    runTeX lualatex "report" reportDir
    C.readFile $ reportDir ++ "/" ++ jobname ++ ".pdf"
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
          ec <- Process.waitForProcess ph
          hClose devNullR
          hClose devNullW
          return ec

-- | Generate a TeX file to render into PDF.
generateTeX :: Student -> CL.ByteString
generateTeX student = TL.encodeUtf8 . TLB.toLazyText $ $(textFile "templates/report.tex") id
  where name = student ^. studentName
        indexNumber = student ^. studentIndexNumber
        className = let (Class (l, c)) = student ^. studentClass in show l ++ [c]
        subjects = "Insert Subjects Here" :: T.Text

generateMail :: FilePath -> FilePath -> Student -> IO GAEMail
generateMail lualatex dir student = do
  let tex = generateTeX student
  let signature =
        case join $ student ^? studentSubmission . ssSignature of
         Nothing -> error "renderMail: Cannot render for a student who has not completed submission."
         Just (ByteString64 bs) -> CL.fromChunks [bs]
  let email =
        case student ^? studentSubmission . ssEmail of
         Nothing -> error "renderMail: Cannot render for a student who has not completed submission."
         Just (Email e) -> T.concat [ student ^. studentName, " <", e, ">" ]
  pdf <- renderPDF lualatex dir "report" [("sig.png", signature), ("report.tex", tex)]
  return GAEMail {
    mailSender = "River Valley High School Science Department <wzcoe.science@gmail.com>",
    mailTo = email,
    mailSubject = "Completion of Science Lab Undertaking Form",
    mailBody = "Thank you for taking part in the science lab undertaking exercise. The completed declaration form is in the attachment for your reference.",
    mailAttachments = [("Completed_Declaration_Form.pdf", ByteString64 pdf)]
    }

-- | A worker thread that consumes values from a TQueue.
queuedThread :: (a -> IO ()) -> (a -> SomeException -> IO ()) -> TQueue a -> IO ()
queuedThread op onError queue = forever $ do
  input <- atomically $ readTQueue queue
  asyncOp <- async $ op input
  result <- waitCatch asyncOp
  either (onError input) return result

-- | A thread that renders emails asynchronously (but not concurrently).
renderMailThread :: FilePath -> FilePath -> (GAEMail -> IO ()) -> TQueue Student -> IO ()
renderMailThread lualatex dir next = queuedThread (generateMail lualatex dir >=> next) $ \student exc -> logM "renderMailThread" ERROR $ "LaTeX render failed (details = " ++ show exc ++ ") when rendering for student " ++ show student

-- | A thread that sends emails asynchronously (but not concurrently).
sendMailThread :: HTTP.Manager -> (() -> IO ()) -> TQueue GAEMail -> IO ()
sendMailThread httpManager next = queuedThread (sendMail httpManager >=> next) $ \mail exc -> logM "sendMailThread" ERROR $ "sendMail failed (details = " ++ show exc ++ ") when sending mail to " ++ (T.unpack $ mailTo mail)
