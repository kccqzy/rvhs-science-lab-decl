{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module LabDecl.PDFServices (pdfServiceThread) where

import Control.Monad
import Control.Monad.Trans
import Control.Arrow (first)
import Control.Lens
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import Data.Char
import Data.Monoid
import Data.Default
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64 as C64
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Aeson as JSON
import qualified Data.Acid as Acid
import Data.Time
import Data.Aeson.TH
import qualified Network.HTTP.Client as HTTP
import Text.Shakespeare.Text (textFile)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import System.IO (openFile, hClose, IOMode(..))
import System.IO.Temp (withTempDirectory)
import qualified System.Process as Process
import qualified Yesod.Core.Types as YT

import qualified LabDecl.RNCryptor as RNCryptor
import LabDecl.TeXRenderAssets
import LabDecl.Types
import LabDecl.Utilities
import LabDecl.AcidicModels
import LabDecl.AsyncQueue

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

sendGae :: JSON.ToJSON a => String -> Bool -> HTTP.Manager -> a -> IO ()
sendGae endpoint isDevelopment manager payload = do
  let url = hostname ++ endpoint
  let encoded = CL.toStrict $ JSON.encode payload
  Right encrypted <- RNCryptor.encrypt credentials encoded
  origRequest <- HTTP.parseUrl url
  let request = HTTP.urlEncodedBody [("payload", C64.encode encrypted)] origRequest
  void $ HTTP.httpNoBody request manager
  where credentials = RNCryptor.Password "TXyT-oqOs-Jwq0-knne-Vui8-1ABd-R1b5-56jL-zhph-LcwL-PzfZ-HIkv-4sLt-AHMK-3LO3-mXEj"
        hostname = if isDevelopment then "http://localhost:8080/" else "https://rvhs-sci-lab-undertaking.appspot.com/"

-- | Sends an email using the Python daemon.
sendMail :: Bool -> HTTP.Manager -> GAEMail -> IO ()
sendMail = sendGae "mail"

uploadFile :: Bool -> HTTP.Manager -> GAEFile -> IO ()
uploadFile = sendGae "storage"

formatClass :: Class -> T.Text
formatClass klass = let (Class (l, c)) = klass in T.pack $ show l ++ [c]

generateTeX :: Student -> Maybe Teacher -> Set Subject -> CL.ByteString
generateTeX student witness scienceSubjects = TL.encodeUtf8 . TLB.toLazyText $ $(textFile "templates/report.tex") id
  where name = escapeLaTeX $ student ^. studentName
        indexNumber = student ^. studentIndexNumber
        className = formatClass (student ^. studentClass)
        subjects = escapeLaTeX . T.intercalate ", " . map (^. subjectName) . Set.toList $ scienceSubjects
        witnessName = maybe "---" (escapeLaTeX . (^. teacherWitnessName)) witness

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
    Tar.unpack dir reportAssets
    let reportDir = dir ++ "/report/"
    mapM_ (uncurry CL.writeFile . first (reportDir++)) files
    void $ runTeX lualatex "report" reportDir
    void $ runTeX lualatex "report" reportDir
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
        reportAssets = Tar.read . GZip.decompress $ reportTarGz

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
  return GAEFile{..}

generateMail :: Student -> C.ByteString -> IO GAEMail
generateMail student pdf = do
  let mailTo =
        case student ^? studentSubmission . ssEmail of
         Nothing -> error "renderMail: Cannot render for a student who has not completed submission."
         Just (Email e) -> e
  let mailSender = "River Valley High School Science Department <rvhs.science.oracle@gmail.com>"
  let mailSubject = "Completion of Undertaking Declaration Certificate (UDC)"
  let mailBody = "Thank you for taking part in the science lab undertaking exercise. The completed UDC is in the attachment for your reference."
  fileName <- generateFileName student
  let mailAttachments = [(fileName, ByteString64 pdf)]
  return GAEMail {..}

pdfServiceThread :: Bool -> Acid.AcidState Database -> HTTP.Manager -> FilePath -> FilePath -> TChan () -> TQueue AsyncInput -> TVar Bool -> YT.Logger -> IO ()
pdfServiceThread isDevelopment acid manager lualatex dir notifyChan queue canBeShutDown logger = do
  let show' = C.pack . show
  internalQueue <- atomically newTQueue
  forkIO $ internalQueueMain logger canBeShutDown internalQueue -- this should be passed in from main
  forever $ do
    (student, witness, subjects, signaturePng) <- atomically $ readTQueue queue
    let tex = generateTeX student witness subjects
    atomically . writeTQueue internalQueue $ def {
      taskName = "PDF Generation for student " <> show' (student ^. idField),
      task = do
          pdf <- generatePDF lualatex dir "report" [("sig.png", signaturePng), ("report.tex", tex)]
          atomically $
            writeTQueue internalQueue $ def {
              taskName = "Upload PDF for student " <> show' (student ^. idField),
              task = do
                  generateFileUpload student pdf >>= uploadFile isDevelopment manager
                  atomically $
                    writeTQueue internalQueue $ def {
                      taskName = "Save to database for student " <> show' (student ^. idField),
                      task = do
                          fileName <- generateFileName student
                          void . Acid.update acid $ PublicStudentSubmissionPdfRendered (student ^. studentId) fileName
                          liftIO . atomically $ writeTChan notifyChan ()
                      }
              }
          atomically $
            writeTQueue internalQueue $ def {
              taskName = "Send Mail With PDF for student " <> show' (student ^. idField),
              task = generateMail student pdf >>= sendMail isDevelopment manager
              }
      }
