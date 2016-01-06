{-# LANGUAGE OverloadedStrings #-}
module LabDecl.Main where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.String (fromString)
import Data.Default (def)
import qualified Data.ByteString.Char8 as C
import qualified Data.Attoparsec.ByteString.Char8 as PC
import qualified Data.Text as T
import qualified Data.Acid.Local as Acid
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import System.Posix.Signals
import qualified System.Log.FastLogger as FastLogger
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.Wai.Logger as WaiLogger
import System.Environment (getEnv, lookupEnv)
import System.IO.Temp (withTempDirectory, createTempDirectory)
import Yesod.Core.Dispatch
import qualified Yesod.Core.Types as YT

import LabDecl.Handlers
import LabDecl.PDFServices

main :: IO ()
main = do
  developmentEnvVar <- lookupEnv "DEVELOPMENT"
  let isDevelopment = case developmentEnvVar of
        Nothing -> False
        Just "0" -> False
        Just "1" -> True
        Just _ -> error "The environment variable DEVELOPMENT should be unset, or set to 0 or 1."
  when isDevelopment $ putStrLn "WARNING: Currently running in DEVELOPMENT mode."
  -- no, we do not use /tmp, you must pass the temp dir explicitly
  tmpdir <- getEnv "LATEX_RUN_FOLDER"
  withTempDirectory tmpdir "labdecld" $ \dir -> do

    -- get all the credentials and configurations we need
    googleClientId <- T.pack <$> getEnv "GOOGLE_CLIENT_ID"
    googleClientSecret <- T.pack <$> getEnv "GOOGLE_CLIENT_SECRET"
    lualatex <- getEnv "LUALATEX"

    -- warp configuration from environment variables
    host <- fromString <$> getEnv "LISTEN_HOST"
    portNumOrParseFail <- (PC.parseOnly PC.decimal . C.pack) <$> getEnv "LISTEN_PORT"
    let port = either (const (error "haha")) id portNumOrParseFail
    let setSettings = foldr (.) (Warp.setServerName "Warp") [Warp.setPort port, Warp.setHost host]

    -- Yesod approot setting
    approot <- fromString <$> getEnv "APPROOT"

    -- HTTP manager
    httpManager <- HTTP.newManager HTTP.tlsManagerSettings

    -- TVars, queues and channels
    notifyChan <- atomically newBroadcastTChan
    asyncQueue <- atomically newTQueue
    canBeShutDown <- atomically $ newTVar True
    shutdownSignal <- atomically newEmptyTMVar

    rendererTempDir <- createTempDirectory dir "renderer"

    -- POSIX signal handlers
    let onReceivePOSIXSignal = atomically $ putTMVar shutdownSignal True
    installHandler sigINT (Catch onReceivePOSIXSignal) Nothing
    installHandler sigTERM (Catch onReceivePOSIXSignal) Nothing

    -- after shutdown
    let wait = do
          needQuit <- atomically $ takeTMVar shutdownSignal
          unless needQuit wait
          -- If invoked from a web request, we cannot actually quit because the
          -- service daemon will restart us. This is why the postSubjectsR
          -- handler puts in False but real POSIX signal handlers put in True.
          -- The recursive wait is necessary because, what if we first receive a
          -- web shutdown request and then a POSIX signal?

    -- logger and logging middleware
    loggerSet' <- FastLogger.newStdoutLoggerSet FastLogger.defaultBufSize
    (getter, _) <- WaiLogger.clockDateCacher
    let logger = YT.Logger loggerSet' getter
    logWare <- RequestLogger.mkRequestLogger def {
      RequestLogger.destination = RequestLogger.Logger loggerSet',
      RequestLogger.outputFormat = RequestLogger.Apache RequestLogger.FromHeader
      }

    -- acid state
    bracket (Acid.openLocalState def) ((>> wait) . Acid.createCheckpointAndClose) $ \acid -> do

      let site = LabDeclarationApp {
            getStatic = eStatic,
            getAcid = acid,
            getNotifyChan = notifyChan,
            getHttpManager = httpManager,
            getAsyncQueue = asyncQueue,
            getCanBeShutdown = canBeShutDown,
            getShutdownSignal = shutdownSignal,
            getGoogleCredentials = (googleClientId, googleClientSecret),
            getApproot = approot,
            isDevelopment = isDevelopment
            }
      let waiAppPlain = toWaiAppPlain site

      -- launch pdfServiceThread
      forkIO $ pdfServiceThread isDevelopment acid httpManager lualatex rendererTempDir notifyChan asyncQueue canBeShutDown logger

      let waiApp = logWare . defaultMiddlewaresNoLogging <$> waiAppPlain
      t <- async $ waiApp >>= Warp.runSettings (setSettings Warp.defaultSettings)
      atomically $ readTMVar shutdownSignal
      putStrLn "****** WILL SHUTDOWN AFTER 2 SEC ******"
      threadDelay 2000000
      putStrLn "****** WILL SHUTDOWN IMMEDIATELY ******"
      cancel t
