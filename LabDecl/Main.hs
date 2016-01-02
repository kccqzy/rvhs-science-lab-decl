{-# LANGUAGE OverloadedStrings #-}
module LabDecl.Main where

import Control.Applicative
import Control.Monad
import Control.Error
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Data.String (fromString)
import Data.Default (def)
import qualified Data.ByteString.Char8 as C
import qualified Data.Attoparsec.ByteString.Char8 as PC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Acid.Local as Acid
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import System.Environment (getEnv, lookupEnv)
import System.IO.Temp (withTempDirectory, createTempDirectory)
import System.IO
import System.Exit
import Yesod.Core.Dispatch (toWaiApp)

import LabDecl.Handlers
import LabDecl.AsyncServ

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

    -- queues and channels
    notifyChan <- atomically newBroadcastTChan
    asyncQueue <- atomically newTQueue

    rendererTempDir <- createTempDirectory dir "renderer"

    -- acid state
    bracket (Acid.openLocalState def) Acid.createCheckpointAndClose $ \acid -> do
      forkIO $ asyncMain isDevelopment acid httpManager lualatex rendererTempDir notifyChan asyncQueue
      toWaiApp LabDeclarationApp { getStatic = eStatic,
                                   getAcid = acid,
                                   getNotifyChan = notifyChan,
                                   getHttpManager = httpManager,
                                   getAsyncQueue = asyncQueue,
                                   getGoogleCredentials = (googleClientId, googleClientSecret),
                                   getApproot = approot,
                                   isDevelopment = isDevelopment
                                } >>= Warp.runSettings (setSettings Warp.defaultSettings)
