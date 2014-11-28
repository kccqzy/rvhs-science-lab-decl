{-# LANGUAGE OverloadedStrings #-}
module LabDecl.Main where

import Control.Applicative
import Control.Monad
import Control.Error
import Control.Exception
import Control.Concurrent.STM
import Data.String (fromString)
import Data.Default (def)
import qualified Data.ByteString.Char8 as C
import qualified Data.Attoparsec.ByteString.Char8 as PC
import qualified Data.Acid.Local as Acid
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import System.Environment (lookupEnv)
import System.IO.Temp (withTempDirectory, createTempDirectory)
import System.IO
import System.Exit
import Yesod.Core.Dispatch (toWaiApp)

import LabDecl.Handlers

main = do
  -- no, we do not use /tmp, you must pass the temp dir explicitly
  tmpdir <- join $ maybe errNoTempDir return <$> lookupEnv "TMPDIR"
  withTempDirectory tmpdir "labdecld" $ \dir -> do

  -- warp configuration from environment variables
  host <- liftM fromString <$> lookupEnv "HOST"
  port <- (>>= either (const Nothing) Just . PC.parseOnly PC.decimal . C.pack) <$> lookupEnv "PORT"
  let setSettings = foldr (.) (Warp.setServerName "Warp") $ catMaybes [Warp.setPort <$> port, Warp.setHost <$> host]

  -- queues and channels
  notifyChan <- atomically newBroadcastTChan

  -- acid state
  bracket acidBegin acidFinally $ \acid ->
    toWaiApp (LabDeclarationApp acid notifyChan) >>= Warp.runSettings (setSettings Warp.defaultSettings)
  where acidBegin = Acid.openLocalState def
        acidFinally = Acid.createCheckpointAndClose
        errNoTempDir = do
          hPutStrLn stderr "No temporary directory specified; pass TMPDIR as environment variable"
          exitFailure
