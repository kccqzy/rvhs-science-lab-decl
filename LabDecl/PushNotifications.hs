{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module LabDecl.PushNotifications where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.Async.Lifted
import Control.Concurrent
import Data.Monoid
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Yesod.Core
import Yesod.WebSockets (webSockets, sendTextData, receiveDataMessageE, sendPing)

-- | An object specifying options for the connection.
data TimeoutSpec = TimeoutSpec {
  retryTimeoutMilliseconds :: Int,
  keepAliveTimeoutMicroseconds :: Int
  } deriving (Show)

-- | Begin an SSE response. This will check for a suitable Accept header and
-- continue the connection only if it is set to 'text/event-stream'. It will
-- then fork a thread to keep track of keepAlive, and sends random comments at
-- this interval. It will also send the requested retryTimeout to the client.
beginSSE :: TimeoutSpec -> TQueue CL.ByteString -> HandlerT site IO TypedContent -> HandlerT site IO TypedContent
beginSSE TimeoutSpec{..} tqueue fallbackHandler = do
  accepts <- reqAccept <$> getRequest
  let acceptHasSSE = "text/event-stream" `elem` accepts
  if not acceptHasSSE
    then fallbackHandler
    else respondSource "text/event-stream" $ do
    let retryIndication = "retry: " <> C.pack (show retryTimeoutMilliseconds) <> "\n\n"
    sendChunkBS retryIndication
    sendFlush

    forever $ do
      r <- liftIO (race (threadDelay keepAliveTimeoutMicroseconds) (atomically $ readTQueue tqueue))
      case r of
        Left _ -> sendChunkBS ": Mr Chow is not a nice teacher; quite abusive in fact.\n\n"
        Right t -> sendChunkLBS ("data: " <> CL.intercalate "\ndata: " (CL.split '\n' t) <> "\n\n")
      sendFlush

-- | Begin a WebSocket session. This mostly offloads actual work to
-- Yesod.WebSockets but keeps the same interface as `beginSSE`.
beginWS :: TimeoutSpec -> TQueue CL.ByteString -> HandlerT site IO a -> HandlerT site IO a
beginWS TimeoutSpec{..} tqueue fallbackHandler = do
  webSockets . forever $ do
    r <- liftIO $ race (threadDelay keepAliveTimeoutMicroseconds) (atomically $ readTQueue tqueue)
    case r of
      Left _ -> sendPing C.empty >> void receiveDataMessageE
      Right t -> sendTextData t
  fallbackHandler
