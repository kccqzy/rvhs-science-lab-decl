{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module LabDecl.AsyncQueue where

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import qualified Data.ByteString.Char8 as C
import Data.String
import Data.Monoid
import Data.Default
import System.Log.FastLogger
import qualified Yesod.Core.Types as YT

data InternalQueueTask = InternalQueueTask {
  taskName :: C.ByteString,
  previousAttemptCount :: Int,
  retryWaitIfFail :: Int,
  task :: IO ()
  }

instance Default InternalQueueTask where
  def = InternalQueueTask "" 0 3000000 (return ())

logShow :: Show a => a -> LogStr
logShow = fromString . show

maximumRetries :: Int
maximumRetries = 10

internalQueueMain :: YT.Logger -> TVar Bool -> TQueue InternalQueueTask -> IO ()
internalQueueMain logger canBeShutDown internalQueue = do
  numTasksToBeAdded <- atomically $ newTVar (0 :: Int)

  let logMsg a = do
        date <- toLogStr <$> YT.loggerDate logger
        pushLogStr (YT.loggerSet logger) $ "- - - [" <> date <> "] internalQueue: " <> a <> "\n"

  forever $ do
    -- Anything can happen here. The queue could be empty or nonempty; the pending task count can be zero or nonzero.
    atomically $ do
      isQueueEmpty <- isEmptyTQueue internalQueue
      hasPendingTasks <- (/= 0) <$> readTVar numTasksToBeAdded
      writeTVar canBeShutDown (isQueueEmpty && not hasPendingTasks)
    queueItem@InternalQueueTask{..} <- atomically $ do
      r <- readTQueue internalQueue
      -- Adding things to be queue and setting canBeShutDown to False should be
      -- in the same transaction. Setting it to False again here theoretically
      -- has no effect.
      writeTVar canBeShutDown False
      return r

    let thisAttemptCount = logShow (1 + previousAttemptCount)
    let taskNameLogStr = toLogStr taskName
    logMsg $ "Starting attempt " <> thisAttemptCount <> " of task \"" <> taskNameLogStr <> "\""
    op <- async (void task)
    result <- waitCatch op
    case result of
      Right () -> logMsg $ "Attempt " <> thisAttemptCount <> " of task \"" <> taskNameLogStr <> "\" succeeded" -- after this logging we are back to the beginning
      Left e -> do
        logMsg $ "Attempt " <> thisAttemptCount <> " of task \"" <> taskNameLogStr <> "\" failed (details = " <> logShow e <> ")"
        if previousAttemptCount == maximumRetries - 1
          then logMsg $ "Task\"" <> toLogStr taskName <> "\" has permanently failed after reaching maximumRetries"
          else do
          atomically $ modifyTVar numTasksToBeAdded (1+)
          void . forkIO $ do -- throw away this thread...
            logMsg $ "Will wait " <> fromString (show retryWaitIfFail) <> " microseconds before adding failed task \"" <> taskNameLogStr <> "\" back to queue"
            threadDelay retryWaitIfFail
            logMsg $ "Will add failed task \"" <> taskNameLogStr <> "\" back to queue"
            atomically $ do
              writeTVar canBeShutDown False -- No effect here. Should already be
                                            -- False.
              writeTQueue internalQueue $ queueItem { previousAttemptCount = previousAttemptCount + 1,
                                                      retryWaitIfFail = retryWaitIfFail `div` 4 * 5 } -- exponential backoff
              modifyTVar numTasksToBeAdded (\v -> v - 1)
