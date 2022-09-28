{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Ports.Outbound.Storage.WaitingJobs.InMemory (initStorage, waitingJobsApiToIO) where

import Core.Jobs
import Data.Time (getCurrentTime)
import Polysemy
import qualified Polysemy.Reader as R
import Relude

initStorage :: IO (IORef WaitingJobs)
initStorage = newIORef NoSuccessfulUpdateYet

waitingJobsApiToIO :: (Member (Embed IO) r, Member (R.Reader (IORef WaitingJobs)) r) => InterpreterFor WaitingJobsApi r
waitingJobsApiToIO = interpret $ \case
  GetJobs -> R.ask >>= readIORef
  SetJobs results -> do
    ioRef <- R.ask
    embed $ do
      updateTime <- getCurrentTime
      let res = WaitingJobs (updateTime, results)
      atomicWriteIORef ioRef res
