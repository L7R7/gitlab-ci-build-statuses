{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Outbound.Storage.Runners.InMemory (initStorage, runnersJobsApiToIO) where

import Core.Runners
import Data.Time (getCurrentTime)
import Polysemy
import qualified Polysemy.Reader as R
import Relude

initStorage :: IO (IORef RunnersJobs)
initStorage = newIORef NoSuccessfulUpdateYet

runnersJobsApiToIO :: (Member (Embed IO) r, Member (R.Reader (IORef RunnersJobs)) r) => InterpreterFor RunnersJobsApi r
runnersJobsApiToIO = interpret $ \case
  GetJobs -> R.ask >>= readIORef
  SetJobs results -> do
    ioRef <- R.ask
    embed $ do
      updateTime <- getCurrentTime
      let res = RunnersJobs (updateTime, results)
      atomicWriteIORef ioRef res
