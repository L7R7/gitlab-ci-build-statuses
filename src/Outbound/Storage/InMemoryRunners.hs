{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Outbound.Storage.InMemoryRunners (initStorage, runnersJobsApiToIO) where

import Data.Time (getCurrentTime)
import Polysemy
import Relude
import Core.Runners

initStorage :: IO (IORef RunnersJobs)
initStorage = newIORef NoSuccessfulUpdateYet

runnersJobsApiToIO :: (Member (Embed IO) r) => IORef RunnersJobs -> InterpreterFor RunnersJobsApi r
runnersJobsApiToIO ioRef = interpret $ \case
  GetJobs -> readIORef ioRef
  SetJobs results -> embed $ do
    updateTime <- getCurrentTime
    let res = RunnersJobs (updateTime, results)
    atomicWriteIORef ioRef res
