{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Ports.Outbound.Storage.BuildStatuses.InMemory (initStorage, buildStatusesApiToIO) where

import Core.BuildStatuses (BuildStatuses (..), BuildStatusesApi (..))
import Data.Time (getCurrentTime)
import Polysemy
import Polysemy.Reader qualified as R
import Relude

initStorage :: IO (IORef BuildStatuses)
initStorage = newIORef NoSuccessfulUpdateYet

buildStatusesApiToIO :: ((Member (Embed IO) r), Member (R.Reader (IORef BuildStatuses)) r) => InterpreterFor BuildStatusesApi r
buildStatusesApiToIO = interpret $ \case
  GetStatuses -> R.ask >>= readIORef
  SetStatuses results -> do
    ioRef <- R.ask
    embed $ do
      updateTime <- getCurrentTime
      let res = Statuses (updateTime, results)
      atomicWriteIORef ioRef res
