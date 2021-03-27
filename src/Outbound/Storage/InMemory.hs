{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Outbound.Storage.InMemory (initStorage, buildStatusesApiToIO) where

import Core.Lib (BuildStatuses (..), BuildStatusesApi (..))
import Data.Time (getCurrentTime)
import Polysemy
import Relude

initStorage :: IO (IORef BuildStatuses)
initStorage = newIORef NoSuccessfulUpdateYet

buildStatusesApiToIO :: (Member (Embed IO) r) => IORef BuildStatuses -> InterpreterFor BuildStatusesApi r
buildStatusesApiToIO ioRef = interpret $ \case
  GetStatuses -> readIORef ioRef
  SetStatuses results -> embed $ do
    updateTime <- getCurrentTime
    let res = Statuses (updateTime, results)
    atomicModifyIORef' ioRef (const (res, res))
