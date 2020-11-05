{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Outbound.Storage.InMemory (initStorage, buildStatusesApiToIO) where

import Core.Lib (BuildStatuses (..), BuildStatusesApi (..))
import Data.Time (getCurrentTime)
import Polysemy
import Relude

initStorage :: IO (IORef BuildStatuses)
initStorage = newIORef NoSuccessfulUpdateYet

buildStatusesApiToIO :: (Member (Embed IO) r) => IORef BuildStatuses -> Sem (BuildStatusesApi ': r) a -> Sem r a
buildStatusesApiToIO ioRef = interpret $ \case
  GetStatuses -> readIORef ioRef
  SetStatuses results -> embed $ do
    updateTime <- getCurrentTime
    let res = Statuses (updateTime, results)
    atomicModifyIORef' ioRef (const (res, res))
