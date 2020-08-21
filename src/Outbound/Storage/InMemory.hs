{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Outbound.Storage.InMemory () where

import App
import Core.Lib (BuildStatuses (..), HasBuildStatuses (..), Result)
import Data.Time (getCurrentTime)
import RIO

instance HasBuildStatuses App where
  getStatuses :: RIO App BuildStatuses
  getStatuses = view readBuildStatuses >>= readIORef
  setStatuses :: [Result] -> RIO App BuildStatuses
  setStatuses results = do
    store <- view readBuildStatuses
    updateTime <- liftIO getCurrentTime
    let res = Statuses (updateTime, results)
    liftIO $ atomicModifyIORef' store (const (res, res))

readBuildStatuses :: SimpleGetter App (IORef BuildStatuses)
readBuildStatuses = to statuses
