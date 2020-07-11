{-# LANGUAGE NoImplicitPrelude #-}

module Outbound.Storage.InMemory where

import App
import Core.Lib (HasBuildStatuses (..), Result)
import Data.Time (UTCTime, getCurrentTime)
import RIO

instance HasBuildStatuses App where
  getStatuses = view readBuildStatuses >>= readIORef
  setStatuses results = do
    store <- view readBuildStatuses
    updateTime <- liftIO getCurrentTime
    liftIO $ atomicModifyIORef' store (const ((Just updateTime, results), (updateTime, results)))

readBuildStatuses :: Lens' App (IORef (Maybe UTCTime, [Result]))
readBuildStatuses = lens statuses (\app st -> app {statuses = st})
