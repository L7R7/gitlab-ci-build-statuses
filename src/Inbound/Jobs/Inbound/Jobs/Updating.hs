{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Inbound.Jobs.Inbound.Jobs.Updating
  ( updateStatusesRegularly,
    UpdateJobDurationHistogram,
    HasDataUpdateInterval (..),
    HasUpdateJobDurationHistogram (..),
  )
where

import Core.Lib (DataUpdateIntervalSeconds (..), HasBuildStatuses (..), HasGetPipelines (..), HasGetProjects (..), updateStatuses)
import Katip
import Prometheus (Histogram, MonadMonitor, observeDuration)
import RIO

updateStatusesRegularly :: (HasGetProjects env, HasGetPipelines env, HasDataUpdateInterval env, HasBuildStatuses env, HasUpdateJobDurationHistogram env, MonadMonitor (RIO env), KatipContext (RIO env)) => RIO env ()
updateStatusesRegularly =
  katipAddNamespace "update" $ do
    updateInterval <- view dataUpdateIntervalL
    forever $ do
      updateWithDurationObservation
      threadDelay $ calculateDelay updateInterval

updateWithDurationObservation :: (HasGetProjects env, HasGetPipelines env, HasBuildStatuses env, HasUpdateJobDurationHistogram env, MonadMonitor (RIO env), KatipContext (RIO env)) => RIO env ()
updateWithDurationObservation = do
  histogram <- view hasUpdateJobDurationHistogramL
  observeDuration histogram $ do
    logLocM InfoS "updating build statuses"
    results <- updateStatuses
    katipAddContext (sl "numResults" $ show $ length results) $ logLocM InfoS "Done updating"

calculateDelay :: DataUpdateIntervalSeconds -> Int
calculateDelay (DataUpdateIntervalSeconds mins) = mins * oneSecond

oneSecond :: Int
oneSecond = 1000000

type UpdateJobDurationHistogram = Histogram

class HasDataUpdateInterval env where
  dataUpdateIntervalL :: SimpleGetter env DataUpdateIntervalSeconds

class HasUpdateJobDurationHistogram env where
  hasUpdateJobDurationHistogramL :: SimpleGetter env UpdateJobDurationHistogram
