{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Inbound.Jobs.Inbound.Jobs.Updating (updateStatusesRegularly, UpdateJobDurationHistogram, HasDataUpdateInterval (..), HasUpdateJobDurationHistogram (..)) where

import Core.Lib (DataUpdateIntervalMinutes (..), HasBuildStatuses (..), HasGetPipelines (..), HasGetProjects (..), updateStatuses)
import Katip
import Prometheus (Histogram, MonadMonitor, observeDuration)
import RIO

updateStatusesRegularly :: (HasGetProjects env, HasGetPipelines env, HasDataUpdateInterval env, HasBuildStatuses env, HasUpdateJobDurationHistogram env, MonadMonitor (RIO env), KatipContext (RIO env)) => RIO env ()
updateStatusesRegularly =
  katipAddNamespace "update" $ do
    histogram <- view hasUpdateJobDurationHistogramL
    updateInterval <- view dataUpdateIntervalL
    forever $ do
      observeDuration histogram $ do
        logLocM InfoS "updating build statuses"
        results <- updateStatuses
        katipAddContext (sl "numResults" $ show $ length results) $ logLocM InfoS "Done updating"
      threadDelay $ calculateDelay updateInterval

calculateDelay :: DataUpdateIntervalMinutes -> Int
calculateDelay (DataUpdateIntervalMinutes mins) = mins * 60 * oneSecond

oneSecond :: Int
oneSecond = 1000000

type UpdateJobDurationHistogram = Histogram

class HasDataUpdateInterval env where
  dataUpdateIntervalL :: Lens' env DataUpdateIntervalMinutes

class HasUpdateJobDurationHistogram env where
  hasUpdateJobDurationHistogramL :: Lens' env UpdateJobDurationHistogram
