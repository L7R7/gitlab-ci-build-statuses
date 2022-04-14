{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Ports.Inbound.Jobs.BuildStatuses
  ( updateStatusesRegularly,
  )
where

import Core.BuildStatuses (BuildStatusesApi, PipelinesApi, ProjectsWithoutExcludesApi)
import Core.Effects (Logger, ParTraverse, addContext, addNamespace, logDebug)
import Core.Shared
import Metrics.Metrics
import Polysemy
import qualified Polysemy.Reader as R
import Polysemy.Time (Seconds (Seconds), Time)
import qualified Polysemy.Time as Time
import Relude
import UseCases.BuildStatuses (updateStatuses)

updateStatusesRegularly ::
  ( Member ProjectsWithoutExcludesApi r,
    Member DurationObservation r,
    Member PipelinesApi r,
    Member BuildStatusesApi r,
    Member Logger r,
    Member (Time t d) r,
    Member ParTraverse r,
    Member (R.Reader (Id Group)) r,
    Member (R.Reader DataUpdateIntervalSeconds) r
  ) =>
  Sem r ()
updateStatusesRegularly = do
  (DataUpdateIntervalSeconds updateInterval) <- R.ask
  addNamespace "update-build-statuses" $ pass <$> infinitely (updateWithDurationObservation >> Time.sleep (Seconds (fromIntegral updateInterval)))

updateWithDurationObservation ::
  ( Member ProjectsWithoutExcludesApi r,
    Member DurationObservation r,
    Member PipelinesApi r,
    Member BuildStatusesApi r,
    Member Logger r,
    Member ParTraverse r,
    Member (R.Reader (Id Group)) r
  ) =>
  Sem r ()
updateWithDurationObservation =
  observeDuration "projects" $ do
    logDebug "updating build statuses"
    results <- updateStatuses
    addContext "numResults" (length results) $ logDebug "Done updating"
