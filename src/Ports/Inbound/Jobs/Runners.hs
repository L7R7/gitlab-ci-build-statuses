{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Ports.Inbound.Jobs.Runners
  ( updateRunnersJobsRegularly,
  )
where

import Config.Config (ProjectExcludeList)
import Core.Effects (Logger, ParTraverse, addContext, addNamespace, logDebug)
import Core.Runners
import Core.Shared
import Metrics.Metrics
import Polysemy
import Polysemy.Reader qualified as R
import Polysemy.Time (Seconds (Seconds), Time)
import Polysemy.Time qualified as Time
import Relude
import UseCases.Runners (updateRunnersJobs)

updateRunnersJobsRegularly ::
  ( Member DurationObservation r,
    Member RunnersApi r,
    Member RunnersJobsApi r,
    Member Logger r,
    Member (Time t d) r,
    Member ParTraverse r,
    Member (R.Reader (NonEmpty (Id Group))) r,
    Member (R.Reader DataUpdateIntervalSeconds) r,
    Member (R.Reader ProjectExcludeList) r
  ) =>
  Sem r ()
updateRunnersJobsRegularly = do
  (DataUpdateIntervalSeconds updateInterval) <- R.ask
  addNamespace "update-runners-jobs" $ pass <$> infinitely (updateWithDurationObservation >> Time.sleep (Seconds (fromIntegral updateInterval)))

updateWithDurationObservation ::
  ( Member DurationObservation r,
    Member RunnersApi r,
    Member RunnersJobsApi r,
    Member Logger r,
    Member ParTraverse r,
    Member (R.Reader (NonEmpty (Id Group))) r,
    Member (R.Reader ProjectExcludeList) r
  ) =>
  Sem r ()
updateWithDurationObservation =
  observeDuration "runners" $ do
    logDebug "updating runnersJobs"
    results <- updateRunnersJobs
    addContext "numResults" (length results) $ logDebug "Done updating"
