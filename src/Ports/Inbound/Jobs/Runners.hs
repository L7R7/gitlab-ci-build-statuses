{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Ports.Inbound.Jobs.Runners
  ( updateRunnersJobsRegularly,
  )
where

import Core.BuildStatuses (Project)
import Core.Effects (Logger, ParTraverse, addContext, addNamespace, logDebug)
import Core.Runners
import Core.Shared
import Metrics.Metrics
import Polysemy
import qualified Polysemy.Reader as R
import Polysemy.Time (Seconds (Seconds), Time)
import qualified Polysemy.Time as Time
import Relude
import UseCases.Runners (updateRunnersJobs)

updateRunnersJobsRegularly ::
  ( Member DurationObservation r,
    Member RunnersApi r,
    Member RunnersJobsApi r,
    Member Logger r,
    Member (Time t d) r,
    Member ParTraverse r,
    Member (R.Reader (Id Group)) r,
    Member (R.Reader DataUpdateIntervalSeconds) r,
    Member (R.Reader [Id Project]) r
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
    Member (R.Reader (Id Group)) r,
    Member (R.Reader [Id Project]) r
  ) =>
  Sem r ()
updateWithDurationObservation =
  observeDuration "runners" $ do
    logDebug "updating runnersJobs"
    results <- updateRunnersJobs
    addContext "numResults" (length results) $ logDebug "Done updating"
