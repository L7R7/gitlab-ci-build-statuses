{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Ports.Inbound.Jobs.WaitingJobs
  ( updateWaitingJobsRegularly,
  )
where

import Core.BuildStatuses (ProjectsWithoutExcludesApi)
import Core.Effects (Logger, ParTraverse, addContext, addNamespace, logDebug)
import Core.Jobs (JobsApi, WaitingJobsApi)
import Core.Shared
import Metrics.Metrics
import Polysemy
import qualified Polysemy.Reader as R
import Polysemy.Time (Seconds (Seconds), Time)
import qualified Polysemy.Time as Time
import Relude
import UseCases.WaitingJobs (updateWaitingJobs)

updateWaitingJobsRegularly ::
  (Member ProjectsWithoutExcludesApi r, Member JobsApi r, Member WaitingJobsApi r, Member DurationObservation r, Member Logger r, Member (Time t d) r, Member ParTraverse r, Member (R.Reader (Id Group)) r, Member (R.Reader DataUpdateIntervalSeconds) r) =>
  Sem r ()
updateWaitingJobsRegularly = do
  (DataUpdateIntervalSeconds updateInterval) <- R.ask
  addNamespace "update-waiting-jobs" $ pass <$> infinitely (updateWithDurationObservation >> Time.sleep (Seconds (fromIntegral updateInterval)))

updateWithDurationObservation :: (Member ProjectsWithoutExcludesApi r, Member JobsApi r, Member WaitingJobsApi r, Member DurationObservation r, Member Logger r, Member ParTraverse r, Member (R.Reader (Id Group)) r) => Sem r ()
updateWithDurationObservation =
  observeDuration "waiting-jobs" $ do
    logDebug "updating waitingJobs"
    results <- updateWaitingJobs
    addContext "numResults" (length results) $ logDebug "Done updating"
