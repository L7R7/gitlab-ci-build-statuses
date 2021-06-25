{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Inbound.Jobs.Runners
  ( updateRunnersJobsRegularly,
  )
where

import Core.BuildStatuses (Project)
import Core.Effects (Logger, ParTraverse, addContext, addNamespace, logDebug, logInfo)
import Core.Runners
import Core.Shared
import Metrics.Metrics
import Polysemy
import Polysemy.Time (Seconds (Seconds), Time)
import qualified Polysemy.Time as Time
import Relude
import UseCases.Runners (updateRunnersJobs)

updateRunnersJobsRegularly :: (Member DurationObservation r, Member RunnersApi r, Member RunnersJobsApi r, Member Logger r, Member (Time t d) r, Member ParTraverse r) => Id Group -> DataUpdateIntervalSeconds -> [Id Project] -> Sem r ()
updateRunnersJobsRegularly groupId (DataUpdateIntervalSeconds updateInterval) excludeList =
  addNamespace "update-runners-jobs" $ pass <$> infinitely (updateWithDurationObservation groupId excludeList >> Time.sleep (Seconds (fromIntegral updateInterval)))

updateWithDurationObservation :: (Member DurationObservation r, Member RunnersApi r, Member RunnersJobsApi r, Member Logger r, Member ParTraverse r) => Id Group -> [Id Project] -> Sem r ()
updateWithDurationObservation groupId excludeList =
  observeDuration "runners" $ do
    logDebug "updating runnersJobs"
    results <- updateRunnersJobs groupId excludeList
    addContext "numResults" (length results) $ logInfo "Done updating"
