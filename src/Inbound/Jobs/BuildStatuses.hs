{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Inbound.Jobs.BuildStatuses
  ( updateStatusesRegularly,
  )
where

import Core.BuildStatuses (BuildStatusesApi, PipelinesApi, Project, ProjectsApi)
import Core.Effects (Logger, ParTraverse, addContext, addNamespace, logDebug, logInfo)
import Core.Shared
import Metrics.Metrics
import Polysemy
import Polysemy.Time (Seconds (Seconds), Time)
import qualified Polysemy.Time as Time
import Relude
import UseCases.BuildStatuses (updateStatuses)

updateStatusesRegularly :: (Member DurationObservation r, Member ProjectsApi r, Member PipelinesApi r, Member BuildStatusesApi r, Member Logger r, Member (Time t d) r, Member ParTraverse r) => Id Group -> DataUpdateIntervalSeconds -> [Id Project] -> Sem r ()
updateStatusesRegularly groupId (DataUpdateIntervalSeconds updateInterval) excludeList =
  addNamespace "update-build-statuses" $ pass <$> infinitely (updateWithDurationObservation groupId excludeList >> Time.sleep (Seconds (fromIntegral updateInterval)))

updateWithDurationObservation :: (Member DurationObservation r, Member ProjectsApi r, Member PipelinesApi r, Member BuildStatusesApi r, Member Logger r, Member ParTraverse r) => Id Group -> [Id Project] -> Sem r ()
updateWithDurationObservation groupId excludeList =
  observeDuration $ do
    logDebug "updating build statuses"
    results <- updateStatuses groupId excludeList
    addContext "numResults" (length results) $ logInfo "Done updating"
