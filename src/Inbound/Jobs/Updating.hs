{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Inbound.Jobs.Updating
  ( updateStatusesRegularly,
  )
where

import Core.Effects (Delay, Logger, ParTraverse, addContext, addNamespace, delaySeconds, logDebug, logInfo)
import Core.Lib (BuildStatusesApi, DataUpdateIntervalSeconds (..), Group, Id, PipelinesApi, ProjectsApi)
import Metrics.Metrics
import Polysemy
import Relude
import Core.UseCases.Lib (updateStatuses)

updateStatusesRegularly :: (Member DurationObservation r, Member ProjectsApi r, Member PipelinesApi r, Member BuildStatusesApi r, Member Logger r, Member Delay r, Member ParTraverse r) => Id Group -> DataUpdateIntervalSeconds -> Sem r ()
updateStatusesRegularly groupId (DataUpdateIntervalSeconds updateInterval) =
  addNamespace "update" $ pass <$> infinitely (updateWithDurationObservation groupId >> delaySeconds updateInterval)

updateWithDurationObservation :: (Member DurationObservation r, Member ProjectsApi r, Member PipelinesApi r, Member BuildStatusesApi r, Member Logger r, Member ParTraverse r) => Id Group -> Sem r ()
updateWithDurationObservation groupId =
  observeDuration $ do
    logDebug "updating build statuses"
    results <- updateStatuses groupId
    addContext "numResults" (length results) $ logInfo "Done updating"
