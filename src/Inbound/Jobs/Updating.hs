{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Inbound.Jobs.Updating
  ( updateStatusesRegularly,
  )
where

import Core.Effects (Logger, ParTraverse, addContext, addNamespace, logDebug, logInfo)
import Core.Lib (BuildStatusesApi, DataUpdateIntervalSeconds (..), Group, Id, PipelinesApi, ProjectsApi)
import Core.UseCases.Lib (updateStatuses)
import Metrics.Metrics
import Polysemy
import Polysemy.Time (Seconds (Seconds), Time)
import qualified Polysemy.Time as Time
import Relude

updateStatusesRegularly :: (Member DurationObservation r, Member ProjectsApi r, Member PipelinesApi r, Member BuildStatusesApi r, Member Logger r, Member (Time t d) r, Member ParTraverse r) => Id Group -> DataUpdateIntervalSeconds -> Sem r ()
updateStatusesRegularly groupId (DataUpdateIntervalSeconds updateInterval) =
  addNamespace "update" $ pass <$> infinitely (updateWithDurationObservation groupId >> Time.sleep (Seconds (fromIntegral updateInterval)))

updateWithDurationObservation :: (Member DurationObservation r, Member ProjectsApi r, Member PipelinesApi r, Member BuildStatusesApi r, Member Logger r, Member ParTraverse r) => Id Group -> Sem r ()
updateWithDurationObservation groupId =
  observeDuration $ do
    logDebug "updating build statuses"
    results <- updateStatuses groupId
    addContext "numResults" (length results) $ logInfo "Done updating"
