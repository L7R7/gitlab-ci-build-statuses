{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Inbound.Jobs.Inbound.Jobs.Updating
  ( updateStatusesRegularly,
  )
where

import Core.Effects (Delay, Logger, ParTraverse, addContext, addNamespace, delaySeconds, logInfo)
import Core.Lib (BuildStatusesApi, DataUpdateIntervalSeconds (..), Group, Id, PipelinesApi, ProjectsApi, updateStatuses)
import Data.Coerce (coerce)
import Metrics.Metrics
import Polysemy
import RIO hiding (logInfo)

updateStatusesRegularly :: (Member DurationObservation r, Member ProjectsApi r, Member PipelinesApi r, Member BuildStatusesApi r, Member Logger r, Member Delay r, Member ParTraverse r) => Id Group -> DataUpdateIntervalSeconds -> Sem r ()
updateStatusesRegularly groupId updateInterval =
  addNamespace "update" $ do
    forever $ do
      updateWithDurationObservation groupId
      delaySeconds $ coerce updateInterval

updateWithDurationObservation :: (Member DurationObservation r, Member ProjectsApi r, Member PipelinesApi r, Member BuildStatusesApi r, Member Logger r, Member ParTraverse r) => Id Group -> Sem r ()
updateWithDurationObservation groupId =
  observeDuration $ do
    logInfo "updating build statuses"
    results <- updateStatuses groupId
    addContext "numResults" (show $ length results) $ logInfo "Done updating"
