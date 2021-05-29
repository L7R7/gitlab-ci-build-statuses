{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Metrics.Metrics
  ( OutgoingHttpRequestsHistogram,
    Metrics (..),
    registerMetrics,
    updateMetricsRegularly,
    updatePipelinesOverviewMetric,
    metricsApiToIO,
    DurationObservation,
    observeDuration,
    observeDurationToIO,
  )
where

import Core.Lib (BuildStatus, BuildStatuses (..), BuildStatusesApi, Group, Id (..), Result (..), getStatuses, isHealthy)
import Data.List (partition)
import Data.List.Extra (enumerate)
import Data.Map hiding (partition)
import Data.Text (toLower)
import GHC.Clock (getMonotonicTime)
import Metrics.PrometheusUtils (VectorWithLabel (VectorWithLabel))
import Polysemy
import Polysemy.Time (Seconds (Seconds), Time)
import qualified Polysemy.Time as Time
import Prometheus hiding (observeDuration)
import Prometheus.Metric.GHC
import Relude

registerMetrics :: IO Metrics
registerMetrics = registerGhcMetrics >> registerAppMetrics

registerGhcMetrics :: IO GHCMetrics
registerGhcMetrics = register ghcMetrics

registerAppMetrics :: IO Metrics
registerAppMetrics = Metrics <$> registerPipelinesOverviewMetric <*> registerOutgoingHttpRequestsHistogram <*> registerUpdateJobDurationHistogram

registerPipelinesOverviewMetric :: IO PipelinesOverviewGauge
registerPipelinesOverviewMetric =
  register $
    vector ("group_id", "build_status") $
      gauge (Info "build_pipelines_by_status_gauge" "Gauge that indicates the count of the pipeline statuses grouped by their result")

registerOutgoingHttpRequestsHistogram :: IO OutgoingHttpRequestsHistogram
registerOutgoingHttpRequestsHistogram = register $ vector ("group_id", "path") $ histogram (Info "outgoing_http_requests_histogram" "Histogram indicating how long outgoing HTTP request durations") defaultBuckets

registerUpdateJobDurationHistogram :: IO UpdateJobDurationHistogram
registerUpdateJobDurationHistogram = register $ vector "group_id" $ histogram (Info "update_job_duration_histogram" "Histogram indicating how long the update job took") (exponentialBuckets 0.5 1.25 20)

type PipelinesOverviewGauge = Vector Label2 Gauge

type OutgoingHttpRequestsHistogram = Vector Label2 Histogram

type UpdateJobDurationHistogram = Vector Label1 Histogram

data Metrics = Metrics
  { currentPipelinesOverview :: !PipelinesOverviewGauge,
    outgoingHttpRequestsHistogram :: !OutgoingHttpRequestsHistogram,
    updateJobDurationHistogram :: !UpdateJobDurationHistogram
  }

data DurationObservation m a where
  ObserveDuration :: m b -> DurationObservation m b

makeSem ''DurationObservation

observeDurationToIO :: (Member (Embed IO) r) => Id Group -> UpdateJobDurationHistogram -> InterpreterFor DurationObservation r
observeDurationToIO groupId@(Id gId) jobDurationHistogram = interpretH $ \case
  ObserveDuration fb -> do
    start <- liftIO getMonotonicTime
    a <- runT fb
    res <- raise $ observeDurationToIO groupId jobDurationHistogram a
    end <- liftIO getMonotonicTime
    let !timeTaken = end - start
    embed (observe (VectorWithLabel jobDurationHistogram (show gId)) timeTaken :: IO ())
    pure res

data MetricsApi m a where
  UpdatePipelinesOverviewMetric :: BuildStatuses -> MetricsApi m ()
  UpdateHealthy :: Int -> MetricsApi m ()
  UpdateUnhealthy :: Int -> MetricsApi m ()

makeSem ''MetricsApi

metricsApiToIO :: (Member (Embed IO) r) => Id Group -> Metrics -> InterpreterFor MetricsApi r
metricsApiToIO groupId@(Id gId) Metrics {..} = interpret $ \case
  UpdatePipelinesOverviewMetric buildStatuses -> embed $ updatePipelinesOverviewMetricIO groupId currentPipelinesOverview buildStatuses
  UpdateHealthy healthyCount -> embed (withLabel currentPipelinesOverview (show gId, "healthy") (`setGauge` fromIntegral healthyCount) :: IO ())
  UpdateUnhealthy unhealthyCount -> embed (withLabel currentPipelinesOverview (show gId, "unhealthy") (`setGauge` fromIntegral unhealthyCount) :: IO ())

updatePipelinesOverviewMetricIO :: Id Group -> PipelinesOverviewGauge -> BuildStatuses -> IO ()
updatePipelinesOverviewMetricIO _ _ NoSuccessfulUpdateYet = pass
updatePipelinesOverviewMetricIO groupId overviewGauge (Statuses (_, results)) = traverse_ (updateSingle groupId overviewGauge) (Data.Map.toList (countByBuildStatus results))

updateSingle :: Id Group -> PipelinesOverviewGauge -> (BuildStatus, Double) -> IO ()
updateSingle (Id groupId) overviewGauge (status, count) = withLabel overviewGauge (show groupId, (toLower . show) status) (`setGauge` count)

countByBuildStatus :: [Result] -> Map BuildStatus Double
countByBuildStatus results = countOccurrences buildStatus results `union` resetValues

resetValues :: Map BuildStatus Double
resetValues = Data.Map.fromList $ (,0) <$> enumerate

updateMetricsRegularly :: (Member BuildStatusesApi r, Member MetricsApi r, Member (Time t d) r) => Sem r ()
updateMetricsRegularly = pass <$> infinitely (updateMetrics >> Time.sleep (Seconds 10))

updateMetrics :: (Member BuildStatusesApi r, Member MetricsApi r) => Sem r ()
updateMetrics = do
  statuses <- getStatuses
  updatePipelinesOverviewMetric statuses
  updateHealthyUnhealthy $ resultsFromBuildStatuses statuses

updateHealthyUnhealthy :: (Member MetricsApi r) => [Result] -> Sem r ()
updateHealthyUnhealthy results = do
  let (healthyCount, unhealthyCount) = bimap length length (partition (isHealthy . buildStatus) results)
  updateHealthy healthyCount
  updateUnhealthy unhealthyCount

resultsFromBuildStatuses :: BuildStatuses -> [Result]
resultsFromBuildStatuses (Statuses (_, res)) = res
resultsFromBuildStatuses NoSuccessfulUpdateYet = []

countOccurrences :: (Ord k, Num a) => (t -> k) -> [t] -> Map k a
countOccurrences f xs = fromListWith (+) [(f x, 1) | x <- xs]
