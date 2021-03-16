{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Metrics.Metrics
  ( OutgoingHttpRequestsHistogram,
    Metrics (..),
    registerMetrics,
    updateMetricsRegularly,
    updatePipelinesOverviewMetric,
    metricsApiToIO,
    VectorWithLabel (..),
    DurationObservation,
    observeDuration,
    observeDurationToIO,
  )
where

import Core.Effects (Delay, delaySeconds)
import Core.Lib (BuildStatus, BuildStatuses (..), BuildStatusesApi, Result (..), getStatuses, isHealthy)
import Data.List (partition)
import Data.List.Extra (enumerate)
import Data.Map hiding (partition)
import Data.Text (toLower)
import GHC.Clock (getMonotonicTime)
import Polysemy
import Prometheus hiding (observeDuration)
import Prometheus.Metric.GHC
import Relude

registerMetrics :: IO Metrics
registerMetrics = do
  _ <- registerGhcMetrics
  registerAppMetrics

registerGhcMetrics :: IO GHCMetrics
registerGhcMetrics = register ghcMetrics

registerAppMetrics :: IO Metrics
registerAppMetrics = Metrics <$> registerPipelinesOverviewMetric <*> registerOutgoingHttpRequestsHistogram <*> registerUpdateJobDurationHistogram

registerPipelinesOverviewMetric :: IO PipelinesOverviewGauge
registerPipelinesOverviewMetric =
  register $
    vector "build_status" $
      gauge (Info "build_pipelines_by_status_gauge" "Gauge that indicates the count of the pipeline statuses grouped by their result")

registerOutgoingHttpRequestsHistogram :: IO OutgoingHttpRequestsHistogram
registerOutgoingHttpRequestsHistogram = register $ vector "path" $ histogram (Info "outgoing_http_requests_histogram" "Histogram indicating how long outgoing HTTP request durations") defaultBuckets

registerUpdateJobDurationHistogram :: IO UpdateJobDurationHistogram
registerUpdateJobDurationHistogram = register $ histogram (Info "update_job_duration_histogram" "Histogram indicating how long the update job took") (exponentialBuckets 3 1.5 10)

type PipelinesOverviewGauge = Vector Label1 Gauge

type OutgoingHttpRequestsHistogram = Vector Label1 Histogram

type UpdateJobDurationHistogram = Histogram

data Metrics = Metrics
  { currentPipelinesOverview :: !PipelinesOverviewGauge,
    outgoingHttpRequestsHistogram :: !OutgoingHttpRequestsHistogram,
    updateJobDurationHistogram :: !UpdateJobDurationHistogram
  }

data DurationObservation m a where
  ObserveDuration :: m b -> DurationObservation m b

makeSem ''DurationObservation

observeDurationToIO :: (Member (Embed IO) r) => UpdateJobDurationHistogram -> Sem (DurationObservation ': r) a -> Sem r a
observeDurationToIO jobDurationHistogram = interpretH $ \case
  ObserveDuration fb -> do
    start <- liftIO getMonotonicTime
    a <- runT fb
    res <- raise $ observeDurationToIO jobDurationHistogram a
    end <- liftIO getMonotonicTime
    let !timeTaken = end - start
    embed (observe jobDurationHistogram timeTaken :: IO ())
    pure res

data MetricsApi m a where
  UpdatePipelinesOverviewMetric :: BuildStatuses -> MetricsApi m ()
  UpdateHealthy :: Int -> MetricsApi m ()
  UpdateUnhealthy :: Int -> MetricsApi m ()

makeSem ''MetricsApi

metricsApiToIO :: (Member (Embed IO) r) => Metrics -> Sem (MetricsApi ': r) a -> Sem r a
metricsApiToIO Metrics {..} = interpret $ \case
  UpdatePipelinesOverviewMetric buildStatuses -> embed $ updatePipelinesOverviewMetricIO currentPipelinesOverview buildStatuses
  UpdateHealthy healthyCount -> embed (withLabel currentPipelinesOverview "healthy" (`setGauge` fromIntegral healthyCount) :: IO ())
  UpdateUnhealthy unhealthyCount -> embed (withLabel currentPipelinesOverview "unhealthy" (`setGauge` fromIntegral unhealthyCount) :: IO ())

updatePipelinesOverviewMetricIO :: PipelinesOverviewGauge -> BuildStatuses -> IO ()
updatePipelinesOverviewMetricIO _ NoSuccessfulUpdateYet = pass
updatePipelinesOverviewMetricIO overviewGauge (Statuses (_, results)) = traverse_ (updateSingle overviewGauge) (Data.Map.toList (countByBuildStatus results))

updateSingle :: PipelinesOverviewGauge -> (BuildStatus, Double) -> IO ()
updateSingle overviewGauge (status, count) = withLabel overviewGauge ((toLower . show) status) (`setGauge` count)

countByBuildStatus :: [Result] -> Map BuildStatus Double
countByBuildStatus results = countOccurrences buildStatus results `union` resetValues

resetValues :: Map BuildStatus Double
resetValues = Data.Map.fromList $ (,0) <$> enumerate

updateMetricsRegularly :: (Member BuildStatusesApi r, Member MetricsApi r, Member Delay r) => Sem r ()
updateMetricsRegularly = pass <$> infinitely (updateMetrics >> delaySeconds 10)

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

data VectorWithLabel l m = VectorWithLabel (Vector l m) l

instance (Label l, Observer m) => Observer (VectorWithLabel l m) where
  observe (VectorWithLabel vctr label) value = withLabel vctr label f
    where
      f metric = observe metric value
