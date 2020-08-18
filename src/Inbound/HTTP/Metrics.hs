{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Inbound.HTTP.Metrics
  ( HasPipelinesOverviewGauge (..),
    HasOutgoingHttpRequestsHistogram (..),
    Metrics (..),
    registerAppMetrics,
    registerGhcMetrics,
    updateMetricsRegularly,
    updatePipelinesOverviewMetric,
    VectorWithLabel (..),
  )
where

import Core.Lib (BuildStatus, BuildStatuses (..), HasBuildStatuses, Result (..), getStatuses, isHealthy)
import Data.List (partition)
import Data.List.Extra (enumerate)
import Data.Map hiding (partition)
import Inbound.Jobs.Inbound.Jobs.Updating (UpdateJobDurationHistogram)
import Prometheus
import Prometheus.Metric.GHC
import RIO hiding (Vector, toList)
import RIO.Text (toLower)

registerGhcMetrics :: MonadIO m => m GHCMetrics
registerGhcMetrics = register ghcMetrics

type PipelinesOverviewGauge = Vector Label1 Gauge

type OutgoingHttpRequestsHistogram = Vector Label1 Histogram

data Metrics = Metrics
  { currentPipelinesOverview :: !PipelinesOverviewGauge,
    outgoingHttpRequestsHistogram :: !OutgoingHttpRequestsHistogram,
    updateJobDurationHistogram :: !UpdateJobDurationHistogram
  }

registerPipelinesOverviewMetric :: IO PipelinesOverviewGauge
registerPipelinesOverviewMetric =
  register $
    vector "build_status" $
      gauge (Info "build_pipelines_by_status_gauge" "Gauge that indicates the count of the pipeline statuses grouped by their result")

registerOutgoingHttpRequestsHistogram :: IO OutgoingHttpRequestsHistogram
registerOutgoingHttpRequestsHistogram = register $ vector "path" $ histogram (Info "outgoing_http_requests_histogram" "Histogram indicating how long outgoing HTTP request durations") defaultBuckets

registerUpdateJobDurationHistogram :: IO UpdateJobDurationHistogram
registerUpdateJobDurationHistogram = register $ histogram (Info "update_job_duration_histogram" "Histogram indicating how long the update job took") (exponentialBuckets 3 1.5 10)

registerAppMetrics :: IO Metrics
registerAppMetrics = Metrics <$> registerPipelinesOverviewMetric <*> registerOutgoingHttpRequestsHistogram <*> registerUpdateJobDurationHistogram

updatePipelinesOverviewMetric :: PipelinesOverviewGauge -> BuildStatuses -> IO ()
updatePipelinesOverviewMetric _ NoSuccessfulUpdateYet = pure ()
updatePipelinesOverviewMetric overviewGauge (Statuses (_, results)) =
  traverse_ (updateSingle overviewGauge) (toList (countByBuildStatus results)) >> updateHealthyUnhealthy overviewGauge results

updateSingle :: PipelinesOverviewGauge -> (BuildStatus, Double) -> IO ()
updateSingle overviewGauge (status, count) = withLabel overviewGauge ((toLower . tshow) status) (`setGauge` count)

updateHealthyUnhealthy :: PipelinesOverviewGauge -> [Result] -> IO ()
updateHealthyUnhealthy overviewGauge results = do
  let (healthyCount, unhealthyCount) = bimap length length (partition (isHealthy . buildStatus) results)
  withLabel overviewGauge "healthy" (`setGauge` fromIntegral healthyCount)
  withLabel overviewGauge "unhealthy" (`setGauge` fromIntegral unhealthyCount)
  pure ()

countByBuildStatus :: [Result] -> Map BuildStatus Double
countByBuildStatus results = countOccurrences buildStatus results `union` resetValues

resetValues :: Map BuildStatus Double
resetValues = fromList $ (,0) <$> enumerate

updateMetrics :: (HasBuildStatuses env, HasPipelinesOverviewGauge env) => RIO env ()
updateMetrics = do
  pipelinesGauge <- view pipelinesOverviewGaugeL
  statuses <- getStatuses
  liftIO $ updatePipelinesOverviewMetric pipelinesGauge statuses

updateMetricsRegularly :: (HasBuildStatuses env, HasPipelinesOverviewGauge env) => RIO env ()
updateMetricsRegularly = forever $ do
  updateMetrics
  threadDelay $ 10 * 1000000

class HasPipelinesOverviewGauge env where
  pipelinesOverviewGaugeL :: Lens' env PipelinesOverviewGauge

class HasOutgoingHttpRequestsHistogram env where
  outgoingHttpRequestsHistogramL :: Lens' env OutgoingHttpRequestsHistogram

countOccurrences :: (Ord k, Num a) => (t -> k) -> [t] -> Map k a
countOccurrences f xs = fromListWith (+) [(f x, 1) | x <- xs]

data VectorWithLabel l m = VectorWithLabel (Vector l m) l

instance (Label l, Observer m) => Observer (VectorWithLabel l m) where
  observe (VectorWithLabel vctr label) value = withLabel vctr label f
    where
      f metric = observe metric value
