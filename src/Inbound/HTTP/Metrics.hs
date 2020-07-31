{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Inbound.HTTP.Metrics
  ( HasPipelinesOverviewGauge (..),
    Metrics (..),
    registerAppMetrics,
    registerGhcMetrics,
    updateMetricsRegularly,
    updatePipelinesOverviewMetric,
  )
where

import Core.Lib (BuildStatus, BuildStatuses (..), HasBuildStatuses, Result (..), getStatuses, isHealthy)
import Data.List (partition)
import Data.Map hiding (partition)
import Prometheus
import Prometheus.Metric.GHC
import RIO hiding (Vector, toList)
import RIO.Text (toLower)

registerGhcMetrics :: MonadIO m => m GHCMetrics
registerGhcMetrics = register ghcMetrics

type PipelinesOverviewGauge = Vector Label1 Gauge

newtype Metrics = Metrics {currentPipelinesOverview :: PipelinesOverviewGauge}

registerPipelinesOverviewMetric :: IO PipelinesOverviewGauge
registerPipelinesOverviewMetric =
  register $
    vector "build_status" $
      gauge (Info "build_pipelines_by_status_gauge" "Gauge that indicates the count of the pipeline statuses grouped by their result")

registerAppMetrics :: IO Metrics
registerAppMetrics = Metrics <$> registerPipelinesOverviewMetric

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
countByBuildStatus = countOccurrences buildStatus

updateMetrics :: (HasBuildStatuses env, HasPipelinesOverviewGauge env) => RIO env ()
updateMetrics = do
  pipelinesGauge <- view getPipelinesOverviewGaugeL
  statuses <- getStatuses
  liftIO $ updatePipelinesOverviewMetric pipelinesGauge statuses

updateMetricsRegularly :: (HasBuildStatuses env, HasPipelinesOverviewGauge env) => RIO env ()
updateMetricsRegularly = forever $ do
  updateMetrics
  threadDelay $ 10 * 1000000

class HasPipelinesOverviewGauge env where
  getPipelinesOverviewGaugeL :: Lens' env PipelinesOverviewGauge

countOccurrences :: (Ord k, Num a) => (t -> k) -> [t] -> Map k a
countOccurrences f xs = fromListWith (+) [(f x, 1) | x <- xs]
