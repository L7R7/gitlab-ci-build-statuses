{-# LANGUAGE OverloadedStrings #-}
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

import Core.Lib (BuildStatus, BuildStatuses (..), HasBuildStatuses, Result (..), getStatuses)
import Data.Map
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
      gauge (Info "metric_name" "Gauge that indicates the count of the pipeline statuses grouped by their result")

registerAppMetrics :: IO Metrics
registerAppMetrics = Metrics <$> registerPipelinesOverviewMetric

updatePipelinesOverviewMetric :: PipelinesOverviewGauge -> BuildStatuses -> IO ()
updatePipelinesOverviewMetric _ NoSuccessfulUpdateYet = pure ()
updatePipelinesOverviewMetric overviewGauge (Statuses (_, results)) = traverse_ (updateSingle overviewGauge) (toList (countByBuildStatus results))

updateSingle :: PipelinesOverviewGauge -> (BuildStatus, Double) -> IO ()
updateSingle overviewGauge (status, count) = withLabel overviewGauge ((toLower . tshow) status) (`setGauge` count)

countByBuildStatus :: [Result] -> Map BuildStatus Double
countByBuildStatus = countOccurrences buildStatus

countOccurrences :: (Ord k, Num a) => (t -> k) -> [t] -> Map k a
countOccurrences f xs = fromListWith (+) [(f x, 1) | x <- xs]

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
