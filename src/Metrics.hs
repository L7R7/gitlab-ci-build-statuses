{-# LANGUAGE OverloadedStrings #-}

module Metrics where

import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef, readIORef)
import Data.List (foldl')
import Katip
import Lens.Micro
import Lib hiding (name)
import Network.Wai.Metrics
import System.IO (stdout)
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.RegistryT as P
import System.Metrics.Prometheus.Ridley
import System.Metrics.Prometheus.Ridley.Types

initializeMetricsMiddleware :: IORef [Result] -> IO (Maybe WaiMetrics)
initializeMetricsMiddleware ioref = do
  stdoutS <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let opts =
        newOptions [("app", "gitlab-ci-build-statuses")] (defaultMetrics <> initializeBuildMetrics ioref)
          & prometheusOptions . samplingFrequency .~ 5
          & (dataRetentionPeriod ?~ 60)
          & katipScribes .~ ("GitlabCiBuildStatuses", [("stdout", stdoutS)])
  ctx <- startRidley opts ["metrics"] 8181
  pure $ ctx ^. ridleyWaiMetrics

initializeBuildMetrics :: IORef [Result] -> [RidleyMetric]
initializeBuildMetrics results = (\m -> m results) <$> [failedBuildsMetric, successfulBuildsMetric]

failedBuildsMetric :: IORef [Result] -> RidleyMetric
failedBuildsMetric ioref = CustomMetric "build_statuses_failed_builds" (processFailedBuilds ioref)

processFailedBuilds :: MonadIO m => IORef [Result] -> RidleyOptions -> P.RegistryT m RidleyMetricHandler
processFailedBuilds ioref opts = do
  let popts = opts ^. prometheusOptions
  openFD <- P.registerGauge "build_statuses_failed_builds" (popts ^. labels)
  return
    RidleyMetricHandler
      { metric = openFD,
        updateMetric = updateFailedBuilds ioref,
        flush = False
      }

updateFailedBuilds :: IORef [Result] -> P.Gauge -> Bool -> IO ()
updateFailedBuilds ioref gauge _ = do
  results <- readIORef ioref
  let numFailed = countResultsWithStatus Failed results
  P.set (fromIntegral numFailed) gauge

successfulBuildsMetric :: IORef [Result] -> RidleyMetric
successfulBuildsMetric ioref = CustomMetric "build_statuses_successful_builds" (processSuccesfulBuilds ioref)

processSuccesfulBuilds :: MonadIO m => IORef [Result] -> RidleyOptions -> P.RegistryT m RidleyMetricHandler
processSuccesfulBuilds ioref opts = do
  let popts = opts ^. prometheusOptions
  openFD <- P.registerGauge "build_statuses_successful_builds" (popts ^. labels)
  return
    RidleyMetricHandler
      { metric = openFD,
        updateMetric = updateSuccesfulBuilds ioref,
        flush = False
      }

updateSuccesfulBuilds :: IORef [Result] -> P.Gauge -> Bool -> IO ()
updateSuccesfulBuilds ioref gauge _ = do
  results <- readIORef ioref
  let numSuccessful = countResultsWithStatus Successful results
  P.set (fromIntegral numSuccessful) gauge

countResultsWithStatus :: Foldable t => BuildStatus -> t Result -> Int
countResultsWithStatus status = count (\res -> buildStatus res == status)

count :: Foldable t => (a -> Bool) -> t a -> Int
count f = foldl' (\acc a -> if f a then acc + 1 else acc) 0
