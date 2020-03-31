{-# LANGUAGE OverloadedStrings #-}

module Metrics where

import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef, readIORef)
import qualified Data.Text as T
import Katip
import Lens.Micro
import Lib hiding (name)
import Network.Wai.Metrics
import System.IO (stdout)
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.RegistryT as P
import System.Metrics.Prometheus.Ridley
import System.Metrics.Prometheus.Ridley.Types
import TextShow (showt)

createMetrics :: [Result] -> T.Text
createMetrics rs = T.unlines (header <> content)
  where
    header = ["# HELP build_status_gauge committed offsets", "# TYPE build_status_gauge gauge"]
    content = convertMetric <$> rs

convertMetric :: Result -> T.Text
convertMetric (Result name status _) = mconcat ["build_status_gauge{repository=\"", name, "\"} ", (showt . toMetricValue) status]

initializeMetricsMiddleware :: IORef [Result] -> IO (Maybe WaiMetrics)
initializeMetricsMiddleware ioref = do
  stdoutS <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let opts =
        newOptions [("app", "gitlab-ci-build-statuses")] (brokenBuildsMetric ioref : defaultMetrics)
          & prometheusOptions . samplingFrequency .~ 5
          & (dataRetentionPeriod ?~ 60)
          & katipScribes .~ ("GitlabCiBuildStatuses", [("stdout", stdoutS)])
  ctx <- startRidley opts ["metrics"] 8181
  pure $ ctx ^. ridleyWaiMetrics

brokenBuildsMetric :: IORef [Result] -> RidleyMetric
brokenBuildsMetric ioref = CustomMetric "build_statuses_broken_builds" (processBrokenBuilds ioref)

processBrokenBuilds :: MonadIO m => IORef [Result] -> RidleyOptions -> P.RegistryT m RidleyMetricHandler
processBrokenBuilds ioref opts = do
  let popts = opts ^. prometheusOptions
  openFD <- P.registerGauge "build_statuses_broken_builds" (popts ^. labels)
  return
    RidleyMetricHandler
      { metric = openFD,
        updateMetric = updateBrokenBuilds ioref,
        flush = False
      }

updateBrokenBuilds :: IORef [Result] -> P.Gauge -> Bool -> IO ()
updateBrokenBuilds ioref gauge _ = do
  results <- readIORef ioref
  let numBroken = length $ filter (\res -> buildStatus res == Failed) results
  P.set (fromIntegral numBroken) gauge
