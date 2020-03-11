{-# LANGUAGE OverloadedStrings #-}

module Metrics where

import           Control.Monad.IO.Class                         (liftIO)
import qualified Data.Text                                      as T
import           Lib
import           System.Metrics.Prometheus.Concurrent.RegistryT
import           System.Metrics.Prometheus.Http.Scrape          (serveHttpTextMetricsT)
import           System.Metrics.Prometheus.Metric.Counter       (inc)
import           System.Metrics.Prometheus.MetricId

main :: IO ()
main =
  runRegistryT $
    -- Labels can be defined as lists or added to an empty label set
   do
    connectSuccessGauge <- registerGauge "example_connections" (fromList [("login", "success")])
    connectFailureGauge <- registerGauge "example_connections" (addLabel "login" "failure" mempty)
    connectCounter <- registerCounter "example_connection_total" mempty
    latencyHistogram <- registerHistogram "example_round_trip_latency_ms" mempty [10,20 .. 100]
    liftIO $ inc connectCounter -- increment a counter
    -- [...] pass metric handles to the rest of the app
    serveHttpTextMetricsT 8080 ["metrics"]

createMetrics :: [Result] -> T.Text
createMetrics rs = T.unlines ["# HELP build_status_gauge committed offsets", "# TYPE build_status_gauge gauge"]

convertMetric :: Result -> T.Text
convertMetric (Result name status) = ""
-- # HELP build_status_gauge committed offsets
-- # TYPE build_status_gauge gauge
-- build_status_gauge{topic="dead.letter",partition="4",} 323.0
-- build_status_gauge{topic="dead.letter",partition="3",} 354.0
-- build_status_gauge{topic="dead.letter",partition="2",} 355.0
-- build_status_gauge{topic="dead.letter",partition="1",} 338.0
-- build_status_gauge{topic="dead.letter",partition="0",} 343.0
