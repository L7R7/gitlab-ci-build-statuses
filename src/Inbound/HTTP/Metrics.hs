{-# LANGUAGE NoImplicitPrelude #-}

module Inbound.HTTP.Metrics where

import Prometheus (register)
import Prometheus.Metric.GHC (GHCMetrics, ghcMetrics)
import RIO (MonadIO)

registerGhcMetrics :: MonadIO m => m GHCMetrics
registerGhcMetrics = register ghcMetrics
