{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Metrics
  ( initPrometheusMetrics,
  )
where

import Control.Monad.Except (MonadIO)
import GHC.Conc (getNumCapabilities, getNumProcessors, numSparks)
import Network.Wai.Handler.Warp (Port)
import RIO
import qualified System.Metrics as EKG
import System.Metrics.Prometheus.Http.Scrape
import System.Metrics.Prometheus.MetricId
import System.Metrics.Prometheus.Registry (Registry)
import System.Metrics.Prometheus.RegistryT
import System.Remote.Monitoring.Prometheus (defaultOptions, registerEKGStore)

initPrometheusMetrics :: EKG.Store -> IO ()
initPrometheusMetrics store = do
  _ <- threadDelay 5000000
  EKG.registerGcMetrics store
  EKG.registerGauge "ghc.conc.num_sparks" (fromIntegral <$> numSparks) store
  EKG.registerCounter "ghc.conc.num_capabilities" (fromIntegral <$> getNumCapabilities) store
  EKG.registerCounter "ghc.conc.num_processors" (fromIntegral <$> getNumProcessors) store
  _ <- mkRegistry store 8080
  return ()

mkRegistry :: MonadIO m => EKG.Store -> Port -> m ((), Registry)
mkRegistry store port =
  runRegistryT $ do
    registerEKGStore store (defaultOptions $ fromList [("ghc", "rts")])
    sample >>= serveHttpTextMetrics port ["metrics"]
