{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App (startWithConfig) where

import Config
import Control.Concurrent (forkIO)
import Inbound.HTTP.Server (startServer)
import Inbound.Jobs.Inbound.Jobs.Updating (updateStatusesRegularly)
import Logger
import Metrics.Metrics
import Outbound.Gitlab.GitlabAPI (initCache, pipelinesApiToIO, projectsApiToIO)
import Outbound.Storage.InMemory (buildStatusesApiToIO)
import Polysemy
import Polysemy.Reader
import Relude hiding (runReader)
import Util (delayToIO, parTraverseToIO)

startMetricsUpdatingJob :: Config -> IO ()
startMetricsUpdatingJob config =
  runM
    . metricsApiToIO (groupId config) (metrics config)
    . buildStatusesApiToIO (statuses config)
    . delayToIO
    $ updateMetricsRegularly

startStatusUpdatingJob :: Config -> IO ()
startStatusUpdatingJob Config {..} = do
  cache <- initCache projectCacheTtlSecs
  runFinal
    . embedToFinal @IO
    . buildStatusesApiToIO statuses
    . pipelinesApiToIO gitlabBaseUrl apiToken groupId (outgoingHttpRequestsHistogram metrics)
    . projectsApiToIO gitlabBaseUrl apiToken (outgoingHttpRequestsHistogram metrics) cache
    . parTraverseToIO maxConcurrency
    . delayToIO
    . runReader logConfig
    . loggerToIO
    . observeDurationToIO groupId (updateJobDurationHistogram metrics)
    $ updateStatusesRegularly groupId dataUpdateIntervalSecs

startWithConfig :: Config -> IO ()
startWithConfig config = do
  metrics <- forkIO $ startMetricsUpdatingJob config
  statuses <- forkIO $ startStatusUpdatingJob config
  atomicWriteIORef (threads config) [(metrics, "metrics"), (statuses, "statuses")]
  startServer config
