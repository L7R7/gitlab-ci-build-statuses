{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App (startWithConfig) where

import Config.Backbone
import Config.Config
import Control.Concurrent (forkIO)
import Inbound.HTTP.Server (startServer)
import Inbound.Jobs.Updating (updateStatusesRegularly)
import Logger
import Metrics.Metrics
import Outbound.Gitlab.GitlabAPI (initCache, pipelinesApiToIO, projectsApiToIO)
import Outbound.Storage.InMemory (buildStatusesApiToIO)
import Polysemy
import Polysemy.Reader
import Polysemy.Time (interpretTimeGhc)
import Relude hiding (runReader)
import Util (parTraverseToIO)

startMetricsUpdatingJob :: Config -> Backbone -> IO ()
startMetricsUpdatingJob config backbone =
  runM
    . metricsApiToIO (groupId config) (metrics backbone)
    . buildStatusesApiToIO (statuses backbone)
    . interpretTimeGhc
    $ updateMetricsRegularly

startStatusUpdatingJob :: Config -> Backbone -> IO ()
startStatusUpdatingJob Config {..} Backbone {..} = do
  cache <- initCache projectCacheTtlSecs
  runFinal
    . embedToFinal @IO
    . buildStatusesApiToIO statuses
    . pipelinesApiToIO gitlabBaseUrl apiToken groupId (outgoingHttpRequestsHistogram metrics)
    . projectsApiToIO gitlabBaseUrl apiToken includeSharedProjects (outgoingHttpRequestsHistogram metrics) cache
    . parTraverseToIO maxConcurrency
    . interpretTimeGhc
    . runReader logConfig
    . loggerToIO
    . observeDurationToIO groupId (updateJobDurationHistogram metrics)
    $ updateStatusesRegularly groupId dataUpdateIntervalSecs

startWithConfig :: Config -> Backbone -> IO ()
startWithConfig config backbone = do
  metrics <- forkIO $ startMetricsUpdatingJob config backbone
  statuses <- forkIO $ startStatusUpdatingJob config backbone
  atomicWriteIORef (threads backbone) [(metrics, "metrics"), (statuses, "statuses")]
  startServer config backbone
