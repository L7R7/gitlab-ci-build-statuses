{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import Outbound.Gitlab.GitlabAPI (pipelinesApiToIO, projectsApiToIO)
import Outbound.Storage.InMemory (buildStatusesApiToIO)
import Polysemy
import Polysemy.Reader
import RIO hiding (runReader)
import Util (delayToIO, parTraverseToIO)

startMetricsUpdatingJob :: Config -> IO ()
startMetricsUpdatingJob config =
  runM
    . metricsApiToIO (metrics config)
    . buildStatusesApiToIO (statuses config)
    . delayToIO
    $ updateMetricsRegularly

startStatusUpdatingJob :: Config -> IO ()
startStatusUpdatingJob Config {..} =
  runFinal
    . embedToFinal @IO
    . buildStatusesApiToIO statuses
    . pipelinesApiToIO gitlabBaseUrl apiToken (outgoingHttpRequestsHistogram metrics)
    . projectsApiToIO gitlabBaseUrl apiToken (outgoingHttpRequestsHistogram metrics)
    . parTraverseToIO maxConcurrency
    . delayToIO
    . runReader logConfig
    . loggerToIO
    . observeDurationToIO (updateJobDurationHistogram metrics)
    $ updateStatusesRegularly groupId dataUpdateIntervalSecs

startWithConfig :: Config -> IO ()
startWithConfig config = do
  _ <- forkIO $ startMetricsUpdatingJob config
  _ <- forkIO $ startStatusUpdatingJob config
  startServer config
