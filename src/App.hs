{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App (App.run) where

import Config.Backbone
import Config.Config
import Control.Concurrent (forkIO)
import qualified Data.Text as T (intercalate)
import Inbound.HTTP.Server (startServer)
import Inbound.Jobs.BuildStatuses (updateStatusesRegularly)
import Inbound.Jobs.Runners (updateRunnersJobsRegularly)
import Katip hiding (getEnvironment)
import Logger
import Metrics.Health (initHealth, initThreads)
import Metrics.Metrics
import Outbound.Gitlab.Pipelines (pipelinesApiToIO)
import Outbound.Gitlab.Projects (projectsApiToIO)
import qualified Outbound.Gitlab.Projects as Projects (initCache)
import Outbound.Gitlab.Runners (runnersApiToIO)
import qualified Outbound.Gitlab.Runners as Runners (initCache)
import Outbound.Storage.BuildStatuses.InMemory (buildStatusesApiToIO)
import qualified Outbound.Storage.BuildStatuses.InMemory as Statuses (initStorage)
import Outbound.Storage.Runners.InMemory (runnersJobsApiToIO)
import qualified Outbound.Storage.Runners.InMemory as Runners (initStorage)
import Polysemy
import Polysemy.Reader (runReader)
import Polysemy.Time (interpretTimeGhc)
import Relude hiding (runReader)
import System.Environment
import Util (parTraverseToIO)
import Validation

startMetricsUpdatingJob :: Config -> Backbone -> IO ()
startMetricsUpdatingJob config backbone =
  runM
    . metricsApiToIO (groupId config) (metrics backbone)
    . buildStatusesApiToIO (statuses backbone)
    . interpretTimeGhc
    $ updateMetricsRegularly

startStatusUpdatingJob :: Config -> Backbone -> IO ()
startStatusUpdatingJob Config {..} Backbone {..} = do
  cache <- Projects.initCache projectCacheTtlSecs
  runFinal
    . embedToFinal
    . buildStatusesApiToIO statuses
    . pipelinesApiToIO gitlabBaseUrl apiToken groupId (outgoingHttpRequestsHistogram metrics)
    . projectsApiToIO gitlabBaseUrl apiToken includeSharedProjects (outgoingHttpRequestsHistogram metrics) cache
    . parTraverseToIO maxConcurrency
    . interpretTimeGhc
    . runReader logConfig
    . loggerToIO
    . observeDurationToIO groupId (updateJobDurationHistogram metrics)
    $ updateStatusesRegularly groupId dataUpdateIntervalSecs projectExcludeList

startRunnersJobsUpdatingJob :: Config -> Backbone -> IO ()
startRunnersJobsUpdatingJob Config {..} Backbone {..} = do
  cache <- Runners.initCache runnerCacheTtlSecs
  runFinal
    . embedToFinal
    . runnersJobsApiToIO runners
    . runnersApiToIO gitlabBaseUrl apiToken (outgoingHttpRequestsHistogram metrics) cache
    . parTraverseToIO maxConcurrency
    . interpretTimeGhc
    . runReader logConfig
    . loggerToIO
    . observeDurationToIO groupId (updateJobDurationHistogram metrics)
    $ updateRunnersJobsRegularly groupId dataUpdateIntervalSecs projectExcludeList

startWithConfig :: Config -> Backbone -> IO ()
startWithConfig config backbone = do
  metrics <- forkIO $ startMetricsUpdatingJob config backbone
  statuses <- forkIO $ startStatusUpdatingJob config backbone
  runnersJobs <- forkIO $ startRunnersJobsUpdatingJob config backbone
  atomicWriteIORef (threads backbone) [(metrics, "metrics"), (statuses, "statuses"), (runnersJobs, "runnersJobs")]
  startServer config backbone

run :: IO ()
run = do
  environment <- getEnvironment
  statuses <- Statuses.initStorage
  runners <- Runners.initStorage
  healthThreads <- initThreads
  metrics <- registerMetrics
  health <- initHealth
  case parseConfigFromEnv environment of
    Success config ->
      withLogEnv (logLevel config) $ \lE -> do
        let backbone = initBackbone metrics statuses runners healthThreads health (LogConfig mempty mempty lE)
        singleLog lE InfoS $ "Using config: " <> show config
        singleLog lE InfoS $ "Running version: " <> show (gitCommit backbone)
        startWithConfig config backbone
    Failure errs ->
      withLogEnv ErrorS $ \lE -> singleLog lE ErrorS $ "Failed to parse config. Exiting now. Errors are: " <> T.intercalate ", " (toList errs)
