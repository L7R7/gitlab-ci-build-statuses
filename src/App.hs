{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module App (App.run) where

import Config.Backbone
import Config.Config
import Control.Concurrent (ThreadId, forkIO)
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
import qualified Polysemy.Reader as R
import Polysemy.Time (interpretTimeGhc)
import Relude
import System.Environment
import Util (parTraverseToIO)
import Validation

startMetricsUpdatingJob :: Config -> Backbone -> IO ()
startMetricsUpdatingJob Config {..} Backbone {..} =
  runM
    . R.runReader groupId
    . R.runReader statuses
    . R.runReader runners
    . R.runReader (currentPipelinesOverview metrics)
    . R.runReader (runningJobsGauge metrics)
    . R.runReader (onlineRunnersGauge metrics)
    . runnersJobsApiToIO
    . metricsApiToIO
    . buildStatusesApiToIO
    . interpretTimeGhc
    $ updateMetricsRegularly

startStatusUpdatingJob :: Config -> Backbone -> IO ()
startStatusUpdatingJob Config {..} Backbone {..} = do
  cache <- Projects.initCache projectCacheTtlSecs
  runFinal
    . embedToFinal
    . R.runReader logConfig
    . R.runReader groupId
    . R.runReader dataUpdateIntervalSecs
    . R.runReader projectExcludeList
    . R.runReader gitlabBaseUrl
    . R.runReader apiToken
    . R.runReader includeSharedProjects
    . R.runReader maxConcurrency
    . R.runReader (outgoingHttpRequestsHistogram metrics)
    . R.runReader (updateJobDurationHistogram metrics)
    . R.runReader statuses
    . R.runReader cache
    . buildStatusesApiToIO
    . pipelinesApiToIO
    . projectsApiToIO
    . parTraverseToIO
    . interpretTimeGhc
    . loggerToIO
    . observeDurationToIO
    $ updateStatusesRegularly

startRunnersJobsUpdatingJobIfEnabled :: Config -> Backbone -> IO [ThreadId]
startRunnersJobsUpdatingJobIfEnabled config _ | jobsView config == Disabled = pure []
startRunnersJobsUpdatingJobIfEnabled config backbone = one <$> forkIO (startRunnersJobsUpdatingJob config backbone)

startRunnersJobsUpdatingJob :: Config -> Backbone -> IO ()
startRunnersJobsUpdatingJob Config {..} Backbone {..} = do
  cache <- Runners.initCache runnerCacheTtlSecs
  runFinal
    . embedToFinal
    . R.runReader logConfig
    . R.runReader groupId
    . R.runReader dataUpdateIntervalSecs
    . R.runReader projectExcludeList
    . R.runReader gitlabBaseUrl
    . R.runReader apiToken
    . R.runReader maxConcurrency
    . R.runReader (outgoingHttpRequestsHistogram metrics)
    . R.runReader (updateJobDurationHistogram metrics)
    . R.runReader runners
    . R.runReader cache
    . runnersJobsApiToIO
    . runnersApiToIO
    . parTraverseToIO
    . interpretTimeGhc
    . loggerToIO
    . observeDurationToIO
    $ updateRunnersJobsRegularly

startWithConfig :: Config -> Backbone -> IO ()
startWithConfig config backbone = do
  metrics <- forkIO $ startMetricsUpdatingJob config backbone
  statuses <- forkIO $ startStatusUpdatingJob config backbone
  runnersJobs <- startRunnersJobsUpdatingJobIfEnabled config backbone
  atomicWriteIORef (threads backbone) ([(metrics, "metrics"), (statuses, "statuses")] <> ((,"runnersJobs") <$> runnersJobs))
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
