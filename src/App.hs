{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module App (App.run) where

import Config.Backbone
import Config.Config
import Config.Interpreters
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
startMetricsUpdatingJob config backbone =
  runM
    . runConfig config
    . runBackbone backbone
    . runnersJobsApiToIO
    . metricsApiToIO
    . buildStatusesApiToIO
    . interpretTimeGhc
    $ updateMetricsRegularly

startStatusUpdatingJob :: Config -> Backbone -> IO ()
startStatusUpdatingJob config@Config {..} backbone = do
  cache <- Projects.initCache projectCacheTtlSecs
  runFinal
    . embedToFinal
    . R.runReader cache
    . runConfig config
    . runBackbone backbone
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
startRunnersJobsUpdatingJob config@Config {..} backbone = do
  cache <- Runners.initCache runnerCacheTtlSecs
  runFinal
    . embedToFinal
    . R.runReader cache
    . runConfig config
    . runBackbone backbone
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
