{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App (App.run) where

import Config.Backbone
import Config.Config
import Config.Interpreters
import Control.Concurrent.Async (Concurrently (..))
import qualified Data.Text as T (intercalate)
import Katip hiding (getEnvironment)
import Logger
import Metrics.Metrics
import Polysemy
import Polysemy.Time (interpretTimeGhc)
import Ports.Inbound.HTTP.Server (startServer)
import Ports.Inbound.Jobs.BuildStatuses (updateStatusesRegularly)
import Ports.Inbound.Jobs.Runners (updateRunnersJobsRegularly)
import Ports.Inbound.Jobs.WaitingJobs (updateWaitingJobsRegularly)
import Ports.Outbound.Gitlab.Jobs (jobsApiToIO)
import Ports.Outbound.Gitlab.Pipelines (pipelinesApiToIO)
import Ports.Outbound.Gitlab.Projects (projectsApiToIO, projectsWithoutExcludesApiInTermsOfProjects)
import Ports.Outbound.Gitlab.Runners (runnersApiToIO)
import Ports.Outbound.Storage.BuildStatuses.InMemory (buildStatusesApiToIO)
import Ports.Outbound.Storage.Runners.InMemory (runnersJobsApiToIO)
import Ports.Outbound.Storage.WaitingJobs.InMemory (waitingJobsApiToIO)
import Relude
import System.Environment
import Util (parTraverseToIO)
import Validation

startMetricsUpdatingJob :: Config -> Backbone -> IO ()
startMetricsUpdatingJob config backbone =
  runM
    . runConfig config
    . runBackbone backbone
    . waitingJobsApiToIO
    . runnersJobsApiToIO
    . metricsApiToIO
    . buildStatusesApiToIO
    . interpretTimeGhc
    $ updateMetricsRegularly

startStatusUpdatingJob :: Config -> Backbone -> IO ()
startStatusUpdatingJob config backbone =
  runFinal
    . embedToFinal
    . runConfig config
    . runBackbone backbone
    . buildStatusesApiToIO
    . pipelinesApiToIO
    . metricsApiToIO
    . projectsApiToIO
    . parTraverseToIO
    . interpretTimeGhc
    . loggerToIO
    . projectsWithoutExcludesApiInTermsOfProjects
    . observeDurationToIO
    $ updateStatusesRegularly

startRunnersJobsUpdatingJobIfEnabled :: Config -> Backbone -> IO ()
startRunnersJobsUpdatingJobIfEnabled config _ | jobsView config == Disabled = pass
startRunnersJobsUpdatingJobIfEnabled config backbone = startRunnersJobsUpdatingJob config backbone

startRunnersJobsUpdatingJob :: Config -> Backbone -> IO ()
startRunnersJobsUpdatingJob config backbone = do
  runFinal
    . embedToFinal
    . runConfig config
    . runBackbone backbone
    . runnersJobsApiToIO
    . metricsApiToIO
    . projectsApiToIO
    . parTraverseToIO
    . interpretTimeGhc
    . loggerToIO
    . projectsWithoutExcludesApiInTermsOfProjects
    . runnersApiToIO
    . observeDurationToIO
    $ updateRunnersJobsRegularly

startWaitingJobsUpdatingJobIfEnabled :: Config -> Backbone -> IO ()
startWaitingJobsUpdatingJobIfEnabled config _ | jobsView config == Disabled = pass
startWaitingJobsUpdatingJobIfEnabled config backbone = startWaitingJobsUpdatingJob config backbone

startWaitingJobsUpdatingJob :: Config -> Backbone -> IO ()
startWaitingJobsUpdatingJob config backbone =
  runFinal
    . embedToFinal
    . runConfig config
    . runBackbone backbone
    . waitingJobsApiToIO
    . metricsApiToIO
    . jobsApiToIO
    . projectsApiToIO
    . parTraverseToIO
    . interpretTimeGhc
    . loggerToIO
    . projectsWithoutExcludesApiInTermsOfProjects
    . observeDurationToIO
    $ updateWaitingJobsRegularly

startWithConfig :: Config -> Backbone -> IO ()
startWithConfig config backbone =
  runConcurrently $
    Concurrently (startMetricsUpdatingJob config backbone)
      *> Concurrently (startStatusUpdatingJob config backbone)
      *> Concurrently (startRunnersJobsUpdatingJobIfEnabled config backbone)
      *> Concurrently (startWaitingJobsUpdatingJobIfEnabled config backbone)
      *> Concurrently (startServer config backbone)

run :: IO ()
run = do
  environment <- getEnvironment
  case parseConfigFromEnv environment of
    Success config@Config {..} ->
      withLogEnv logLevel $ \lE -> do
        backbone <- initBackbone lE config
        singleLog lE InfoS $ "Using config: " <> show config
        singleLog lE InfoS $ "Running version: " <> show (gitCommit backbone)
        startWithConfig config backbone
    Failure errs ->
      withLogEnv ErrorS $ \lE -> singleLog lE ErrorS $ "Failed to parse config. Exiting now. Errors are: " <> T.intercalate ", " (toList errs)
