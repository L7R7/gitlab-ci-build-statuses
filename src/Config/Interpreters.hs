{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Config.Interpreters (runBackbone, runConfig) where

import Config.Backbone
import Config.Config
import Core.BuildStatuses
import Core.Runners
import Core.Shared
import Metrics.Metrics
import Polysemy
import Polysemy.Reader
import Relude hiding (Reader, runReader)

runConfig :: Config -> Sem (Reader SharedProjects : Reader ApiToken : Reader (Url GitlabHost) : Reader MaxConcurrency : Reader DataUpdateIntervalSeconds : Reader [Id Project] : Reader (Id Group) ': r) a -> Sem r a
runConfig Config {..} =
  runReader groupId
    . runReader projectExcludeList
    . runReader dataUpdateIntervalSecs
    . runReader maxConcurrency
    . runReader gitlabBaseUrl
    . runReader apiToken
    . runReader includeSharedProjects

runBackbone :: Backbone -> Sem (Reader (IORef RunnersJobs) : Reader (IORef BuildStatuses) : Reader LogConfig : Reader RunningJobsGauge : Reader OnlineRunnersGauge : Reader PipelinesOverviewGauge : Reader OutgoingHttpRequestsHistogram : Reader UpdateJobDurationHistogram ': r) a -> Sem r a
runBackbone Backbone {..} =
  runMetrics metrics
    . runReader logConfig
    . runReader statuses
    . runReader runners

runMetrics :: Metrics -> Sem (Reader RunningJobsGauge : Reader OnlineRunnersGauge : Reader PipelinesOverviewGauge : Reader OutgoingHttpRequestsHistogram : Reader UpdateJobDurationHistogram ': r) a -> Sem r a
runMetrics Metrics {..} =
  runReader updateJobDurationHistogram
    . runReader outgoingHttpRequestsHistogram
    . runReader currentPipelinesOverview
    . runReader onlineRunnersGauge
    . runReader runningJobsGauge
