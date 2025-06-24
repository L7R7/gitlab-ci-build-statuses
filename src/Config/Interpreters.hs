{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Interpreters (runBackbone, runConfig) where

import Config.Backbone
import Config.Config
import Core.BuildStatuses
import Core.Runners
import Core.Shared
import Data.Cache (Cache)
import Metrics.Metrics
import Polysemy
import Polysemy.Reader
import Relude hiding (Reader, runReader)

runConfig :: Config -> Sem (Reader SharedProjects : Reader UserAgent : Reader ApiToken : Reader (Url GitlabHost) : Reader MaxConcurrency : Reader DataUpdateIntervalSeconds : Reader ExtraProjectsList : Reader ProjectExcludeList : Reader (NonEmpty (Id Group)) ': r) a -> Sem r a
runConfig Config {..} =
  runReader groupId
    . runReader projectExcludeList
    . runReader extraProjectsList
    . runReader dataUpdateIntervalSecs
    . runReader maxConcurrency
    . runReader gitlabBaseUrl
    . runReader apiToken
    . runReader userAgent
    . runReader includeSharedProjects

runBackbone :: Backbone -> Sem (Reader (IORef RunnersJobs) : Reader (IORef BuildStatuses) : Reader (Cache (Id Group) [Project]) : Reader (Cache (Id Group) [Runner]) : Reader (Cache (Id Group) [(Id Project, [Runner])]) : Reader LogConfig : Reader RunningJobsGauge : Reader OnlineRunnersGauge : Reader PipelinesOverviewGauge : Reader OutgoingHttpRequestsHistogram : Reader UpdateJobDurationHistogram : Reader CacheResultsCounter ': r) a -> Sem r a
runBackbone Backbone {..} =
  runMetrics metrics
    . runReader logConfig
    . runReader projectRunnersCache
    . runReader groupRunnersCache
    . runReader projectsCache
    . runReader statuses
    . runReader runners

runMetrics :: Metrics -> Sem (Reader RunningJobsGauge : Reader OnlineRunnersGauge : Reader PipelinesOverviewGauge : Reader OutgoingHttpRequestsHistogram : Reader UpdateJobDurationHistogram : Reader CacheResultsCounter ': r) a -> Sem r a
runMetrics Metrics {..} =
  runReader cacheResultsCounter
    . runReader updateJobDurationHistogram
    . runReader outgoingHttpRequestsHistogram
    . runReader currentPipelinesOverview
    . runReader onlineRunnersGauge
    . runReader runningJobsGauge
