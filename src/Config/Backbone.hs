{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Config.Backbone
  ( LogConfig (LogConfig),
    GitCommit (..),
    logContext,
    logEnv,
    logNamespace,
    Backbone (..),
    initBackbone,
  )
where

import Config.Config
import Control.Lens
import Core.BuildStatuses (BuildStatuses, Project)
import Core.Runners (Runner, RunnersJobs)
import Core.Shared
import Data.Cache (Cache)
import GitHash
import Katip (LogContexts, LogEnv, Namespace)
import Metrics.Metrics
import Ports.Outbound.Gitlab.Projects qualified as Projects (initCache)
import Ports.Outbound.Gitlab.Runners qualified as Runners (initCache)
import Ports.Outbound.Storage.BuildStatuses.InMemory qualified as Statuses (initStorage)
import Ports.Outbound.Storage.Runners.InMemory qualified as Runners (initStorage)
import Relude hiding (lookupEnv)
import Text.Show qualified (show)

initBackbone :: LogEnv -> Config -> IO Backbone
initBackbone logEnv Config {..} = do
  statuses <- Statuses.initStorage
  runners <- Runners.initStorage
  metrics <- registerMetrics
  projectsCache <- Projects.initCache projectCacheTtlSecs
  groupRunnersCache <- Runners.initCache runnerCacheTtlSecs
  projectRunnersCache <- Runners.initCache runnerCacheTtlSecs
  let logConfig = LogConfig mempty mempty logEnv
  pure $ Backbone metrics statuses runners projectsCache groupRunnersCache projectRunnersCache logConfig (GitCommit $ giTag gitCommit <> "/" <> giBranch gitCommit <> "@" <> giHash gitCommit)
  where
    gitCommit = $$tGitInfoCwd

data Backbone = Backbone
  { metrics :: Metrics,
    statuses :: IORef BuildStatuses,
    runners :: IORef RunnersJobs,
    projectsCache :: Cache (Id Group) [Project],
    groupRunnersCache :: Cache (Id Group) [Runner],
    projectRunnersCache :: Cache (Id Group) [(Id Project, [Runner])],
    logConfig :: LogConfig,
    gitCommit :: GitCommit
  }

data LogConfig = LogConfig
  { _logNamespace :: Namespace,
    _logContext :: LogContexts,
    _logEnv :: LogEnv
  }

newtype GitCommit = GitCommit String

instance Show GitCommit where
  show (GitCommit commit) = "GitCommit " <> commit

makeLenses ''LogConfig
