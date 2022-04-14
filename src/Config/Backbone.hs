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
import Core.Jobs (WaitingJobs)
import Core.Runners (Runner, RunnersJobs)
import Core.Shared
import Data.Cache (Cache)
import GitHash
import Katip (LogContexts, LogEnv, Namespace)
import Metrics.Metrics
import qualified Ports.Outbound.Gitlab.Projects as Projects (initCache)
import qualified Ports.Outbound.Gitlab.Runners as Runners (initCache)
import qualified Ports.Outbound.Storage.BuildStatuses.InMemory as Statuses (initStorage)
import qualified Ports.Outbound.Storage.Runners.InMemory as Runners (initStorage)
import qualified Ports.Outbound.Storage.WaitingJobs.InMemory as WaitingJobs (initStorage)
import Relude hiding (lookupEnv)
import qualified Text.Show (show)

initBackbone :: LogEnv -> Config -> IO Backbone
initBackbone logEnv Config {..} = do
  statuses <- Statuses.initStorage
  runners <- Runners.initStorage
  waitingJobs <- WaitingJobs.initStorage
  metrics <- registerMetrics
  projectsCache <- Projects.initCache projectCacheTtlSecs
  groupRunnersCache <- Runners.initCache runnerCacheTtlSecs
  projectRunnersCache <- Runners.initCache runnerCacheTtlSecs
  let logConfig = LogConfig mempty mempty logEnv
  pure $ Backbone metrics statuses runners waitingJobs projectsCache groupRunnersCache projectRunnersCache logConfig (GitCommit $ giTag gitCommit <> "/" <> giBranch gitCommit <> "@" <> giHash gitCommit)
  where
    gitCommit = $$tGitInfoCwd

data Backbone = Backbone
  { metrics :: Metrics,
    statuses :: IORef BuildStatuses,
    runners :: IORef RunnersJobs,
    waitingJobs :: IORef WaitingJobs,
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
