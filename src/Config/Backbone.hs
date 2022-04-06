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

import Control.Lens
import Core.BuildStatuses (BuildStatuses, Project)
import Core.Jobs (WaitingJobs)
import Core.Runners (Runner, RunnersJobs)
import Core.Shared
import Data.Cache (Cache)
import GitHash
import Katip (LogContexts, LogEnv, Namespace)
import Metrics.Metrics
import Relude hiding (lookupEnv)
import qualified Text.Show (show)

initBackbone :: Metrics -> IORef BuildStatuses -> IORef RunnersJobs -> IORef WaitingJobs -> Cache (Id Group) [Project] -> Cache (Id Group) [Runner] -> LogConfig -> Backbone
initBackbone metrics iorefBuilds iorefRunnersJobs ioRefWaitingJobs projectsCache runnersCache logConfig =
  Backbone metrics iorefBuilds iorefRunnersJobs ioRefWaitingJobs projectsCache runnersCache logConfig (GitCommit $ giTag gitCommit <> "/" <> giBranch gitCommit <> "@" <> giHash gitCommit)
  where
    gitCommit = $$tGitInfoCwd

data Backbone = Backbone
  { metrics :: Metrics,
    statuses :: IORef BuildStatuses,
    runners :: IORef RunnersJobs,
    waitingJobs :: IORef WaitingJobs,
    projectsCache :: Cache (Id Group) [Project],
    runnersCache :: Cache (Id Group) [Runner],
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
