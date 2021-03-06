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

import Control.Concurrent (ThreadId)
import Control.Lens
import Core.BuildStatuses (BuildStatuses)
import Core.Runners (RunnersJobs)
import GitHash
import Katip (LogContexts, LogEnv, Namespace)
import Metrics.Metrics
import Relude hiding (lookupEnv)

initBackbone :: Metrics -> IORef BuildStatuses -> IORef RunnersJobs -> IORef [(ThreadId, Text)] -> IORef Bool -> LogConfig -> Backbone
initBackbone metrics iorefBuilds iorefRunnersJobs iorefThreads health logConfig =
  Backbone metrics iorefBuilds iorefRunnersJobs logConfig (GitCommit $ giTag gitCommit <> "/" <> giBranch gitCommit <> "@" <> giHash gitCommit) iorefThreads health
  where
    gitCommit = $$tGitInfoCwd

data Backbone = Backbone
  { metrics :: Metrics,
    statuses :: IORef BuildStatuses,
    runners :: IORef RunnersJobs,
    logConfig :: LogConfig,
    gitCommit :: GitCommit,
    threads :: IORef [(ThreadId, Text)],
    health :: IORef Bool
  }

data LogConfig = LogConfig
  { _logNamespace :: Namespace,
    _logContext :: LogContexts,
    _logEnv :: LogEnv
  }

newtype GitCommit = GitCommit String deriving stock (Show)

makeLenses ''LogConfig
