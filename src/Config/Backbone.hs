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
import Core.BuildStatuses (BuildStatuses)
import Core.Runners (RunnersJobs)
import GitHash
import Katip (LogContexts, LogEnv, Namespace)
import Metrics.Metrics
import Relude hiding (lookupEnv)
import qualified Text.Show (show)

initBackbone :: Metrics -> IORef BuildStatuses -> IORef RunnersJobs -> LogConfig -> Backbone
initBackbone metrics iorefBuilds iorefRunnersJobs logConfig =
  Backbone metrics iorefBuilds iorefRunnersJobs logConfig (GitCommit $ giTag gitCommit <> "/" <> giBranch gitCommit <> "@" <> giHash gitCommit)
  where
    gitCommit = $$tGitInfoCwd

data Backbone = Backbone
  { metrics :: Metrics,
    statuses :: IORef BuildStatuses,
    runners :: IORef RunnersJobs,
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
