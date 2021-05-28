{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config.Backbone
  ( LogConfig (LogConfig),
    GitCommit (..),
    logContext,
    logEnv,
    logNamespace,
    parseLogLevelWithDefault,
    Backbone (..),
    parseBackboneFromEnv,
  )
where

import Control.Concurrent (ThreadId)
import Control.Lens
import Core.Lib (BuildStatuses)
import GitHash
import Katip (LogContexts, LogEnv, Namespace, Severity (..))
import Metrics.Metrics
import Relude hiding (lookupEnv)

envLogLevel :: Text
envLogLevel = "GCB_LOG_LEVEL"

parseBackboneFromEnv :: Metrics -> IORef BuildStatuses -> IORef [(ThreadId, Text)] -> LogConfig -> Backbone
parseBackboneFromEnv metrics iorefBuilds iorefThreads logConfig =
  Backbone metrics iorefBuilds logConfig (GitCommit $ giTag gitCommit <> "/" <> giBranch gitCommit <> "@" <> giHash gitCommit) iorefThreads
  where
    gitCommit = $$tGitInfoCwd

data Backbone = Backbone
  { metrics :: Metrics,
    statuses :: IORef BuildStatuses,
    logConfig :: LogConfig,
    gitCommit :: GitCommit,
    threads :: IORef [(ThreadId, Text)]
  }

data LogConfig = LogConfig
  { _logNamespace :: Namespace,
    _logContext :: LogContexts,
    _logEnv :: LogEnv
  }

newtype GitCommit = GitCommit String deriving (Show)

parseLogLevelWithDefault :: [(String, String)] -> (Severity, Maybe Text)
parseLogLevelWithDefault env = case lookupEnv env envLogLevel of
  Nothing -> (InfoS, Just "Couldn't parse log level from env. Using Info as fallback")
  Just "DEBUG" -> (DebugS, Nothing)
  Just "INFO" -> (InfoS, Nothing)
  Just "WARN" -> (WarningS, Nothing)
  Just "ERROR" -> (ErrorS, Nothing)
  Just s -> (InfoS, Just (s <> " is no valid log level. Using Info as fallback"))

lookupEnv :: [(String, String)] -> Text -> Maybe Text
lookupEnv env key = fromString . snd <$> find (\(k, _) -> k == toString key) env

makeLenses ''LogConfig
