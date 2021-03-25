{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config
  ( ApiToken (..),
    Config (..),
    ConfigError (..),
    GitlabHost,
    GitCommit (..),
    LogConfig (LogConfig),
    MaxConcurrency (..),
    UiUpdateIntervalSeconds (..),
    ProjectCacheTtlSeconds (..),
    parseConfigFromEnv,
    Validation (Failure, Success),
    showErrors,
    logContext,
    logEnv,
    logNamespace,
    parseLogLevelWithDefault,
  )
where

import Control.Lens
import Core.Lib (BuildStatuses, DataUpdateIntervalSeconds (..), Group, Id (..), Url (..))
import qualified Data.ByteString as B hiding (pack)
import qualified Data.Text as T (intercalate)
import Data.Validation
import GitHash
import Katip (LogContexts, LogEnv, Namespace, Severity (..))
import Metrics.Metrics
import Network.URI (parseAbsoluteURI)
import Relude hiding (lookupEnv)
import qualified Text.Show
import Control.Concurrent (ThreadId)

envGroupId :: Text
envGroupId = "GCB_GITLAB_GROUP_ID"

envApiToken :: Text
envApiToken = "GCB_GITLAB_API_TOKEN"

envBaseUrl :: Text
envBaseUrl = "GCB_GITLAB_BASE_URL"

envDataUpdateInterval :: Text
envDataUpdateInterval = "GCB_DATA_UPDATE_INTERVAL_SECS"

envUiUpdateInterval :: Text
envUiUpdateInterval = "GCB_UI_UPDATE_INTERVAL_SECS"

envProjectCacheTtl :: Text
envProjectCacheTtl = "GCB_PROJECT_CACHE_TTL_SECS"

envMaxConcurrency :: Text
envMaxConcurrency = "GCB_MAX_CONCURRENCY"

envLogLevel :: Text
envLogLevel = "GCB_LOG_LEVEL"

parseConfigFromEnv :: Metrics -> IORef BuildStatuses -> IORef [(ThreadId, Text)] -> LogConfig -> [(String, String)] -> Validation (NonEmpty ConfigError) Config
parseConfigFromEnv metrics iorefBuilds iorefThreads logConfig env =
  Config <$> readApiTokenFromEnv env
    <*> readGroupIdFromEnv env
    <*> readBaseUrlFromEnv env
    <*> pure (readDataUpdateIntervalFromEnv env)
    <*> pure (readUiUpdateIntervalFromEnv env)
    <*> pure (readProjectCacheTtlSecondsFromEnv env)
    <*> pure (readMaxConcurrencyFromEnv env)
    <*> pure metrics
    <*> pure iorefBuilds
    <*> pure logConfig
    <*> pure (GitCommit $ giTag gitCommit <> "/" <> giBranch gitCommit <> "@" <> giHash gitCommit)
    <*> pure iorefThreads
  where
    gitCommit = $$tGitInfoCwd

showErrors :: NonEmpty ConfigError -> Text
showErrors errs = T.intercalate ", " $ fmap show (toList errs)

data Config = Config
  { apiToken :: ApiToken,
    groupId :: Id Group,
    gitlabBaseUrl :: Url GitlabHost,
    dataUpdateIntervalSecs :: DataUpdateIntervalSeconds,
    uiUpdateIntervalSecs :: UiUpdateIntervalSeconds,
    projectCacheTtlSecs :: ProjectCacheTtlSeconds,
    maxConcurrency :: MaxConcurrency,
    metrics :: Metrics,
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

instance Show Config where
  show Config {..} =
    "Config: GroupId "
      <> intercalate
        ", "
        [ show groupId,
          "Base URL " <> show gitlabBaseUrl,
          show dataUpdateIntervalSecs,
          show uiUpdateIntervalSecs,
          show projectCacheTtlSecs,
          show maxConcurrency,
          coerce gitCommit
        ]

newtype ApiToken = ApiToken B.ByteString

data GitlabHost

newtype UiUpdateIntervalSeconds = UiUpdateIntervalSeconds Int deriving (Show)

newtype ProjectCacheTtlSeconds = ProjectCacheTtlSeconds Int64 deriving (Show)

data ConfigError = ApiTokenMissing | GroupIdMissing | GitlabBaseUrlMissing | GitlabBaseUrlInvalid Text

instance Show ConfigError where
  show ApiTokenMissing = "API Token is missing. Set it via " <> show envApiToken
  show GroupIdMissing = "Group ID is missing. Set it via " <> show envGroupId
  show GitlabBaseUrlMissing = "Gitlab base URL is missing. Set it via " <> show envBaseUrl
  show (GitlabBaseUrlInvalid url) = "Gitlab base URL set via " <> show envBaseUrl <> "is invalid. The value is: " <> show url

newtype MaxConcurrency = MaxConcurrency Int deriving (Show)

newtype GitCommit = GitCommit String deriving (Show)

readApiTokenFromEnv :: [(String, String)] -> Validation (NonEmpty ConfigError) ApiToken
readApiTokenFromEnv env = do
  let maybeGroupId = encodeUtf8 <$> lookupEnv env envApiToken
  maybe (_Failure # single ApiTokenMissing) (\token -> _Success # ApiToken token) maybeGroupId

readGroupIdFromEnv :: [(String, String)] -> Validation (NonEmpty ConfigError) (Id Group)
readGroupIdFromEnv env = do
  let maybeGroupId = do
        groupIdString <- toString <$> lookupEnv env envGroupId
        readMaybe groupIdString
  maybe (_Failure # single GroupIdMissing) (\gId -> _Success # Id gId) maybeGroupId

readBaseUrlFromEnv :: [(String, String)] -> Validation (NonEmpty ConfigError) (Url GitlabHost)
readBaseUrlFromEnv env = do
  let valueFromEnv = lookupEnv env envBaseUrl
  let baseUrl = valueFromEnv >>= parseAbsoluteURI . toString
  if isJust baseUrl
    then maybe urlMissing (\url -> _Success # Url url) baseUrl
    else maybe urlMissing (\s -> _Failure # single (GitlabBaseUrlInvalid s)) valueFromEnv
  where
    urlMissing = _Failure # single GitlabBaseUrlMissing

readDataUpdateIntervalFromEnv :: [(String, String)] -> DataUpdateIntervalSeconds
readDataUpdateIntervalFromEnv env = DataUpdateIntervalSeconds $ parsePositiveWithDefault env envDataUpdateInterval 60

readUiUpdateIntervalFromEnv :: [(String, String)] -> UiUpdateIntervalSeconds
readUiUpdateIntervalFromEnv env = UiUpdateIntervalSeconds $ parsePositiveWithDefault env envUiUpdateInterval 5

readProjectCacheTtlSecondsFromEnv :: [(String, String)] -> ProjectCacheTtlSeconds
readProjectCacheTtlSecondsFromEnv env = ProjectCacheTtlSeconds $ parsePositiveWithDefault env envProjectCacheTtl 0

readMaxConcurrencyFromEnv :: [(String, String)] -> MaxConcurrency
readMaxConcurrencyFromEnv env = MaxConcurrency $ parsePositiveWithDefault env envMaxConcurrency 2

parsePositiveWithDefault :: (Ord a, Num a, Read a) => [(String, String)] -> Text -> a -> a
parsePositiveWithDefault env text fallback = fromMaybe fallback $ find (> 0) (lookupEnv env text >>= (readMaybe . toString))

parseLogLevelWithDefault :: [(String, String)] -> (Severity, Maybe Text)
parseLogLevelWithDefault env = case lookupEnv env envLogLevel of
  Nothing -> (InfoS, Just "Couldn't parse log level from env. Using Info as fallback")
  Just "DEBUG" -> (DebugS, Nothing)
  Just "INFO" -> (InfoS, Nothing)
  Just "WARN" -> (WarningS, Nothing)
  Just "ERROR" -> (ErrorS, Nothing)
  Just s -> (InfoS, Just (s <> " is no valid log level. Using Info as fallback"))

single :: a -> NonEmpty a
single a = a :| []

lookupEnv :: [(String, String)] -> Text -> Maybe Text
lookupEnv env key = fromString . snd <$> find (\(k, _) -> k == toString key) env

makeLenses ''LogConfig
