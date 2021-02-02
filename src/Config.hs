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
import Data.List.NonEmpty hiding (group, intersperse, toList)
import Data.Maybe
import qualified Data.Text as T (intercalate, unpack)
import Data.Validation
import GitHash
import Katip (LogContexts, LogEnv, Namespace, Severity (..))
import Metrics.Metrics
import Network.URI (parseAbsoluteURI)
import qualified RIO.Map as Map
import RIO.Process
import Relude
import qualified Text.Show

envGroupId :: Text
envGroupId = "GITLAB_GROUP_ID"

envApiToken :: Text
envApiToken = "GITLAB_API_TOKEN"

envBaseUrl :: Text
envBaseUrl = "GITLAB_BASE_URL"

envDataUpdateInterval :: Text
envDataUpdateInterval = "DATA_UPDATE_INTERVAL_SECS"

envUiUpdateInterval :: Text
envUiUpdateInterval = "UI_UPDATE_INTERVAL_SECS"

envMaxConcurrency :: Text
envMaxConcurrency = "MAX_CONCURRENCY"

envLogLevel :: Text
envLogLevel = "LOG_LEVEL"

parseConfigFromEnv :: Metrics -> IORef BuildStatuses -> LogConfig -> ProcessContext -> Validation (NonEmpty ConfigError) Config
parseConfigFromEnv metrics ioref logConfig pc =
  Config <$> readApiTokenFromEnv pc
    <*> readGroupIdFromEnv pc
    <*> readBaseUrlFromEnv pc
    <*> pure (readDataUpdateIntervalFromEnv pc)
    <*> pure (readUiUpdateIntervalFromEnv pc)
    <*> pure (readMaxConcurrencyFromEnv pc)
    <*> pure metrics
    <*> pure ioref
    <*> pure logConfig
    <*> pure (GitCommit $ giBranch gitCommit <> "@" <> giHash gitCommit)
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
    maxConcurrency :: MaxConcurrency,
    metrics :: Metrics,
    statuses :: IORef BuildStatuses,
    logConfig :: LogConfig,
    gitCommit :: GitCommit
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
          show maxConcurrency,
          show gitCommit
        ]

newtype ApiToken = ApiToken B.ByteString

data GitlabHost

newtype UiUpdateIntervalSeconds = UiUpdateIntervalSeconds Int deriving (Show)

data ConfigError = ApiTokenMissing | GroupIdMissing | GitlabBaseUrlMissing | GitlabBaseUrlInvalid Text

instance Show ConfigError where
  show ApiTokenMissing = "API Token is missing. Set it via " <> show envApiToken
  show GroupIdMissing = "Group ID is missing. Set it via " <> show envGroupId
  show GitlabBaseUrlMissing = "Gitlab base URL is missing. Set it via " <> show envBaseUrl
  show (GitlabBaseUrlInvalid url) = "Gitlab base URL set via " <> show envBaseUrl <> "is invalid. The value is: " <> show url

newtype MaxConcurrency = MaxConcurrency Int deriving (Show)

newtype GitCommit = GitCommit String deriving (Show)

readApiTokenFromEnv :: ProcessContext -> Validation (NonEmpty ConfigError) ApiToken
readApiTokenFromEnv pc = do
  let maybeGroupId = encodeUtf8 <$> envFromPC pc envApiToken
  maybe (_Failure # single ApiTokenMissing) (\token -> _Success # ApiToken token) maybeGroupId

readGroupIdFromEnv :: ProcessContext -> Validation (NonEmpty ConfigError) (Id Group)
readGroupIdFromEnv pc = do
  let maybeGroupId = do
        groupIdString <- T.unpack <$> envFromPC pc envGroupId
        readMaybe groupIdString
  maybe (_Failure # single GroupIdMissing) (\gId -> _Success # Id gId) maybeGroupId

readBaseUrlFromEnv :: ProcessContext -> Validation (NonEmpty ConfigError) (Url GitlabHost)
readBaseUrlFromEnv pc = do
  let valueFromEnv = envFromPC pc envBaseUrl
  let baseUrl = valueFromEnv >>= parseAbsoluteURI . T.unpack
  if isJust baseUrl
    then maybe urlMissing (\url -> _Success # Url url) baseUrl
    else maybe urlMissing (\s -> _Failure # single (GitlabBaseUrlInvalid s)) valueFromEnv
  where
    urlMissing = _Failure # single GitlabBaseUrlMissing

readDataUpdateIntervalFromEnv :: ProcessContext -> DataUpdateIntervalSeconds
readDataUpdateIntervalFromEnv pc = DataUpdateIntervalSeconds $ parsePositiveWithDefault pc envDataUpdateInterval 60

readUiUpdateIntervalFromEnv :: ProcessContext -> UiUpdateIntervalSeconds
readUiUpdateIntervalFromEnv pc = UiUpdateIntervalSeconds $ parsePositiveWithDefault pc envUiUpdateInterval 5

readMaxConcurrencyFromEnv :: ProcessContext -> MaxConcurrency
readMaxConcurrencyFromEnv pc = MaxConcurrency $ parsePositiveWithDefault pc envMaxConcurrency 2

parsePositiveWithDefault :: ProcessContext -> Text -> Int -> Int
parsePositiveWithDefault pc text fallback = fromMaybe fallback $ find (> 0) (envFromPC pc text >>= (readMaybe . T.unpack))

parseLogLevelWithDefault :: ProcessContext -> (Severity, Maybe Text)
parseLogLevelWithDefault pc = case envFromPC pc envLogLevel of
  Nothing -> (InfoS, Just "Couldn't parse log level from env. Using Info as fallback")
  Just "DEBUG" -> (DebugS, Nothing)
  Just "INFO" -> (InfoS, Nothing)
  Just "WARN" -> (WarningS, Nothing)
  Just "ERROR" -> (ErrorS, Nothing)
  Just s -> (InfoS, Just (s <> " s no valid log level. Using Info as fallback"))

single :: a -> NonEmpty a
single a = a :| []

envFromPC :: ProcessContext -> Text -> Maybe Text
envFromPC pc key = Map.lookup key envVars
  where
    envVars = view envVarsL pc

makeLenses ''LogConfig
