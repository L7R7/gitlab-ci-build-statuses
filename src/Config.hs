{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config
  ( ApiToken (..),
    BaseUrl (..),
    Config (..),
    ConfigError (..),
    UiUpdateIntervalSeconds (..),
    parseConfigFromEnv,
    showErrors,
  )
where

import Control.Lens
import Core.Lib (DataUpdateIntervalMinutes (..), GroupId (..), MaxConcurrency (..))
import qualified Data.ByteString as B hiding (pack)
import Data.List (find)
import Data.List.NonEmpty hiding (group, toList)
import Data.Maybe
import qualified Data.Text as T
import Data.Validation
import Network.URI (URI, parseAbsoluteURI)
import RIO hiding (logError, logInfo)
import qualified RIO.Map as Map
import RIO.Process

envGroupId :: T.Text
envGroupId = "GITLAB_GROUP_ID"

envApiToken :: T.Text
envApiToken = "GITLAB_API_TOKEN"

envBaseUrl :: T.Text
envBaseUrl = "GITLAB_BASE_URL"

envDataUpdateInterval :: T.Text
envDataUpdateInterval = "DATA_UPDATE_INTERVAL_MINS"

envUiUpdateInterval :: T.Text
envUiUpdateInterval = "UI_UPDATE_INTERVAL_SECS"

envMaxConcurrency :: T.Text
envMaxConcurrency = "MAX_CONCURRENCY"

parseConfigFromEnv :: ProcessContext -> Validation (NonEmpty ConfigError) Config
parseConfigFromEnv pc =
  Config <$> readApiTokenFromEnv pc
    <*> readGroupIdFromEnv pc
    <*> readBaseUrlFromEnv pc
    <*> pure (readDataUpdateIntervalFromEnv pc)
    <*> pure (readUiUpdateIntervalFromEnv pc)
    <*> pure (readMaxConcurrencyFromEnv pc)

showErrors :: NonEmpty ConfigError -> T.Text
showErrors errs = T.intercalate ", " $ fmap tshow (toList errs)

data Config = Config
  { apiToken :: ApiToken,
    groupId :: GroupId,
    gitlabBaseUrl :: BaseUrl,
    dataUpdateIntervalMins :: DataUpdateIntervalMinutes,
    uiUpdateIntervalSecs :: UiUpdateIntervalSeconds,
    maxConcurrency :: MaxConcurrency
  }

instance Show Config where
  show Config {..} =
    "Config: GroupId "
      <> show groupId
      <> ", Base URL "
      <> show gitlabBaseUrl
      <> ", "
      <> show dataUpdateIntervalMins
      <> ", "
      <> show uiUpdateIntervalSecs
      <> ", "
      <> show maxConcurrency

newtype ApiToken = ApiToken B.ByteString

newtype BaseUrl = BaseUrl URI deriving newtype (Show)

newtype UiUpdateIntervalSeconds = UiUpdateIntervalSeconds Int deriving (Show)

data ConfigError = ApiTokenMissing | GroupIdMissing | GitlabBaseUrlMissing | GitlabBaseUrlInvalid T.Text

instance Show ConfigError where
  show ApiTokenMissing = "API Token is missing. Set it via " <> show envApiToken
  show GroupIdMissing = "Group ID is missing. Set it via " <> show envGroupId
  show GitlabBaseUrlMissing = "Gitlab base URL is missing. Set it via " <> show envBaseUrl
  show (GitlabBaseUrlInvalid url) = "Gitlab base URL set via " <> show envBaseUrl <> "is invalid. The value is: " <> show url

readApiTokenFromEnv :: ProcessContext -> Validation (NonEmpty ConfigError) ApiToken
readApiTokenFromEnv pc = do
  let maybeGroupId = encodeUtf8 <$> envFromPC pc envApiToken
  maybe (_Failure # single ApiTokenMissing) (\token -> _Success # ApiToken token) maybeGroupId

readGroupIdFromEnv :: ProcessContext -> Validation (NonEmpty ConfigError) GroupId
readGroupIdFromEnv pc = do
  let maybeGroupId = do
        groupIdString <- T.unpack <$> envFromPC pc envGroupId
        readMaybe groupIdString
  maybe (_Failure # single GroupIdMissing) (\gId -> _Success # GroupId gId) maybeGroupId

readBaseUrlFromEnv :: ProcessContext -> Validation (NonEmpty ConfigError) BaseUrl
readBaseUrlFromEnv pc = do
  let valueFromEnv = envFromPC pc envBaseUrl
  let baseUrl = valueFromEnv >>= parseAbsoluteURI . T.unpack
  if isJust baseUrl
    then maybe urlMissing (\url -> _Success # BaseUrl url) baseUrl
    else maybe urlMissing (\s -> _Failure # single (GitlabBaseUrlInvalid s)) valueFromEnv
  where
    urlMissing = _Failure # single GitlabBaseUrlMissing

readDataUpdateIntervalFromEnv :: ProcessContext -> DataUpdateIntervalMinutes
readDataUpdateIntervalFromEnv pc = DataUpdateIntervalMinutes $ fromMaybe 1 maybeUpdateInterval
  where
    maybeUpdateInterval = find (> 0) (envFromPC pc envDataUpdateInterval >>= (readMaybe . T.unpack))

readUiUpdateIntervalFromEnv :: ProcessContext -> UiUpdateIntervalSeconds
readUiUpdateIntervalFromEnv pc = UiUpdateIntervalSeconds $ fromMaybe 5 maybeUpdateInterval
  where
    maybeUpdateInterval = find (> 0) (envFromPC pc envUiUpdateInterval >>= (readMaybe . T.unpack))

readMaxConcurrencyFromEnv :: ProcessContext -> MaxConcurrency
readMaxConcurrencyFromEnv pc = MaxConcurrency $ fromMaybe 5 maybeMaxConcurrency
  where
    maybeMaxConcurrency = find (> 0) (envFromPC pc envMaxConcurrency >>= (readMaybe . T.unpack))

single :: a -> NonEmpty a
single a = a :| []

envFromPC :: ProcessContext -> Text -> Maybe Text
envFromPC pc key = Map.lookup key envVars
  where
    envVars = RIO.view envVarsL pc
