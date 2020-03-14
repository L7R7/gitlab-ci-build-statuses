{-# LANGUAGE OverloadedStrings #-}

module Config
  ( ApiToken(..)
  , BaseUrl(..)
  , Config(..)
  , ConfigError(..)
  , DataUpdateIntervalMinutes(..)
  , GroupId(..)
  , ProjectId(..)
  , UiUpdateIntervalSeconds(..)
  , parseConfigFromEnv
  , showErrors
  ) where

import           Control.Lens
import qualified Data.ByteString         as B hiding (pack)
import           Data.ByteString.Char8   (pack)
import           Data.Either.Combinators
import           Data.List.NonEmpty      hiding (group)
import           Data.Maybe
import           Data.Validation
import           Env
import           Network.HTTP.Simple     (parseRequest)
import           System.Environment
import           Text.Read

envGroupId :: String
envGroupId = "GITLAB_GROUP_ID"

envApiToken :: String
envApiToken = "GITLAB_API_TOKEN"

envBaseUrl :: String
envBaseUrl = "GITLAB_BASE_URL"

envDataUpdateInterval :: String
envDataUpdateInterval = "DATA_UPDATE_INTERVAL_MINS"

envUiUpdateInterval :: String
envUiUpdateInterval = "UI_UPDATE_INTERVAL_SECS"

parseConfigFromEnv :: IO (Validation (NonEmpty ConfigError) Config)
parseConfigFromEnv = do
  token <- readApiTokenFromEnv
  group <- readGroupIdFromEnv
  baseUrl <- readBaseUrlFromEnv
  dataUpdateInterval <- readDataUpdateIntervalFromEnv
  uiUpdateInterval <- readUiUpdateIntervalFromEnv
  pure $ Config <$> token <*> group <*> baseUrl <*> pure dataUpdateInterval <*> pure uiUpdateInterval

data Config =
  Config
    { apiToken               :: ApiToken
    , groupId                :: GroupId
    , gitlabBaseUrl          :: BaseUrl
    , dataUpdateIntervalMins :: DataUpdateIntervalMinutes
    , uiUpdateIntervalSecs   :: UiUpdateIntervalSeconds
    }

newtype ApiToken =
  ApiToken B.ByteString

newtype ProjectId =
  ProjectId Int

newtype GroupId =
  GroupId Int

newtype BaseUrl =
  BaseUrl String

newtype DataUpdateIntervalMinutes =
  DataUpdateIntervalMinutes Int

newtype UiUpdateIntervalSeconds =
  UiUpdateIntervalSeconds Int

data ConfigError
  = ApiTokenMissing
  | GroupIdMissing
  | GitlabBaseUrlMissing
  | GitlabBaseUrlInvalid String

instance Show ConfigError where
  show ApiTokenMissing = unwords ["API Token is missing. Set it via", envApiToken]
  show GroupIdMissing = unwords ["Group ID is missing. Set it via", envGroupId]
  show GitlabBaseUrlMissing = unwords ["Gitlab base URL is missing. Set it via", envBaseUrl]
  show (GitlabBaseUrlInvalid url) = unwords ["Gitlab base URL set via", envBaseUrl, "is invalid. The value is:", url]

readApiTokenFromEnv :: IO (Validation (NonEmpty ConfigError) ApiToken)
readApiTokenFromEnv = do
  maybeGroupId <- lookupEnv envApiToken
  pure $ maybe (_Failure # single ApiTokenMissing) (\token -> _Success # (ApiToken $ pack token)) maybeGroupId

readGroupIdFromEnv :: IO (Validation (NonEmpty ConfigError) GroupId)
readGroupIdFromEnv = do
  maybeGroupIdString <- lookupEnv envGroupId
  let maybeGroupId = maybeGroupIdString >>= readMaybe
  pure $ maybe (_Failure # single GroupIdMissing) (\gId -> _Success # GroupId gId) maybeGroupId

readBaseUrlFromEnv :: IO (Validation (NonEmpty ConfigError) BaseUrl)
readBaseUrlFromEnv = do
  maybeBaseUrl <- lookupEnv envBaseUrl
  let urlValid = isJust $ maybeBaseUrl >>= parseRequest
  pure $
    if urlValid
      then maybe urlMissing (\s -> _Success # BaseUrl s) maybeBaseUrl
      else maybe urlMissing (\s -> _Failure # single (GitlabBaseUrlInvalid s)) maybeBaseUrl
  where
    urlMissing = _Failure # single GitlabBaseUrlMissing

readDataUpdateIntervalFromEnv :: IO DataUpdateIntervalMinutes
readDataUpdateIntervalFromEnv = do
  maybeUpdateIntervalString <- lookupEnv envDataUpdateInterval
  let maybeUpdateInterval = maybeUpdateIntervalString >>= readMaybe >>= filterUpdateInterval
  pure $ DataUpdateIntervalMinutes $ fromMaybe 5 maybeUpdateInterval

readUiUpdateIntervalFromEnv :: IO UiUpdateIntervalSeconds
readUiUpdateIntervalFromEnv = do
  maybeUpdateIntervalString <- lookupEnv envUiUpdateInterval
  let maybeUpdateInterval = maybeUpdateIntervalString >>= readMaybe >>= filterUpdateInterval
  pure $ UiUpdateIntervalSeconds $ fromMaybe 60 maybeUpdateInterval

filterUpdateInterval :: Int -> Maybe Int
filterUpdateInterval i
  | i < 1 = Nothing
  | otherwise = Just i

single :: a -> NonEmpty a
single a = a :| []

showErrors :: NonEmpty ConfigError -> String
showErrors errs = unlines $ fmap show (toList errs)
