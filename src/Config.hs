{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config
  ( ApiToken (..),
    BaseUrl (..),
    Config (..),
    ConfigError (..),
    DataUpdateIntervalMinutes (..),
    GroupId (..),
    UiUpdateIntervalSeconds (..),
    parseConfigFromEnv,
    showErrors,
  )
where

import Control.Lens
import qualified Data.ByteString as B hiding (pack)
import Data.List.NonEmpty hiding (group, toList)
import Data.Maybe
import qualified Data.Text as T
import Data.Validation
import Network.HTTP.Simple (parseRequest)
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

parseConfigFromEnv :: ProcessContext -> Validation (NonEmpty ConfigError) Config
parseConfigFromEnv pc = Config <$> readApiTokenFromEnv pc <*> readGroupIdFromEnv pc <*> readBaseUrlFromEnv pc <*> pure (readDataUpdateIntervalFromEnv pc) <*> pure (readUiUpdateIntervalFromEnv pc)

showErrors :: NonEmpty ConfigError -> T.Text
showErrors errs = T.intercalate ", " $ fmap tshow (toList errs)

data Config
  = Config
      { apiToken :: ApiToken,
        groupId :: GroupId,
        gitlabBaseUrl :: BaseUrl,
        dataUpdateIntervalMins :: DataUpdateIntervalMinutes,
        uiUpdateIntervalSecs :: UiUpdateIntervalSeconds
      }

instance Show Config where
  show (Config _ group baseUrl dataUpdate uiUpdate) =
    "Config: GroupId "
      <> show group
      <> ", Base URL "
      <> show baseUrl
      <> ", Data Update interval(mins) "
      <> show dataUpdate
      <> ", UI interval(secs) "
      <> show uiUpdate

newtype ApiToken = ApiToken B.ByteString

newtype GroupId = GroupId Int deriving (Show)

newtype BaseUrl = BaseUrl String deriving (Show)

newtype DataUpdateIntervalMinutes = DataUpdateIntervalMinutes Int deriving (Show)

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
  let maybeBaseUrl = envFromPC pc envBaseUrl
  let urlValid = isJust $ maybeBaseUrl >>= (parseRequest . T.unpack)
  if urlValid
    then maybe urlMissing (\s -> _Success # BaseUrl (T.unpack s)) maybeBaseUrl
    else maybe urlMissing (\s -> _Failure # single (GitlabBaseUrlInvalid s)) maybeBaseUrl
  where
    urlMissing = _Failure # single GitlabBaseUrlMissing

readDataUpdateIntervalFromEnv :: ProcessContext -> DataUpdateIntervalMinutes
readDataUpdateIntervalFromEnv pc = DataUpdateIntervalMinutes $ fromMaybe 5 maybeUpdateInterval
  where
    maybeUpdateInterval = envFromPC pc envDataUpdateInterval >>= (readMaybe . T.unpack) >>= filterUpdateInterval

readUiUpdateIntervalFromEnv :: ProcessContext -> UiUpdateIntervalSeconds
readUiUpdateIntervalFromEnv pc = UiUpdateIntervalSeconds $ fromMaybe 60 maybeUpdateInterval
  where
    maybeUpdateInterval = envFromPC pc envUiUpdateInterval >>= (readMaybe . T.unpack) >>= filterUpdateInterval

filterUpdateInterval :: Int -> Maybe Int
filterUpdateInterval i
  | i < 1 = Nothing
  | otherwise = Just i

single :: a -> NonEmpty a
single a = a :| []

envFromPC :: ProcessContext -> Text -> Maybe Text
envFromPC pc key = Map.lookup key envVars
  where
    envVars = RIO.view envVarsL pc
