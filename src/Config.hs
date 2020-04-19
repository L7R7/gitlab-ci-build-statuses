{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config
  ( ApiToken (..),
    BaseUrl (..),
    Config (..),
    ConfigError (..),
    DataUpdateIntervalMinutes (..),
    GroupId (..),
    ProjectId (..),
    UiUpdateIntervalSeconds (..),
    parseConfigFromEnv,
    showErrors,
  )
where

import Colog (LoggerT, Message, logError, logInfo)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B hiding (pack)
import Data.ByteString.Char8 (pack)
import Data.List.NonEmpty hiding (group, toList)
import Data.Maybe
import qualified Data.Text as T
import Data.Validation
import Network.HTTP.Simple (parseRequest)
import RIO hiding (logError, logInfo)
import System.Environment
import Text.Read
import TextShow

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

parseConfigFromEnv :: LoggerT Message IO (Validation (NonEmpty ConfigError) Config)
parseConfigFromEnv = do
  config <- liftIO parseConfig
  case config of
    Failure errs -> logError $ "Failed to parse config: " <> showErrors errs
    Success conf -> logInfo $ "Parsed config: " <> showt conf
  liftIO $ pure config

parseConfig :: IO (Validation (NonEmpty ConfigError) Config)
parseConfig = do
  token <- readApiTokenFromEnv
  group <- readGroupIdFromEnv
  baseUrl <- readBaseUrlFromEnv
  dataUpdateInterval <- readDataUpdateIntervalFromEnv
  uiUpdateInterval <- readUiUpdateIntervalFromEnv
  pure $ Config <$> token <*> group <*> baseUrl <*> pure dataUpdateInterval <*> pure uiUpdateInterval

showErrors :: NonEmpty ConfigError -> T.Text
showErrors errs = T.intercalate ", " $ fmap showt (toList errs)

data Config
  = Config
      { apiToken :: ApiToken,
        groupId :: GroupId,
        gitlabBaseUrl :: BaseUrl,
        dataUpdateIntervalMins :: DataUpdateIntervalMinutes,
        uiUpdateIntervalSecs :: UiUpdateIntervalSeconds
      }

instance TextShow Config where
  showb (Config _ group baseUrl dataUpdate uiUpdate) =
    showb ("Config: GroupId" :: String)
      <> showbSpace
      <> (showb . show) group
      <> showbCommaSpace
      <> showb ("Base URL" :: String)
      <> showbSpace
      <> (showb . show) baseUrl
      <> showbSpace
      <> showb ("Data Update interval(mins)" :: String)
      <> showbSpace
      <> (showb . show) dataUpdate
      <> showbSpace
      <> showbSpace
      <> showb ("UI interval(secs)" :: String)
      <> showbSpace
      <> (showb . show) uiUpdate

newtype ApiToken = ApiToken B.ByteString

newtype ProjectId = ProjectId Int deriving (Show)

newtype GroupId = GroupId Int deriving (Show)

newtype BaseUrl = BaseUrl String deriving (Show)

newtype DataUpdateIntervalMinutes = DataUpdateIntervalMinutes Int deriving (Show)

newtype UiUpdateIntervalSeconds = UiUpdateIntervalSeconds Int deriving (Show)

data ConfigError = ApiTokenMissing | GroupIdMissing | GitlabBaseUrlMissing | GitlabBaseUrlInvalid String

instance TextShow ConfigError where
  showb ApiTokenMissing = showb ("API Token is missing. Set it via" :: String) <> showbSpace <> showb envApiToken
  showb GroupIdMissing = showb ("Group ID is missing. Set it via" :: String) <> showbSpace <> showb envGroupId
  showb GitlabBaseUrlMissing = showb ("Gitlab base URL is missing. Set it via" :: String) <> showbSpace <> showb envBaseUrl
  showb (GitlabBaseUrlInvalid url) = showb ("Gitlab base URL set via" :: String) <> showbSpace <> showb envBaseUrl <> showb ("is invalid. The value is:" :: String) <> showb url

readApiTokenFromEnv :: IO (Validation (NonEmpty ConfigError) ApiToken)
readApiTokenFromEnv = do
  maybeGroupId <- lookupEnv envApiToken
  pure $ maybe (_Failure # single ApiTokenMissing) (\token -> _Success # ApiToken (pack token)) maybeGroupId

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
