{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config.Config
  ( ApiToken (..),
    Config (..),
    ConfigError (..),
    GitlabHost,
    MaxConcurrency (..),
    UiUpdateIntervalSeconds (..),
    ProjectCacheTtlSeconds (..),
    SharedProjects (..),
    parseConfigFromEnv,
    Validation (Failure, Success),
    showErrors,
  )
where

import Control.Lens
import Core.Lib (DataUpdateIntervalSeconds (..), Group, Id (..), Url (..))
import qualified Data.ByteString as B hiding (pack)
import Data.Char (toLower)
import qualified Data.Text as T (intercalate)
import Data.Validation
import Network.URI (parseAbsoluteURI)
import Relude hiding (lookupEnv)
import qualified Text.Show

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

envIncludeSharedProjects :: Text
envIncludeSharedProjects = "GCB_INCLUDE_SHARED_PROJECTS"

parseConfigFromEnv :: [(String, String)] -> Validation (NonEmpty ConfigError) Config
parseConfigFromEnv env =
  Config <$> readApiTokenFromEnv env
    <*> readGroupIdFromEnv env
    <*> readBaseUrlFromEnv env
    <*> pure (readDataUpdateIntervalFromEnv env)
    <*> pure (readUiUpdateIntervalFromEnv env)
    <*> pure (readProjectCacheTtlSecondsFromEnv env)
    <*> pure (readMaxConcurrencyFromEnv env)
    <*> pure (readIncludeSharedProjectsFromEnv env)

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
    includeSharedProjects :: SharedProjects
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
          "Shared projects: " <> show includeSharedProjects
          -- coerce gitCommit
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

data SharedProjects = Include | Exclude deriving (Show)

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

readIncludeSharedProjectsFromEnv :: [(String, String)] -> SharedProjects
readIncludeSharedProjectsFromEnv env = fromMaybe Include (lookupEnv env envIncludeSharedProjects >>= (parse . toString))
  where
    parse s | (toLower <$> s) == "include" = Just Include
    parse s | (toLower <$> s) == "exclude" = Just Exclude
    parse _ = Nothing

parsePositiveWithDefault :: (Ord a, Num a, Read a) => [(String, String)] -> Text -> a -> a
parsePositiveWithDefault env text fallback = fromMaybe fallback $ find (> 0) (lookupEnv env text >>= (readMaybe . toString))

single :: a -> NonEmpty a
single a = a :| []

lookupEnv :: [(String, String)] -> Text -> Maybe Text
lookupEnv env key = fromString . snd <$> find (\(k, _) -> k == toString key) env
