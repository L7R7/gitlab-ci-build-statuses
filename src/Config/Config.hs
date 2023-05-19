{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Config
  ( ApiToken (..),
    Config (..),
    GitlabHost,
    MaxConcurrency (..),
    UiUpdateIntervalSeconds (..),
    ProjectCacheTtlSeconds (..),
    RunnerCacheTtlSeconds (..),
    SharedProjects (..),
    JobsView (..),
    parseConfigFromEnv,
  )
where

import Barbies
import Config.Util
import Control.Lens
import Core.BuildStatuses (Project)
import Core.Shared (DataUpdateIntervalSeconds (..), Group, Id (..), Url (..))
import Data.Biapplicative
import Data.ByteString qualified as B hiding (pack)
import Data.Char (toLower)
import Data.Generic.HKD
import Data.List.Extra (splitOn)
import Katip (Severity (..))
import Network.URI (parseAbsoluteURI)
import Relude hiding (lookupEnv)
import Text.Printf
import Text.Show qualified (show)
import Validation

data Config = Config
  { apiToken :: ApiToken,
    groupId :: NonEmpty (Id Group),
    gitlabBaseUrl :: Url GitlabHost,
    dataUpdateIntervalSecs :: DataUpdateIntervalSeconds,
    uiUpdateIntervalSecs :: UiUpdateIntervalSeconds,
    projectCacheTtlSecs :: ProjectCacheTtlSeconds,
    runnerCacheTtlSecs :: RunnerCacheTtlSeconds,
    maxConcurrency :: MaxConcurrency,
    includeSharedProjects :: SharedProjects,
    logLevel :: Severity,
    projectExcludeList :: [Id Project],
    jobsView :: JobsView
  }
  deriving stock (Eq, Generic)

instance Show Config where
  show Config {..} =
    "Config: "
      <> intercalate
        ", "
        [ "GroupIds: " <> show groupId,
          "Base URL: " <> show gitlabBaseUrl,
          show dataUpdateIntervalSecs,
          show uiUpdateIntervalSecs,
          show projectCacheTtlSecs,
          show runnerCacheTtlSecs,
          show maxConcurrency,
          "Shared projects: " <> show includeSharedProjects,
          "Log level: " <> show logLevel,
          "Excluded projects: " <> show projectExcludeList,
          "Jobs view: " <> show jobsView
        ]

newtype ApiToken = ApiToken B.ByteString deriving newtype (Eq)

data GitlabHost

newtype UiUpdateIntervalSeconds = UiUpdateIntervalSeconds Int
  deriving stock (Show)
  deriving (Num) via Int
  deriving newtype (Eq)

newtype ProjectCacheTtlSeconds = ProjectCacheTtlSeconds Int64
  deriving stock (Show)
  deriving (Num) via Int64
  deriving newtype (Eq)

newtype RunnerCacheTtlSeconds = RunnerCacheTtlSeconds Int64
  deriving stock (Show)
  deriving (Num) via Int64
  deriving newtype (Eq)

newtype MaxConcurrency = MaxConcurrency Int
  deriving stock (Show)
  deriving (Num) via Int
  deriving newtype (Eq)

newtype GitCommit = GitCommit String deriving stock (Show)

data SharedProjects = Include | Exclude deriving stock (Eq, Show)

data JobsView = Enabled | Disabled deriving stock (Eq, Show)

type ConfigH f = HKD Config f

envVarNames :: ConfigH (Const EnvVariableName)
envVarNames =
  bmap (first EnvVariableName)
    $ build @Config
      "GCB_GITLAB_API_TOKEN"
      "GCB_GITLAB_GROUP_ID"
      "GCB_GITLAB_BASE_URL"
      "GCB_DATA_UPDATE_INTERVAL_SECS"
      "GCB_UI_UPDATE_INTERVAL_SECS"
      "GCB_PROJECT_CACHE_TTL_SECS"
      "GCB_RUNNER_CACHE_TTL_SECS"
      "GCB_MAX_CONCURRENCY"
      "GCB_INCLUDE_SHARED_PROJECTS"
      "GCB_LOG_LEVEL"
      "GCB_EXCLUDE_PROJECTS"
      "GCB_JOBS_VIEW"

errorMessages :: ConfigH (Const ErrorMessage)
errorMessages = bzipWith (biliftA2 (printf "%s (set it via %s)") const) msgs envVarNames
  where
    msgs :: ConfigH (Const ErrorMessage)
    msgs =
      bmap (first ErrorMessage)
        $ build @Config
          "Gitlab API Token is missing"
          "Group ID is missing"
          "Gitlab base URL is missing"
          "Data Update interval is missing. Must be a positive integer"
          "UI Update interval is missing. Must be a positive integer"
          "Project list cache TTL is missing. Must be a positive integer"
          "Runner list cache TTL is missing. Must be a positive integer"
          "Max concurrency is missing. Must be a positive integer"
          "Configuration whether to include shared projects is missing. Possible values are `include`, `exclude`"
          "Log level is missing. Possible values are `DEBUG`, `INFO`, `WARN`, `ERROR`"
          "List of projects to exclude is missing. Must be a list of comma-separated string of numbers that indicate the project IDs"
          "Configuration whether to enable runners jobs is missing. Possible values are `enabled`, `disabled`"

parse :: ConfigH (Compose ((->) String) Maybe)
parse =
  build @Config
    (Compose parseApiToken)
    (Compose parseGroupsList)
    (Compose $ fmap Url . parseAbsoluteURI)
    (readPositive DataUpdateIntervalSeconds)
    (readPositive UiUpdateIntervalSeconds)
    (readPositive ProjectCacheTtlSeconds)
    (readPositive RunnerCacheTtlSeconds)
    (readPositive MaxConcurrency)
    (Compose parseIncludeSharedProjects)
    (Compose parseLogLevel)
    (Compose parseProjectExcludeList)
    (Compose parseJobsView)

parseApiToken :: String -> Maybe ApiToken
parseApiToken "" = Nothing
parseApiToken s = Just $ ApiToken $ encodeUtf8 s

parseIncludeSharedProjects :: String -> Maybe SharedProjects
parseIncludeSharedProjects s | (toLower <$> s) == "include" = Just Include
parseIncludeSharedProjects s | (toLower <$> s) == "exclude" = Just Exclude
parseIncludeSharedProjects _ = Nothing

parseLogLevel :: String -> Maybe Severity
parseLogLevel "DEBUG" = Just DebugS
parseLogLevel "INFO" = Just InfoS
parseLogLevel "WARN" = Just WarningS
parseLogLevel "ERROR" = Just ErrorS
parseLogLevel _ = Nothing

parseGroupsList :: String -> Maybe (NonEmpty (Id Group))
parseGroupsList s = do
  groups <- traverse (fmap Id . find (> 0) . readMaybe) (splitOn "," s)
  nonEmpty $ ordNub groups

parseProjectExcludeList :: String -> Maybe [Id Project]
parseProjectExcludeList s = ordNub <$> traverse (fmap Id . readMaybe) (splitOn "," s)

parseJobsView :: String -> Maybe JobsView
parseJobsView s | (toLower <$> s) == "enabled" = Just Enabled
parseJobsView s | (toLower <$> s) == "disabled" = Just Disabled
parseJobsView _ = Nothing

readPositive :: (Ord a, Num a, Read a) => (a -> b) -> Compose ((->) String) Maybe b
readPositive f = Compose $ fmap f . find (> 0) . readMaybe

defaults :: ConfigH Maybe
defaults =
  bpure empty
    & field @"dataUpdateIntervalSecs" .~ Just 60
    & field @"uiUpdateIntervalSecs" .~ Just 5
    & field @"projectCacheTtlSecs" .~ Just 3600
    & field @"runnerCacheTtlSecs" .~ Just 300
    & field @"maxConcurrency" .~ Just 2
    & field @"includeSharedProjects" .~ Just Include
    & field @"logLevel" .~ Just InfoS
    & field @"projectExcludeList" .~ Just []
    & field @"jobsView" .~ Just Enabled

parseConfigFromEnv :: [(String, String)] -> Validation (NonEmpty Text) Config
parseConfigFromEnv env = parseConfig envVarNames errorMessages defaults parse (first EnvVariableName <$> env)
