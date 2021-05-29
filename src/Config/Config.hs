{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config.Config
  ( ApiToken (..),
    Config (..),
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

import Barbies
import Control.Lens
import Core.Lib (DataUpdateIntervalSeconds (..), Group, Id (..), Url (..))
import Data.Biapplicative
import qualified Data.ByteString as B hiding (pack)
import Data.Char (toLower)
import Data.Generic.HKD
import qualified Data.Text as T (intercalate)
import Data.Validation
import Katip (Severity (..))
import Network.URI (parseAbsoluteURI)
import Relude hiding (lookupEnv)
import qualified Text.Show (show)

data Config = Config
  { apiToken :: ApiToken,
    groupId :: Id Group,
    gitlabBaseUrl :: Url GitlabHost,
    dataUpdateIntervalSecs :: DataUpdateIntervalSeconds,
    uiUpdateIntervalSecs :: UiUpdateIntervalSeconds,
    projectCacheTtlSecs :: ProjectCacheTtlSeconds,
    maxConcurrency :: MaxConcurrency,
    includeSharedProjects :: SharedProjects,
    logLevel :: Severity
  }
  deriving (Generic)

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
          "Shared projects: " <> show includeSharedProjects,
          "Log level: " <> show logLevel
        ]

newtype ApiToken = ApiToken B.ByteString

data GitlabHost

newtype UiUpdateIntervalSeconds = UiUpdateIntervalSeconds Int
  deriving (Show)
  deriving (Num) via Int

newtype ProjectCacheTtlSeconds = ProjectCacheTtlSeconds Int64
  deriving (Show)
  deriving (Num) via Int64

newtype MaxConcurrency = MaxConcurrency Int
  deriving (Show)
  deriving (Num) via Int

newtype GitCommit = GitCommit String deriving (Show)

data SharedProjects = Include | Exclude deriving (Show)

type ConfigH f = HKD Config f

envVars :: ConfigH (Const String)
envVars =
  build @Config
    "GCB_GITLAB_API_TOKEN"
    "GCB_GITLAB_GROUP_ID"
    "GCB_GITLAB_BASE_URL"
    "GCB_DATA_UPDATE_INTERVAL_SECS"
    "GCB_UI_UPDATE_INTERVAL_SECS"
    "GCB_PROJECT_CACHE_TTL_SECS"
    "GCB_MAX_CONCURRENCY"
    "GCB_INCLUDE_SHARED_PROJECTS"
    "GCB_LOG_LEVEL"

errorMessages :: ConfigH (Const String)
errorMessages = bzipWith (biliftA2 (\errMsg envVar -> errMsg <> " (set it via " <> envVar <> ")") const) msgs envVars
  where
    msgs =
      build @Config
        "Gitlab API Token is missing"
        "Group ID is missing"
        "Gitlab base URL is missing"
        "Data Update interval is missing. Must be a positive integer"
        "UI Update interval is missing. Must be a positive integer"
        "Project cache list TTL is missing. Must be a positive integer"
        "Max concurrency is missing. Must be a positive integer"
        "Configuration whether to include shared projects is missing. Possible values are `include`, `exclude`"
        "Log level is missing. Possible values are `DEBUG`, `INFO`, `WARN`, `ERROR`"

parse :: ConfigH (Compose ((->) String) Maybe)
parse =
  build @Config
    (Compose $ Just . ApiToken . encodeUtf8)
    (Compose $ fmap Id . readMaybe)
    (Compose $ fmap Url . parseAbsoluteURI)
    (readPositive DataUpdateIntervalSeconds)
    (readPositive UiUpdateIntervalSeconds)
    (readPositive ProjectCacheTtlSeconds)
    (readPositive MaxConcurrency)
    (Compose parseIncludeSharedProjects)
    (Compose parseLogLevel)

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

readPositive :: (Ord a, Num a, Read a) => (a -> b) -> Compose ((->) String) Maybe b
readPositive f = Compose $ fmap f . find (> 0) . readMaybe

defaults :: ConfigH Maybe
defaults =
  bpure empty & field @"dataUpdateIntervalSecs" .~ Just 60
    & field @"uiUpdateIntervalSecs" .~ Just 5
    & field @"projectCacheTtlSecs" .~ Just 0
    & field @"maxConcurrency" .~ Just 2
    & field @"includeSharedProjects" .~ Just Include
    & field @"logLevel" .~ Just InfoS

unwrap :: ConfigH Identity -> Config
unwrap = runIdentity . construct

parseConfigFromEnv :: [(String, String)] -> Validation (NonEmpty Text) Config
parseConfigFromEnv env = unwrap <$> validateConfig errorMessages (parseConfigWithDefaults env)

validateConfig :: (ApplicativeB b, TraversableB b) => b (Const String) -> b Maybe -> Validation (NonEmpty Text) (b Identity)
validateConfig errMsgs mOpts = bsequence' $ bzipWith v mOpts errMsgs
  where
    v (Just x) _ = Success x
    v Nothing (Const err) = Failure $ fromString err :| []

parseConfigWithDefaults :: [(String, String)] -> ConfigH Maybe
parseConfigWithDefaults env = bzipWith (<|>) (fromEnv env envVars parse) defaults

fromEnv :: ApplicativeB b => [(String, String)] -> b (Const String) -> b (Compose ((->) String) Maybe) -> b Maybe
fromEnv env = bzipWith (\s (Compose f) -> lookupEnv env s >>= f)

lookupEnv :: [(String, String)] -> Const String k -> Maybe String
lookupEnv env (Const key) = snd <$> find ((== toString key) . fst) env

showErrors :: NonEmpty Text -> Text
showErrors errs = T.intercalate ", " (toList errs)
