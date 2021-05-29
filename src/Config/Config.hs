{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
import qualified Data.ByteString as B hiding (pack)
import Data.Char (toLower)
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

data ConfigH f = ConfigH
  { apiTokenH :: f ApiToken,
    groupIdH :: f (Id Group),
    gitlabBaseUrlH :: f (Url GitlabHost),
    dataUpdateIntervalSecsH :: f DataUpdateIntervalSeconds,
    uiUpdateIntervalSecsH :: f UiUpdateIntervalSeconds,
    projectCacheTtlSecsH :: f ProjectCacheTtlSeconds,
    maxConcurrencyH :: f MaxConcurrency,
    includeSharedProjectsH :: f SharedProjects,
    logLevelH :: f Severity
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

instance (Alternative f) => Semigroup (ConfigH f) where
  (<>) = bzipWith (<|>)

instance (Alternative f) => Monoid (ConfigH f) where
  mempty = bpure empty

envVars :: ConfigH (Const String)
envVars =
  ConfigH
    { apiTokenH = "GCB_GITLAB_API_TOKEN",
      groupIdH = "GCB_GITLAB_GROUP_ID",
      gitlabBaseUrlH = "GCB_GITLAB_BASE_URL",
      dataUpdateIntervalSecsH = "GCB_DATA_UPDATE_INTERVAL_SECS",
      uiUpdateIntervalSecsH = "GCB_UI_UPDATE_INTERVAL_SECS",
      projectCacheTtlSecsH = "GCB_PROJECT_CACHE_TTL_SECS",
      maxConcurrencyH = "GCB_MAX_CONCURRENCY",
      includeSharedProjectsH = "GCB_INCLUDE_SHARED_PROJECTS",
      logLevelH = "GCB_LOG_LEVEL"
    }

errorMessages :: ConfigH (Const String)
errorMessages = bzipWith (\(Const s1) (Const s2) -> Const $ s1 <> " (set it via " <> s2 <> ")") msgs envVars
  where
    msgs =
      ConfigH
        { apiTokenH = "Gitlab API Token is missing",
          groupIdH = "Group ID is missing",
          gitlabBaseUrlH = "Gitlab base URL is missing",
          dataUpdateIntervalSecsH = "Data Update interval is missing. Must be a positive integer",
          uiUpdateIntervalSecsH = "UI Update interval is missing. Must be a positive integer",
          projectCacheTtlSecsH = "Project cache list TTL is missing. Must be a positive integer",
          maxConcurrencyH = "Max concurrency is missing. Must be a positive integer",
          includeSharedProjectsH = "Configuration whether to include shared projects is missing. Possible values are `include`, `exclude`",
          logLevelH = "Log level is missing. Possible values are `DEBUG`, `INFO`, `WARN`, `ERROR`"
        }

parse :: ConfigH (Compose ((->) String) Maybe)
parse =
  ConfigH
    { apiTokenH = Compose $ Just . ApiToken . encodeUtf8,
      groupIdH = Compose $ fmap Id . readMaybe,
      gitlabBaseUrlH = Compose $ fmap Url . parseAbsoluteURI,
      dataUpdateIntervalSecsH = readPositive DataUpdateIntervalSeconds,
      uiUpdateIntervalSecsH = readPositive UiUpdateIntervalSeconds,
      projectCacheTtlSecsH = readPositive ProjectCacheTtlSeconds,
      maxConcurrencyH = readPositive MaxConcurrency,
      includeSharedProjectsH = Compose parseIncludeSharedProjects,
      logLevelH = Compose parseLogLevel
    }

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
  mempty
    { dataUpdateIntervalSecsH = Just 60,
      uiUpdateIntervalSecsH = Just 5,
      projectCacheTtlSecsH = Just 0,
      maxConcurrencyH = Just 2,
      includeSharedProjectsH = Just Include,
      logLevelH = Just InfoS
    }

unwrap :: ConfigH Identity -> Config
unwrap
  ( ConfigH
      (Identity apiTokenX)
      (Identity groupIdX)
      (Identity gitlabBaseUrlX)
      (Identity dataUpdateIntervalSecsX)
      (Identity uiUpdateIntervalSecsX)
      (Identity projectCacheTtlSecsX)
      (Identity maxConcurrencyX)
      (Identity includeSharedProjectsX)
      (Identity logLevelX)
    ) = Config apiTokenX groupIdX gitlabBaseUrlX dataUpdateIntervalSecsX uiUpdateIntervalSecsX projectCacheTtlSecsX maxConcurrencyX includeSharedProjectsX logLevelX

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
