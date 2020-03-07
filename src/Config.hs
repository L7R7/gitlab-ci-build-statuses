{-# LANGUAGE OverloadedStrings #-}

module Config
  ( ApiToken(..)
  , BaseUrl(..)
  , Config(..)
  , GroupId(..)
  , ProjectId(..)
  , ConfigError(..)
  , parseConfigFromEnv
  , showErrors
  ) where

import           Control.Lens
import qualified Data.ByteString       as B hiding (pack)
import           Data.ByteString.Char8 (pack)
import           Data.List.NonEmpty    hiding (group)
import           Data.Maybe
import           Data.Validation
import           Network.HTTP.Simple   (parseRequest)
import           System.Environment
import           Text.Read

envGroupId :: String
envGroupId = "GITLAB_GROUP_ID"

envApiToken :: String
envApiToken = "GITLAB_API_TOKEN"

envBaseUrl :: String
envBaseUrl = "GITLAB_BASE_URL"

data Config =
  Config
    { apiToken      :: ApiToken
    , groupId       :: GroupId
    , gitlabBaseUrl :: BaseUrl
    }

newtype ApiToken =
  ApiToken B.ByteString

newtype ProjectId =
  ProjectId Int

newtype GroupId =
  GroupId Int

newtype BaseUrl =
  BaseUrl String

parseConfigFromEnv :: IO (Validation (NonEmpty ConfigError) Config)
parseConfigFromEnv = do
  token <- readApiTokenFromEnv
  group <- readGroupIdFromEnv
  baseUrl <- readBaseUrlFromEnv
  pure $ Config <$> token <*> group <*> baseUrl

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
  pure $
    case maybeGroupId of
      Nothing    -> _Failure # single ApiTokenMissing
      Just token -> _Success # (ApiToken $ pack token)

readGroupIdFromEnv :: IO (Validation (NonEmpty ConfigError) GroupId)
readGroupIdFromEnv = do
  maybeGroupIdString <- lookupEnv envGroupId
  let maybeGroupId = maybeGroupIdString >>= readMaybe
  pure $
    case maybeGroupId of
      Nothing      -> _Failure # single GroupIdMissing
      Just groupId -> _Success # GroupId groupId

readBaseUrlFromEnv :: IO (Validation (NonEmpty ConfigError) BaseUrl)
readBaseUrlFromEnv = do
  maybeBaseUrl <- lookupEnv envBaseUrl
  let urlValid = isJust $ maybeBaseUrl >>= parseRequest
  pure $
    if urlValid
      then maybe (_Failure # single GitlabBaseUrlMissing) (\s -> _Success # BaseUrl s) maybeBaseUrl
      else maybe (_Failure # single GitlabBaseUrlMissing) (\s -> _Failure # single (GitlabBaseUrlInvalid s)) maybeBaseUrl

single :: a -> NonEmpty a
single a = a :| []

showErrors :: NonEmpty ConfigError -> String
showErrors errs = unwords $ fmap show (toList errs)
