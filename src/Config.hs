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
  , parseConfig
  ) where

import qualified Data.ByteString         as B hiding (pack)
import           Data.Either.Combinators
import           Data.Maybe
import           Env
import           Network.HTTP.Simple     (parseRequest)

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

parseConfig :: IO Config
parseConfig =
  Env.parse (header "config parser" . handleError configErrorHandler) $
  Config <$> var ((Right . ApiToken) <=< str <=< nonempty) envApiToken (help "gitlab api token") <*>
  var (parseGroupId <=< auto) envGroupId (help "gitlab group id") <*>
  var parseBaseUrl envBaseUrl (help "base url") <*>
  var parseDataUpdateInterval envDataUpdateInterval (help "data update interval") <*>
  var parseUiUpdateInterval envUiUpdateInterval (help "ui update interval")

configErrorHandler :: ErrorHandler ConfigError
--  case err of
--    NonPositive n -> Just (printf "  %s must be > 0, but is %d" name n)
--    _             -> defaultErrorHandler name err
configErrorHandler name err = Just $ show err -- TODO: lriedisser 2020-03-11 was ist name?

instance AsUnset ConfigError where
  unset = EnvError unset
  tryUnset err =
    case err of
      EnvError err' -> tryUnset err'
      _             -> Nothing

instance AsEmpty ConfigError where
  empty = EnvError empty
  tryEmpty err =
    case err of
      EnvError err' -> tryEmpty err'
      _             -> Nothing

instance AsUnread ConfigError where
  unread = EnvError . unread
  tryUnread err =
    case err of
      EnvError err' -> tryUnread err'
      _             -> Nothing

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
  | GroupIdInvalid
  | GitlabBaseUrlMissing
  | GitlabBaseUrlInvalid String
  | UiUpdateIntervalInvalid
  | DataUpdateIntervalInvalid
  | EnvError Error

instance Show ConfigError where
  show ApiTokenMissing = unwords ["API Token is missing. Set it via", envApiToken]
  show GroupIdInvalid = unwords ["Group ID is missing. Set it via", envGroupId]
  show GitlabBaseUrlMissing = unwords ["Gitlab base URL is missing. Set it via", envBaseUrl]
  show (GitlabBaseUrlInvalid url) = unwords ["Gitlab base URL set via", envBaseUrl, "is invalid. The value is:", url]
  show UiUpdateIntervalInvalid = "update interval for updating the UI must be positive and non-zero"
  show DataUpdateIntervalInvalid = "update interval for updating the UI must be positive and non-zero"

parseDataUpdateInterval :: String -> Either ConfigError DataUpdateIntervalMinutes
parseDataUpdateInterval = auto >=> filterNonPositive >=> (Right . DataUpdateIntervalMinutes)
  where
    filterNonPositive i = maybeToRight DataUpdateIntervalInvalid (positive i)

parseUiUpdateInterval :: String -> Either ConfigError UiUpdateIntervalSeconds
parseUiUpdateInterval = auto >=> filterNonPositive >=> (Right . UiUpdateIntervalSeconds)
  where
    filterNonPositive i = maybeToRight UiUpdateIntervalInvalid (positive i)

parseGroupId :: Int -> Either ConfigError GroupId
parseGroupId i = GroupId <$> maybeToRight GroupIdInvalid (positive i)

parseBaseUrl :: String -> Either ConfigError BaseUrl
parseBaseUrl = nonempty >=> isValidUrl
  where
    isValidUrl s
      | isJust (parseRequest s) = (Right . BaseUrl) s
      | otherwise = Left (GitlabBaseUrlInvalid s)

positive :: Int -> Maybe Int
positive n
  | n <= 0 = Nothing
  | otherwise = return n
