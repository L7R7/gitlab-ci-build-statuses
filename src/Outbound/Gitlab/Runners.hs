{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Outbound.Gitlab.Runners (initCache, runnersApiToIO) where

import Burrito
import Config.Config (ApiToken (..), GitlabHost, RunnerCacheTtlSeconds (RunnerCacheTtlSeconds))
import Core.Runners (IpAddress (..), Job (..), Runner, RunnersApi (..), Stage (..))
import Core.Shared (Group, Id, Url (..))
import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Cache
import Metrics.Metrics (OutgoingHttpRequestsHistogram)
import Outbound.Gitlab.Helpers
import Outbound.Gitlab.Instances ()
import Polysemy
import Relude
import System.Clock

initCache :: RunnerCacheTtlSeconds -> IO (Cache (Id Group) [Runner])
initCache (RunnerCacheTtlSeconds ttl) = newCache (Just (TimeSpec ttl 0))

runnersApiToIO :: Member (Embed IO) r => Url GitlabHost -> ApiToken -> OutgoingHttpRequestsHistogram -> Cache (Id Group) [Runner] -> InterpreterFor RunnersApi r
runnersApiToIO baseUrl apiToken histogram cache = interpret $ \case
  GetOnlineRunnersForGroup groupId -> embed $ do
    cached <- lookup cache groupId
    case cached of
      (Just runners) -> pure $ Right runners
      Nothing -> do
        let template = [uriTemplate|/api/v4/groups/{groupId}/runners?status=online|]
        result <- fetchDataPaginated baseUrl apiToken template [("groupId", (stringValue . show) groupId)] groupId histogram
        traverse_ (insert cache groupId) result
        pure result
  GetRunningJobsForRunner groupId runnerId -> do
    let template = [uriTemplate|/api/v4/runners/{runnerId}/jobs?status=running|]
    embed $ fetchDataPaginated baseUrl apiToken template [("runnerId", (stringValue . show) runnerId)] groupId histogram

instance FromJSON Job where
  parseJSON = withObject "job" $ \job -> do
    jobId <- job .: "id"
    jobProjectId <- job .: "project" >>= \p -> p .: "id"
    jobProjectName <- job .: "project" >>= \p -> p .: "name"
    jobStage <- job .: "stage"
    jobName <- job .: "name"
    jobRef <- job .: "ref"
    jobWebUrl <- job .: "web_url"
    pure Job {..}

deriving newtype instance FromJSON Stage

deriving newtype instance FromJSON IpAddress

instance FromJSON Runner where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
