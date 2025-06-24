{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ports.Outbound.Gitlab.Runners (initCache, runnersApiToIO) where

import Burrito
import Config.Config (ApiToken (..), GitlabHost, RunnerCacheTtlSeconds (RunnerCacheTtlSeconds), UserAgent)
import Core.BuildStatuses
import Core.Runners (Description (..), IpAddress (..), Job (..), Runner, RunnersApi (..), Stage (..), Tag (..))
import Core.Shared (Group, Id, UpdateError, Url (..))
import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Cache
import Metrics.Metrics (CacheResult (..), CacheTag (CacheTag), MetricsApi, OutgoingHttpRequestsHistogram, recordCacheLookupResult)
import Polysemy
import Polysemy.Reader qualified as R
import Ports.Outbound.Gitlab.Helpers
import Ports.Outbound.Gitlab.Instances ()
import Relude
import System.Clock

initCache :: RunnerCacheTtlSeconds -> IO (Cache (Id Group) v)
initCache (RunnerCacheTtlSeconds ttl) = newCache (Just (TimeSpec ttl 0))

runnersApiToIO ::
  ( Member (Embed IO) r,
    Member MetricsApi r,
    Member ProjectsWithoutExcludesApi r,
    Member (R.Reader (Url GitlabHost)) r,
    Member (R.Reader ApiToken) r,
    Member (R.Reader UserAgent) r,
    Member (R.Reader OutgoingHttpRequestsHistogram) r,
    Member (R.Reader (Cache (Id Group) [Runner])) r,
    Member (R.Reader (Cache (Id Group) [(Id Project, [Runner])])) r
  ) =>
  InterpreterFor RunnersApi r
runnersApiToIO sem = do
  baseUrl <- R.ask
  apiToken <- R.ask
  userAgent <- R.ask
  histogram <- R.ask
  groupCache <- R.ask
  projectCache <- R.ask
  runnersApiToIO' baseUrl apiToken userAgent histogram groupCache projectCache sem

-- | this type acts as an intermediate data structure for first getting a list of active runners and
--   then fetching the details for each runner.
--   this is necessary because the list endpoint doesn't include the runner's tag list
newtype RunnerId = RunnerId (Id Runner)

instance FromJSON RunnerId where
  parseJSON = withObject "runner" $ \runner -> do
    runnerId <- runner .: "id"
    pure $ RunnerId runnerId

runnersApiToIO' ::
  ( Member (Embed IO) r,
    Member MetricsApi r,
    Member ProjectsWithoutExcludesApi r
  ) =>
  Url GitlabHost ->
  ApiToken ->
  UserAgent ->
  OutgoingHttpRequestsHistogram ->
  Cache (Id Group) [Runner] ->
  Cache (Id Group) [(Id Project, [Runner])] ->
  InterpreterFor RunnersApi r
runnersApiToIO' baseUrl apiToken userAgent histogram groupCache projectCache = interpret $ \case
  GetOnlineRunnersForGroup groupId -> do
    (result, cacheResult) <- embed $ do
      cached <- lookup groupCache groupId
      case cached of
        (Just runners) -> pure (Right runners, Hit)
        Nothing -> do
          let template = [uriTemplate|/api/v4/groups/{groupId}/runners?status=online&type=group_type|]
          runnerIds <- fetchDataPaginated @RunnerId baseUrl apiToken userAgent template [("groupId", (stringValue . show) groupId)] histogram
          result <- fetchRunnersForRunnerIds runnerIds
          traverse_ (insert groupCache groupId) result
          pure (result, Miss)
    recordCacheLookupResult (CacheTag "runners") cacheResult
    pure result
  GetProjectRunnersForGroup groupId -> do
    (result, cacheResult) <- do
      cached <- embed $ lookup projectCache groupId
      case cached of
        (Just runners) -> pure (Right runners, Hit)
        Nothing -> do
          projects <- getProjectsNotOnExcludeListOrEmpty groupId
          result <- sequence <$> traverse (\(Project projectId _ _ _ _) -> fmap (projectId,) <$> getRunnersForProject projectId) projects
          embed $ traverse_ (insert projectCache groupId) result
          pure (result, Miss)
    recordCacheLookupResult (CacheTag "project-runners") cacheResult
    pure result
    where
      getRunnersForProject projectId = do
        let template = [uriTemplate|/api/v4/projects/{projectId}/runners?type=project_type|]
        embed $ do
          runnerIds <- fetchDataPaginated baseUrl apiToken userAgent template [("projectId", (stringValue . show) projectId)] histogram
          fetchRunnersForRunnerIds runnerIds
  GetRunningJobsForRunner runnerId -> do
    let template = [uriTemplate|/api/v4/runners/{runnerId}/jobs?status=running|]
    embed $ fetchDataPaginated baseUrl apiToken userAgent template [("runnerId", (stringValue . show) runnerId)] histogram
  where
    fetchRunnersForRunnerIds :: Either UpdateError [RunnerId] -> IO (Either UpdateError [Runner])
    fetchRunnersForRunnerIds (Left err) = pure $ Left err
    fetchRunnersForRunnerIds (Right runnerIds) =
      sequence
        <$> traverse
          ( \(RunnerId runnerId) -> do
              let runnerTemplate = [uriTemplate|/api/v4/runners/{runnerId}|]
              fetchData @Runner baseUrl apiToken userAgent runnerTemplate [("runnerId", (stringValue . show) runnerId)] histogram
          )
          runnerIds

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

deriving newtype instance FromJSON Description

deriving newtype instance FromJSON Tag
