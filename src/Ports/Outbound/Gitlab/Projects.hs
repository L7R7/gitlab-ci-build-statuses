{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Ports.Outbound.Gitlab.Projects (initCache, projectsApiToIO) where

import Burrito
import Config.Config (ApiToken (..), GitlabHost, ProjectCacheTtlSeconds (ProjectCacheTtlSeconds), SharedProjects (Exclude, Include))
import Core.BuildStatuses (Project, ProjectsApi (..))
import Core.Shared (Group, Id, Url (..))
import Data.Cache
import Metrics.Metrics (CacheResult (..), CacheTag (CacheTag), MetricsApi, OutgoingHttpRequestsHistogram, recordCacheLookupResult)
import Polysemy
import qualified Polysemy.Reader as R
import Ports.Outbound.Gitlab.Helpers
import Ports.Outbound.Gitlab.Instances ()
import Relude
import System.Clock

initCache :: ProjectCacheTtlSeconds -> IO (Cache (Id Group) [Project])
initCache (ProjectCacheTtlSeconds ttl) = newCache (Just (TimeSpec ttl 0))

projectsApiToIO :: (Member (Embed IO) r, Member MetricsApi r, Member (R.Reader (Url GitlabHost)) r, Member (R.Reader ApiToken) r, Member (R.Reader SharedProjects) r, Member (R.Reader OutgoingHttpRequestsHistogram) r, Member (R.Reader (Cache (Id Group) [Project])) r) => InterpreterFor ProjectsApi r
projectsApiToIO = interpret $ \case
  GetProjects groupId -> do
    cache <- R.ask
    baseUrl <- R.ask
    apiToken <- R.ask
    sharedProjects <- R.ask
    histogram <- R.ask
    (result, cacheResult) <- embed $ do
      cached <- lookup cache groupId
      case cached of
        (Just projects) -> pure (Right projects, Hit)
        Nothing -> do
          let template = [uriTemplate|/api/v4/groups/{groupId}/projects?simple=true&include_subgroups=true&archived=false{&with_shared}|]
          result <- fetchDataPaginated baseUrl apiToken template [("groupId", (stringValue . show) groupId), ("with_shared", withShared sharedProjects)] groupId histogram
          traverse_ (insert cache groupId) result
          pure (result, Miss)
    recordCacheLookupResult (CacheTag "projects") cacheResult
    pure result
  where
    withShared Include = stringValue "true"
    withShared Exclude = stringValue "false"
