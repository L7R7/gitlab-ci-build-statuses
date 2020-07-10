{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App where

import Config
import Core.Lib
import Data.Aeson.Types (FromJSON)
import Data.Coerce (coerce)
import Data.Time (UTCTime, getCurrentTime)
import Env
import Katip
import Network.HTTP.Client.Conduit (Request, parseRequest_)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestHeader)
import RIO
import System.Metrics

data App
  = App
      { statuses :: !(IORef (Maybe UTCTime, [Result])),
        config :: !Config,
        ekgStore :: !Store,
        logNamespace :: !Namespace,
        logContext :: !LogContexts,
        logEnv :: !LogEnv
      }

instance Katip (RIO App) where
  getLogEnv = asks logEnv
  localLogEnv f (RIO a) = RIO (local (\s -> s {logEnv = f (logEnv s)}) a)

instance KatipContext (RIO App) where
  getKatipContext = asks logContext
  localKatipContext f (RIO app) = RIO (local (\s -> s {logContext = f (logContext s)}) app)
  getKatipNamespace = asks logNamespace
  localKatipNamespace f (RIO app) = RIO (local (\s -> s {logNamespace = f (logNamespace s)}) app)

instance HasConfig App where
  configL = lens config (\app iJC -> app {config = iJC})

instance HasStore App where
  storeL = lens ekgStore (\app st -> app {ekgStore = st})

instance HasApiToken App where
  apiTokenL = lens (apiToken . config) (\app token -> app {config = (config app) {apiToken = token}})

instance HasBaseUrl App where
  baseUrlL = lens (gitlabBaseUrl . config) (\app u -> app {config = (config app) {gitlabBaseUrl = u}})

instance HasGroupId App where
  groupIdL = lens (groupId . config) (\app g -> app {config = (config app) {groupId = g}})

instance HasDataUpdateInterval App where
  dataUpdateIntervalL = lens (dataUpdateIntervalMins . config) (\app d -> app {config = (config app) {dataUpdateIntervalMins = d}})

instance HasUiUpdateInterval App where
  uiUpdateIntervalL = lens (uiUpdateIntervalSecs . config) (\app u -> app {config = (config app) {uiUpdateIntervalSecs = u}})

instance HasGetProjects App where
  getProjects = do
    baseUrl <- view baseUrlL
    group <- view groupIdL
    fetchData $ projectsRequest baseUrl group

projectsRequest :: BaseUrl -> GroupId -> Request
projectsRequest baseUrl gId =
  parseRequest_ $ mconcat [coerce baseUrl, "/api/v4/groups/", show gId, "/projects?per_page=100&simple=true&include_subgroups=true"]

instance HasGetPipelines App where
  getPipelines pId = do
    baseUrl <- view baseUrlL
    fetchData $ pipelinesRequest baseUrl pId

pipelinesRequest :: BaseUrl -> ProjectId -> Request
pipelinesRequest (BaseUrl baseUrl) (ProjectId i) = parseRequest_ $ mconcat [baseUrl, "/api/v4/projects/", show i, "/pipelines?scope=branches"]

fetchData :: (HasApiToken env, FromJSON a) => Request -> RIO env (Either UpdateError [a])
fetchData request = do
  token <- view apiTokenL
  result <- try (getResponseBody <$> httpJSON (setRequestHeader "PRIVATE-TOKEN" [coerce token] request))
  pure $ mapLeft HttpError result

instance HasBuildStatuses App where
  getStatuses = view readBuildStatuses >>= readIORef
  setStatuses results = do
    store <- view readBuildStatuses
    updateTime <- liftIO getCurrentTime
    liftIO $ atomicModifyIORef' store (const ((Just updateTime, results), (updateTime, results)))

readBuildStatuses :: Lens' App (IORef (Maybe UTCTime, [Result]))
readBuildStatuses = lens statuses (\app st -> app {statuses = st})
