{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Outbound.Gitlab.GtlabAPI where

import App
import Config (ApiToken (..), BaseUrl (..))
import Core.Lib (GroupId, HasGetPipelines (..), HasGetProjects (..), ProjectId (..), UpdateError (..))
import Data.Aeson (FromJSON)
import Data.Coerce (coerce)
import Env (HasApiToken, apiTokenL, baseUrlL, groupIdL)
import Network.HTTP.Simple (Request, getResponseBody, httpJSON, parseRequest_, setRequestHeader)
import RIO

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
