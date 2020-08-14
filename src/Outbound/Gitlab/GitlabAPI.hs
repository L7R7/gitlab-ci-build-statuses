{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Outbound.Gitlab.GitlabAPI where

import App
import Config (ApiToken (..), BaseUrl (..))
import Core.Lib (GroupId, HasGetPipelines (..), HasGetProjects (..), PipelineId (..), ProjectId (..), UpdateError (..))
import Data.Aeson (FromJSON)
import Data.Coerce (coerce)
import Env (HasApiToken, apiTokenL, baseUrlL, groupIdL)
import Inbound.HTTP.Metrics
import Network.HTTP.Simple (Request, getResponseBody, httpJSONEither, parseRequest_, setRequestHeader)
import Prometheus (observeDuration)
import RIO

instance HasGetProjects App where
  getProjects = do
    baseUrl <- view baseUrlL
    group <- view groupIdL
    fetchData "/api/v4/groups/{groupId}/projects" $ projectsRequest baseUrl group

projectsRequest :: BaseUrl -> GroupId -> Request
projectsRequest baseUrl gId =
  parseRequest_ $ mconcat [show baseUrl, "/api/v4/groups/", show gId, "/projects?per_page=100&simple=true&include_subgroups=true"]

instance HasGetPipelines App where
  getPipelines pId = do
    baseUrl <- view baseUrlL
    fetchData "/api/v4/projects/{projectId}/pipelines" $ pipelinesRequest baseUrl pId
  getSinglePipeline project pipeline = do
    baseUrl <- view baseUrlL
    fetchData "/api/v4/projects/{projectId}/pipelines/{pipelineId}" $ singlePipelineRequest baseUrl project pipeline

pipelinesRequest :: BaseUrl -> ProjectId -> Request
pipelinesRequest (BaseUrl baseUrl) (ProjectId i) = parseRequest_ $ mconcat [show baseUrl, "/api/v4/projects/", show i, "/pipelines?scope=branches"]

singlePipelineRequest :: BaseUrl -> ProjectId -> PipelineId -> Request
singlePipelineRequest (BaseUrl baseUrl) (ProjectId project) (PipelineId pipeline) = parseRequest_ $ mconcat [show baseUrl, "/api/v4/projects/", show project, "/pipelines/", show pipeline]

fetchData :: (HasApiToken env, HasOutgoingHttpRequestsHistogram env, FromJSON a) => Text -> Request -> RIO env (Either UpdateError a)
fetchData metricLabel request = do
  token <- view apiTokenL
  histogram <- view outgoingHttpRequestsHistogramL
  result <- liftIO $ observeDuration (VectorWithLabel histogram metricLabel) (try (mapLeft ConversionError . getResponseBody <$> httpJSONEither (setRequestHeader "PRIVATE-TOKEN" [coerce token] request)))
  pure . join $ mapLeft HttpError result
