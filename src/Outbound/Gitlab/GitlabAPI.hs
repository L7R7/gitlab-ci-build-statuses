{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Outbound.Gitlab.GitlabAPI () where

import App
import Burrito
import Config (ApiToken (..), BaseUrl (..))
import Core.Lib (DetailedPipeline, HasGetPipelines (..), HasGetProjects (..), Pipeline, PipelineId, Project, ProjectId, UpdateError (..))
import Data.Aeson (FromJSON)
import Data.Text (pack)
import Env (HasApiToken, HasBaseUrl, apiTokenL, baseUrlL, groupIdL)
import Inbound.HTTP.Metrics
import Network.HTTP.Simple (Request, getResponseBody, httpJSONEither, parseRequest, setRequestHeader)
import Prometheus (observeDuration)
import RIO

instance HasGetProjects App where
  getProjects :: RIO App (Either UpdateError [Project])
  getProjects = do
    group <- view groupIdL
    let template = [uriTemplate|/api/v4/groups/{groupId}/projects?per_page=100&simple=true&include_subgroups=true|]
    fetchData template [("groupId", (stringValue . show) group)]

instance HasGetPipelines App where
  getPipelines :: ProjectId -> RIO App (Either UpdateError [Pipeline])
  getPipelines project = do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines|]
    fetchData template [("projectId", (stringValue . show) project)]
  getSinglePipeline :: ProjectId -> PipelineId -> RIO App (Either UpdateError DetailedPipeline)
  getSinglePipeline project pipeline = do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines/{pipelineId}|]
    fetchData template [("projectId", (stringValue . show) project), ("pipelineId", (stringValue . show) pipeline)]

fetchData ::
  (HasApiToken env, HasOutgoingHttpRequestsHistogram env, HasBaseUrl env, FromJSON a) =>
  Template ->
  [(String, Value)] ->
  RIO env (Either UpdateError a)
fetchData template vars = do
  (BaseUrl baseUrl) <- view baseUrlL
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    (Left invalidUrl) -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchData' request template

fetchData' :: (HasApiToken env, HasOutgoingHttpRequestsHistogram env, FromJSON a) => Request -> Template -> RIO env (Either UpdateError a)
fetchData' request template = do
  (ApiToken token) <- view apiTokenL
  histogram <- view outgoingHttpRequestsHistogramL
  result <- measure histogram (try (mapLeft ConversionError . getResponseBody <$> httpJSONEither (addToken token request)))
  pure . join $ mapLeft HttpError result
  where
    addToken token = setRequestHeader "PRIVATE-TOKEN" [token]
    measure histogram action = liftIO $ observeDuration (VectorWithLabel histogram ((pack . render) template)) action
