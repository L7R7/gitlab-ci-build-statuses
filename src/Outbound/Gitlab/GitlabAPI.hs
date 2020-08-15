{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Outbound.Gitlab.GitlabAPI () where

import App
import Burrito
import Config (ApiToken (..), BaseUrl (..))
import Core.Lib (HasGetPipelines (..), HasGetProjects (..), UpdateError (..))
import Data.Aeson (FromJSON)
import Data.Text (pack)
import Env (HasApiToken, HasBaseUrl, apiTokenL, baseUrlL, groupIdL)
import Inbound.HTTP.Metrics
import Network.HTTP.Simple (getResponseBody, httpJSONEither, parseRequest, setRequestHeader)
import Prometheus (observeDuration)
import RIO

instance HasGetProjects App where
  getProjects = do
    group <- view groupIdL
    let template = [uriTemplate|/api/v4/groups/{groupId}/projects?per_page=100&simple=true&include_subgroups=true|]
    fetchData template [("groupId", (stringValue . show) group)]

instance HasGetPipelines App where
  getPipelines project = do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines|]
    fetchData template [("projectId", (stringValue . show) project)]
  getSinglePipeline project pipeline = do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines/{pipelineId}|]
    fetchData template [("projectId", (stringValue . show) project), ("pipelineId", (stringValue . show) pipeline)]

fetchData ::
  (HasApiToken env, HasOutgoingHttpRequestsHistogram env, HasBaseUrl env, FromJSON a) =>
  Template ->
  [(String, Value)] ->
  RIO env (Either UpdateError a)
fetchData template vars = do
  (ApiToken token) <- view apiTokenL
  (BaseUrl baseUrl) <- view baseUrlL
  histogram <- view outgoingHttpRequestsHistogramL
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    (Left invalidUrl) -> pure $ Left $ HttpError invalidUrl
    Right request -> do
      result <- measure histogram (try (mapLeft ConversionError . getResponseBody <$> httpJSONEither (addToken token request)))
      pure . join $ mapLeft HttpError result
  where
    addToken token = setRequestHeader "PRIVATE-TOKEN" [token]
    measure histogram action = liftIO $ observeDuration (VectorWithLabel histogram ((pack . render) template)) action
