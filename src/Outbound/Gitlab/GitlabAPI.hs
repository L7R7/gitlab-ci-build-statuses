{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Outbound.Gitlab.GitlabAPI () where

import App
import Burrito
import Config (ApiToken (..), BaseUrl (..), maxConcurrency)
import Core.Lib (DetailedPipeline, HasGetPipelines (..), HasGetProjects (..), Id, Pipeline, Project, UpdateError (..))
import Data.Aeson (FromJSON)
import Data.List (find)
import Data.Text (pack)
import Env (HasApiToken, HasBaseUrl, apiTokenL, baseUrlL, groupIdL)
import Inbound.HTTP.Metrics
import Network.HTTP.Client.Conduit (requestFromURI_, responseTimeout, responseTimeoutMicro)
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types (Link (..), LinkParam (Rel), href)
import Network.HTTP.Simple (Request, Response, getResponseBody, getResponseHeader, httpJSONEither, parseRequest, setRequestHeader)
import Network.URI (URI)
import Prometheus (observeDuration)
import RIO

instance HasGetProjects App where
  getProjects :: RIO App (Either UpdateError [Project])
  getProjects = do
    group <- view groupIdL
    let template = [uriTemplate|/api/v4/groups/{groupId}/projects?simple=true&include_subgroups=true|]
    fetchDataPaginated template [("groupId", (stringValue . show) group)]
  maxConcurrencyL = to (maxConcurrency . config)

instance HasGetPipelines App where
  getPipelines :: Id Project -> RIO App (Either UpdateError [Pipeline])
  getPipelines project = do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines|]
    fetchData template [("projectId", (stringValue . show) project)]
  getSinglePipeline :: Id Project -> Id Pipeline -> RIO App (Either UpdateError DetailedPipeline)
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
  token <- view apiTokenL
  histogram <- view outgoingHttpRequestsHistogramL
  result <- measure histogram template (try (mapLeft ConversionError . getResponseBody <$> httpJSONEither (setTimeout $ addToken token request)))
  pure . join $ mapLeft HttpError result

fetchDataPaginated ::
  (HasApiToken env, HasOutgoingHttpRequestsHistogram env, HasBaseUrl env, FromJSON a) =>
  Template ->
  [(String, Value)] ->
  RIO env (Either UpdateError [a])
fetchDataPaginated template vars = do
  token <- view apiTokenL
  (BaseUrl baseUrl) <- view baseUrlL
  histogram <- view outgoingHttpRequestsHistogramL
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    (Left invalidUrl) -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchDataPaginated' token histogram template request []

fetchDataPaginated' ::
  (HasApiToken env, HasOutgoingHttpRequestsHistogram env, FromJSON a) =>
  ApiToken ->
  OutgoingHttpRequestsHistogram ->
  Template ->
  Request ->
  [a] ->
  RIO env (Either UpdateError [a])
fetchDataPaginated' apiToken histogram template request acc = do
  result <- try $ do
    response <- measure histogram template $ httpJSONEither (setTimeout $ addToken apiToken request)
    let next = parseNextRequest response
    case mapLeft ConversionError $ getResponseBody response of
      Left err -> pure $ Left err
      Right as -> maybe (pure $ Right (as <> acc)) (\req -> fetchDataPaginated' apiToken histogram template req (as <> acc)) next
  pure $ join $ mapLeft HttpError result

parseNextRequest :: Response a -> Maybe Request
parseNextRequest response = requestFromURI_ <$> parseNextHeader response

parseNextHeader :: Response a -> Maybe URI
parseNextHeader response = href <$> find isNextLink ((parseLinkHeaderBS <$> getResponseHeader "link" response) >>= concat)

isNextLink :: Link -> Bool
isNextLink (Link _ [(Rel, "next")]) = True
isNextLink _ = False

addToken :: ApiToken -> Request -> Request
addToken (ApiToken token) = setRequestHeader "PRIVATE-TOKEN" [token]

setTimeout :: Request -> Request
setTimeout request = request {responseTimeout = responseTimeoutMicro 5000000}

measure :: OutgoingHttpRequestsHistogram -> Template -> IO a -> RIO env a
measure histogram template action = liftIO $ observeDuration (VectorWithLabel histogram ((pack . render) template)) action
