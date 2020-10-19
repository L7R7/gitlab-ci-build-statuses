{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Outbound.Gitlab.GitlabAPI (projectsApiToIO, pipelinesApiToIO) where

import Burrito
import Config (ApiToken (..), GitlabHost)
import Control.Lens (Prism', Traversal', filtered, prism', _1, _2)
import Core.Lib (Id (Id), PipelinesApi (..), ProjectsApi (..), Ref (Ref), UpdateError (..), Url)
import Data.Aeson (FromJSON)
import Data.List (find)
import Data.Text (pack, unpack)
import Metrics.Metrics (OutgoingHttpRequestsHistogram, VectorWithLabel (VectorWithLabel))
import Network.HTTP.Client.Conduit (HttpExceptionContent, requestFromURI, requestHeaders, responseTimeout, responseTimeoutMicro)
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types (Link (..), LinkParam (Rel), href)
import Network.HTTP.Simple (HttpException (..), JSONException (..), Request, RequestHeaders, Response, getResponseBody, getResponseHeader, httpJSONEither, parseRequest, setRequestHeader)
import Network.HTTP.Types.Header (HeaderName)
import Network.URI (URI)
import Polysemy
import Prometheus (observeDuration)
import RIO

projectsApiToIO :: Member (Embed IO) r => Url GitlabHost -> ApiToken -> OutgoingHttpRequestsHistogram -> Sem (ProjectsApi ': r) a -> Sem r a
projectsApiToIO baseUrl apiToken histogram = interpret $ \case
  GetProjects groupId -> do
    let template = [uriTemplate|/api/v4/groups/{groupId}/projects?simple=true&include_subgroups=true&archived=false|]
    embed $ fetchDataPaginated baseUrl apiToken template [("groupId", (stringValue . show) groupId)] histogram

pipelinesApiToIO :: Member (Embed IO) r => Url GitlabHost -> ApiToken -> OutgoingHttpRequestsHistogram -> Sem (PipelinesApi ': r) a -> Sem r a
pipelinesApiToIO baseUrl apiToken histogram = interpret $ \case
  GetLatestPipelineForRef (Id project) (Ref ref) -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines?ref={ref}&per_page=1|]
    embed $ headOrUpdateError <$> fetchData baseUrl apiToken template [("projectId", (stringValue . show) project), ("ref", (stringValue . unpack) ref)] histogram
  GetSinglePipeline (Id project) (Id pipeline) -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines/{pipelineId}|]
    embed $ fetchData baseUrl apiToken template [("projectId", (stringValue . show) project), ("pipelineId", (stringValue . show) pipeline)] histogram

headOrUpdateError :: Either UpdateError [a] -> Either UpdateError a
headOrUpdateError (Right (a : _)) = Right a
headOrUpdateError (Right []) = Left NoPipelineForDefaultBranch
headOrUpdateError (Left e) = Left e

fetchData :: (FromJSON a) => Url GitlabHost -> ApiToken -> Template -> [(String, Value)] -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError a)
fetchData baseUrl apiToken template vars histogram = do
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    Left invalidUrl -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchData' apiToken request template histogram

fetchData' :: (FromJSON a) => ApiToken -> Request -> Template -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError a)
fetchData' apiToken request template histogram = do
  result <- measure histogram template (try (mapLeft ConversionError . getResponseBody <$> httpJSONEither (setTimeout $ addToken apiToken request)))
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

fetchDataPaginated :: (FromJSON a) => Url GitlabHost -> ApiToken -> Template -> [(String, Value)] -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError [a])
fetchDataPaginated baseUrl apiToken template vars histogram = do
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    (Left invalidUrl) -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchDataPaginated' apiToken request template histogram []

fetchDataPaginated' :: (FromJSON a) => ApiToken -> Request -> Template -> OutgoingHttpRequestsHistogram -> [a] -> IO (Either UpdateError [a])
fetchDataPaginated' apiToken request template histogram acc = do
  result <- try $ do
    response <- measure histogram template $ httpJSONEither (setTimeout $ addToken apiToken request)
    let next = parseNextRequest response
    case mapLeft ConversionError $ getResponseBody response of
      Left err -> pure $ Left err
      Right as -> maybe (pure $ Right (as <> acc)) (\req -> fetchDataPaginated' apiToken req template histogram (as <> acc)) next
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

parseNextRequest :: Response a -> Maybe Request
parseNextRequest response = parseNextHeader response >>= requestFromURI

parseNextHeader :: Response a -> Maybe URI
parseNextHeader response = href <$> find isNextLink ((parseLinkHeaderBS <$> getResponseHeader "link" response) >>= concat)

isNextLink :: Link -> Bool
isNextLink (Link _ [(Rel, "next")]) = True
isNextLink _ = False

addToken :: ApiToken -> Request -> Request
addToken (ApiToken token) = setRequestHeader privateToken [token]

setTimeout :: Request -> Request
setTimeout request = request {responseTimeout = responseTimeoutMicro 5000000}

measure :: OutgoingHttpRequestsHistogram -> Template -> IO a -> IO a
measure histogram template = observeDuration (VectorWithLabel histogram ((pack . render) template))

-- TODO: 2020-08-31 can we use lenses for that (does it make sense to do that?)
removeApiTokenFromUpdateError :: UpdateError -> UpdateError
removeApiTokenFromUpdateError (HttpError httpException) = HttpError (removeApiTokenFromHttpException httpException)
removeApiTokenFromUpdateError (ConversionError jsonException) = ConversionError (removeApiTokenFromJsonException jsonException)
removeApiTokenFromUpdateError EmptyPipelinesResult = EmptyPipelinesResult
removeApiTokenFromUpdateError NoPipelineForDefaultBranch = NoPipelineForDefaultBranch

removeApiTokenFromHttpException :: HttpException -> HttpException
removeApiTokenFromHttpException = set (reqPrism . _1 . headers . tokenHeader) "xxxxx"

reqPrism :: Prism' HttpException (Request, HttpExceptionContent)
reqPrism = prism' (uncurry HttpExceptionRequest) extract
  where
    extract (HttpExceptionRequest request reason) = Just (request, reason)
    extract _ = Nothing

tokenHeader :: Traversal' RequestHeaders ByteString
tokenHeader = traverse . filtered (\header -> fst header == privateToken) . _2

privateToken :: HeaderName
privateToken = "PRIVATE-TOKEN"

headers :: Lens' Request RequestHeaders
headers = lens getter setter
  where
    getter = requestHeaders
    setter r h = r {requestHeaders = h}

removeApiTokenFromJsonException :: JSONException -> JSONException
removeApiTokenFromJsonException (JSONParseException request response parseError) = JSONParseException (removeApiTokenFromRequest request) response parseError
removeApiTokenFromJsonException (JSONConversionException request response s) = JSONConversionException (removeApiTokenFromRequest request) response s

removeApiTokenFromRequest :: Request -> Request
removeApiTokenFromRequest = set (headers . tokenHeader) "xxxxx"
