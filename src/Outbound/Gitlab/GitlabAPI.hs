{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Outbound.Gitlab.GitlabAPI (initCache, projectsApiToIO, pipelinesApiToIO) where

import Burrito
import Config.Config (ApiToken (..), GitlabHost, ProjectCacheTtlSeconds (ProjectCacheTtlSeconds), SharedProjects (Exclude, Include))
import Control.Exception (try)
import Core.Lib (BuildStatus (..), DetailedPipeline (..), Group, Id (Id), Pipeline, PipelinesApi (..), Project, ProjectsApi (..), Ref (Ref), UpdateError (..), Url (..))
import Data.Aeson hiding (Result, Value)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Cache
import Data.Either.Combinators (mapLeft)
import Metrics.Metrics (OutgoingHttpRequestsHistogram)
import Metrics.PrometheusUtils (VectorWithLabel (VectorWithLabel))
import Network.HTTP.Client.Conduit (requestFromURI, responseTimeout, responseTimeoutMicro)
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types (Link (..), LinkParam (Rel), href)
import Network.HTTP.Simple (Request, Response, getResponseBody, getResponseHeader, httpJSONEither, parseRequest, setRequestHeader)
import Network.URI (URI, parseURI)
import Outbound.Gitlab.RequestResponseUtils
import Polysemy
import Prometheus (observeDuration)
import Relude
import System.Clock

initCache :: ProjectCacheTtlSeconds -> IO (Cache (Id Group) [Project])
initCache (ProjectCacheTtlSeconds ttl) = newCache (Just (TimeSpec ttl 0))

projectsApiToIO :: Member (Embed IO) r => Url GitlabHost -> ApiToken -> SharedProjects -> OutgoingHttpRequestsHistogram -> Cache (Id Group) [Project] -> InterpreterFor ProjectsApi r
projectsApiToIO baseUrl apiToken sharedProjects histogram cache = interpret $ \case
  GetProjects groupId -> embed $ do
    cached <- lookup cache groupId
    case cached of
      (Just projects) -> pure $ Right projects
      Nothing -> do
        let template = [uriTemplate|/api/v4/groups/{groupId}/projects?simple=true&include_subgroups=true&archived=false{&with_shared}|]
        result <- fetchDataPaginated baseUrl apiToken template [("groupId", (stringValue . show) groupId), ("with_shared", withShared sharedProjects)] groupId histogram
        traverse_ (insert cache groupId) result
        pure result
  where
    withShared Include = stringValue "true"
    withShared Exclude = stringValue "false"

pipelinesApiToIO :: Member (Embed IO) r => Url GitlabHost -> ApiToken -> Id Group -> OutgoingHttpRequestsHistogram -> InterpreterFor PipelinesApi r
pipelinesApiToIO baseUrl apiToken groupId histogram = interpret $ \case
  GetLatestPipelineForRef (Id project) (Ref ref) -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines?ref={ref}&per_page=1|]
    embed $ headOrUpdateError <$> fetchData baseUrl apiToken template [("projectId", (stringValue . show) project), ("ref", (stringValue . toString) ref)] groupId histogram
  GetSinglePipeline (Id project) (Id pipeline) -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines/{pipelineId}|]
    embed $ fetchData baseUrl apiToken template [("projectId", (stringValue . show) project), ("pipelineId", (stringValue . show) pipeline)] groupId histogram

headOrUpdateError :: Either UpdateError [a] -> Either UpdateError a
headOrUpdateError (Right (a : _)) = Right a
headOrUpdateError (Right []) = Left NoPipelineForDefaultBranch
headOrUpdateError (Left e) = Left e

fetchData :: (FromJSON a) => Url GitlabHost -> ApiToken -> Template -> [(String, Value)] -> Id Group -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError a)
fetchData baseUrl apiToken template vars groupId histogram = do
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    Left invalidUrl -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchData' apiToken request template groupId histogram

fetchData' :: (FromJSON a) => ApiToken -> Request -> Template -> Id Group -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError a)
fetchData' apiToken request template groupId histogram = do
  result <- measure groupId histogram template (try (mapLeft ConversionError . getResponseBody <$> httpJSONEither (setTimeout $ addToken apiToken request)))
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

fetchDataPaginated :: (FromJSON a) => Url GitlabHost -> ApiToken -> Template -> [(String, Value)] -> Id Group -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError [a])
fetchDataPaginated baseUrl apiToken template vars groupId histogram = do
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    (Left invalidUrl) -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchDataPaginated' apiToken request template groupId histogram []

fetchDataPaginated' :: (FromJSON a) => ApiToken -> Request -> Template -> Id Group -> OutgoingHttpRequestsHistogram -> [a] -> IO (Either UpdateError [a])
fetchDataPaginated' apiToken request template groupId histogram acc = do
  result <- try $ do
    response <- measure groupId histogram template $ httpJSONEither (setTimeout $ addToken apiToken request)
    let next = parseNextRequest response
    case mapLeft ConversionError $ getResponseBody response of
      Left err -> pure $ Left err
      Right as -> maybe (pure $ Right (as <> acc)) (\req -> fetchDataPaginated' apiToken req template groupId histogram (as <> acc)) next
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

parseNextRequest :: Response a -> Maybe Request
parseNextRequest response = parseNextHeader response >>= requestFromURI

parseNextHeader :: Response a -> Maybe URI
parseNextHeader response = href <$> find isNextLink (getResponseHeader "link" response >>= concat . parseLinkHeaderBS)

isNextLink :: Link -> Bool
isNextLink (Link _ [(Rel, "next")]) = True
isNextLink _ = False

addToken :: ApiToken -> Request -> Request
addToken (ApiToken token) = setRequestHeader privateToken [token]

setTimeout :: Request -> Request
setTimeout request = request {responseTimeout = responseTimeoutMicro 5000000}

measure :: Id Group -> OutgoingHttpRequestsHistogram -> Template -> IO a -> IO a
measure (Id groupId) histogram template = observeDuration (VectorWithLabel histogram (show groupId, (toText . render) template))

instance FromJSON DetailedPipeline where
  parseJSON = withObject "detailedPipeline" $ \dp -> do
    detailedPipelineId <- dp .: "id"
    detailedPipelineRef <- dp .: "ref"
    detailedPipelineWebUrl <- dp .: "web_url"
    detailedPipelineStatus <- dp .: "detailed_status" >>= \ds -> ds .: "group"
    pure DetailedPipeline {..}

instance FromJSON BuildStatus where
  parseJSON = withText "BuildStatus" $ \x -> maybe (fail $ mconcat ["couldn't parse build status from '", show x, "'"]) pure (inverseMap buildStatusToApiString x)

buildStatusToApiString :: IsString p => BuildStatus -> p
buildStatusToApiString Unknown = "unknown"
buildStatusToApiString Cancelled = "canceled"
buildStatusToApiString Created = "created"
buildStatusToApiString Failed = "failed"
buildStatusToApiString Manual = "manual"
buildStatusToApiString Pending = "pending"
buildStatusToApiString Preparing = "preparing"
buildStatusToApiString Running = "running"
buildStatusToApiString Scheduled = "scheduled"
buildStatusToApiString Skipped = "skipped"
buildStatusToApiString Successful = "success"
buildStatusToApiString SuccessfulWithWarnings = "success-with-warnings"
buildStatusToApiString WaitingForResource = "waiting_for_resource"

instance FromJSON Pipeline where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON (Url a) where
  parseJSON = withText "URI" $ \v -> Url <$> maybe (fail "Bad URI") pure (parseURI (toString v))

instance FromJSON Project where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
