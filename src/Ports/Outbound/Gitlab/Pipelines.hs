{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Ports.Outbound.Gitlab.Pipelines (pipelinesApiToIO) where

import Burrito
import Config.Config (ApiToken (..), GitlabHost, UserAgent)
import Core.BuildStatuses (Pipeline (..), PipelineSource (..), PipelinesApi (..))
import Core.Shared (Id (Id), Ref (Ref), UpdateError (..), Url (..))
import Metrics.Metrics (OutgoingHttpRequestsHistogram)
import Polysemy
import Polysemy.Reader qualified as R
import Ports.Outbound.Gitlab.Helpers
import Ports.Outbound.Gitlab.Instances ()
import Relude

pipelinesApiToIO :: (Member (Embed IO) r, Member (R.Reader (Url GitlabHost)) r, Member (R.Reader ApiToken) r, Member (R.Reader UserAgent) r, Member (R.Reader OutgoingHttpRequestsHistogram) r) => InterpreterFor PipelinesApi r
pipelinesApiToIO sem = do
  baseUrl <- R.ask
  apiToken <- R.ask
  userAgent <- R.ask
  histogram <- R.ask
  pipelinesApiToIO' baseUrl apiToken userAgent histogram sem

pipelinesApiToIO' :: (Member (Embed IO) r) => Url GitlabHost -> ApiToken -> UserAgent -> OutgoingHttpRequestsHistogram -> InterpreterFor PipelinesApi r
pipelinesApiToIO' baseUrl apiToken userAgent histogram =
  interpret $ \case
    GetLatestPipelineForRef (Id project) (Ref ref) -> do
      let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines?ref={ref}&per_page=25|]
      -- the sources query parameter is not repeatable, we can only filter for one source at a time
      -- therefore, we fetch 25 pipelines and assume that there's at least one non-scheduled pipeline in there
      embed $ headOrUpdateError . fmap (filter (\pipeline -> pipelineSource pipeline /= PipelineSourceSchedule)) <$> fetchData baseUrl apiToken userAgent template [("projectId", (stringValue . show) project), ("ref", (stringValue . toString) ref)] histogram
    GetSinglePipeline (Id project) (Id pipeline) -> do
      let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines/{pipelineId}|]
      embed $ fetchData baseUrl apiToken userAgent template [("projectId", (stringValue . show) project), ("pipelineId", (stringValue . show) pipeline)] histogram

headOrUpdateError :: Either UpdateError [a] -> Either UpdateError a
headOrUpdateError (Right (a : _)) = Right a
headOrUpdateError (Right []) = Left EmptyResult
headOrUpdateError (Left e) = Left e
