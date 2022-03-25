{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Ports.Outbound.Gitlab.Pipelines (pipelinesApiToIO) where

import Burrito
import Config.Config (ApiToken (..), GitlabHost)
import Core.BuildStatuses (PipelinesApi (..))
import Core.Shared (Group, Id (Id), Ref (Ref), UpdateError (..), Url (..))
import Metrics.Metrics (OutgoingHttpRequestsHistogram)
import Polysemy
import qualified Polysemy.Reader as R
import Ports.Outbound.Gitlab.Helpers
import Ports.Outbound.Gitlab.Instances ()
import Relude

pipelinesApiToIO :: (Member (Embed IO) r, Member (R.Reader (Id Group)) r, Member (R.Reader (Url GitlabHost)) r, Member (R.Reader ApiToken) r, Member (R.Reader OutgoingHttpRequestsHistogram) r) => InterpreterFor PipelinesApi r
pipelinesApiToIO sem = do
  groupId <- R.ask
  baseUrl <- R.ask
  apiToken <- R.ask
  histogram <- R.ask
  pipelinesApiToIO' groupId baseUrl apiToken histogram sem

pipelinesApiToIO' :: (Member (Embed IO) r) => Id Group -> Url GitlabHost -> ApiToken -> OutgoingHttpRequestsHistogram -> InterpreterFor PipelinesApi r
pipelinesApiToIO' groupId baseUrl apiToken histogram =
  interpret $ \case
    GetLatestPipelineForRef (Id project) (Ref ref) -> do
      let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines?ref={ref}&per_page=1|]
      embed $ headOrUpdateError <$> fetchData baseUrl apiToken template [("projectId", (stringValue . show) project), ("ref", (stringValue . toString) ref)] groupId histogram
    GetSinglePipeline (Id project) (Id pipeline) -> do
      let template = [uriTemplate|/api/v4/projects/{projectId}/pipelines/{pipelineId}|]
      embed $ fetchData baseUrl apiToken template [("projectId", (stringValue . show) project), ("pipelineId", (stringValue . show) pipeline)] groupId histogram

headOrUpdateError :: Either UpdateError [a] -> Either UpdateError a
headOrUpdateError (Right (a : _)) = Right a
headOrUpdateError (Right []) = Left EmptyResult
headOrUpdateError (Left e) = Left e
