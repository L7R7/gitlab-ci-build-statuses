{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Outbound.Gitlab.Pipelines (pipelinesApiToIO) where

import Burrito
import Config.Config (ApiToken (..), GitlabHost)
import Core.Lib (Group, Id (Id), PipelinesApi (..), Ref (Ref), UpdateError (..), Url (..))
import Metrics.Metrics (OutgoingHttpRequestsHistogram)
import Outbound.Gitlab.Helpers
import Outbound.Gitlab.Instances ()
import Polysemy
import Relude

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
headOrUpdateError (Right []) = Left EmptyResult
headOrUpdateError (Left e) = Left e
