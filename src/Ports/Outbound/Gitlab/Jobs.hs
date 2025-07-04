{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ports.Outbound.Gitlab.Jobs (jobsApiToIO) where

import Burrito
import Config.Config (ApiToken (..), GitlabHost, UserAgent)
import Core.Jobs
import Core.Shared (Url (..))
import Data.Aeson
import Metrics.Metrics (OutgoingHttpRequestsHistogram)
import Polysemy
import Polysemy.Reader qualified as R
import Ports.Outbound.Gitlab.Helpers
import Ports.Outbound.Gitlab.Instances (buildStatusToApiString)
import Relude

jobsApiToIO :: (Member (Embed IO) r, Member (R.Reader (Url GitlabHost)) r, Member (R.Reader ApiToken) r, Member (R.Reader UserAgent) r, Member (R.Reader OutgoingHttpRequestsHistogram) r) => InterpreterFor JobsApi r
jobsApiToIO sem = do
  baseUrl <- R.ask
  apiToken <- R.ask
  userAgent <- R.ask
  histogram <- R.ask
  jobsApiToIO' baseUrl apiToken userAgent histogram sem

jobsApiToIO' :: (Member (Embed IO) r) => Url GitlabHost -> ApiToken -> UserAgent -> OutgoingHttpRequestsHistogram -> InterpreterFor JobsApi r
jobsApiToIO' baseUrl apiToken userAgent histogram = interpret $ \case
  GetJobsWithStatuses projectId statuses -> do
    let template = [uriTemplate|/api/v4/projects/{projectId}/jobs{?scope%5B%5D*}|]
    embed $ fetchDataPaginated baseUrl apiToken userAgent template [("projectId", (stringValue . show) projectId), ("scope%5B%5D", listValue (buildStatusToApiString <$> toList statuses))] histogram

instance FromJSON Job where
  parseJSON = withObject "job" $ \job -> do
    jobId <- job .: "id"
    jobStatus <- job .: "status"
    pure Job {..}
