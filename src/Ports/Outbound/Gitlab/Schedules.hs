{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Ports.Outbound.Gitlab.Schedules (schedulesApiToIO) where

import Burrito
import Config.Config (ApiToken (..), GitlabHost, UserAgent)
import Core.BuildStatuses (SchedulesApi (..))
import Core.Shared (Id (Id), Url (..))
import Metrics.Metrics (OutgoingHttpRequestsHistogram)
import Polysemy
import Polysemy.Reader qualified as R
import Ports.Outbound.Gitlab.Helpers
import Ports.Outbound.Gitlab.Instances ()
import Relude

schedulesApiToIO :: (Member (Embed IO) r, Member (R.Reader (Url GitlabHost)) r, Member (R.Reader ApiToken) r, Member (R.Reader UserAgent) r, Member (R.Reader OutgoingHttpRequestsHistogram) r) => InterpreterFor SchedulesApi r
schedulesApiToIO sem = do
  baseUrl <- R.ask
  apiToken <- R.ask
  userAgent <- R.ask
  histogram <- R.ask
  schedulesApiToIO' baseUrl apiToken userAgent histogram sem

schedulesApiToIO' :: (Member (Embed IO) r) => Url GitlabHost -> ApiToken -> UserAgent -> OutgoingHttpRequestsHistogram -> InterpreterFor SchedulesApi r
schedulesApiToIO' baseUrl apiToken userAgent histogram =
  interpret $ \case
    GetActiveSchedulesForProject (Id project) -> do
      let template = [uriTemplate|/api/v4/projects/{projectId}/pipeline_schedules?scope={scope}|]
      embed $ fetchDataPaginated baseUrl apiToken userAgent template [("projectId", (stringValue . show) project), ("scope", stringValue "active")] histogram
    GetSchedule (Id project) (Id schedule) -> do
      let template = [uriTemplate|/api/v4/projects/{projectId}/pipeline_schedules/{scheduleId}|]
      embed $ fetchData baseUrl apiToken userAgent template [("projectId", (stringValue . show) project), ("scheduleId", (stringValue . show) schedule)] histogram
