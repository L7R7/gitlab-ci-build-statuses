{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ports.Inbound.HTTP.Runners.Html
  ( API,
    template,
  )
where

import Config.Backbone
import Config.Config
import Core.Runners hiding (getJobs)
import Core.Runners qualified as R (getJobs)
import Core.Shared (DataUpdateIntervalSeconds, Ref (Ref))
import Data.Map (toList)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Lucid
import Lucid.Base (makeAttribute)
import Polysemy
import Polysemy.Reader qualified as R
import Polysemy.Time (Time)
import Polysemy.Time qualified as Time
import Ports.Inbound.HTTP.Util (AutoRefresh (Refresh), lastUpdatedToHtml)
import Relude
import Servant (Get, QueryFlag, (:>))
import Servant.HTML.Lucid

type API = "jobs" :> QueryFlag "norefresh" :> Get '[HTML] (Html ())

template ::
  ( Member RunnersJobsApi r,
    Member (Time UTCTime d) r,
    Member (R.Reader JobsView) r,
    Member (R.Reader DataUpdateIntervalSeconds) r,
    Member (R.Reader UiUpdateIntervalSeconds) r,
    Member (R.Reader GitCommit) r
  ) =>
  AutoRefresh ->
  Sem r (Html ())
template autoRefresh = do
  dataUpdateInterval <- R.ask
  uiUpdateInterval <- R.ask
  gitCommit <- R.ask
  jobsView <- R.ask
  now <- Time.now
  if jobsView == Enabled
    then template' now dataUpdateInterval uiUpdateInterval gitCommit autoRefresh <$> R.getJobs
    else pure $ runnersViewDisabled uiUpdateInterval gitCommit autoRefresh

runnersViewDisabled :: UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> Html ()
runnersViewDisabled uiUpdateInterval gitCommit autoRefresh = do
  pageHeader uiUpdateInterval gitCommit autoRefresh Nothing
  body_ $ div_ [class_ "job no-successful-update"] $ p_ "Runners view is disabled. Update your config to enable it"

template' :: UTCTime -> DataUpdateIntervalSeconds -> UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> RunnersJobs -> Html ()
template' now dataUpdateInterval uiUpdateInterval gitCommit autoRefresh runnersJobs = do
  pageHeader uiUpdateInterval gitCommit autoRefresh (Just runnersJobs)
  pageBody dataUpdateInterval now runnersJobs

pageHeader :: UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> Maybe RunnersJobs -> Html ()
pageHeader (UiUpdateIntervalSeconds updateInterval) gitCommit autoRefresh runnersJobs = do
  doctype_
  html_ [lang_ "en"]
    $ head_
    $ do
      meta_ [charset_ "UTF-8"]
      when (autoRefresh == Refresh) $ meta_ [httpEquiv_ "Refresh", content_ (show updateInterval)]
      title_ "Build Statuses"
      link_ [rel_ "icon", type_ "image/png", href_ ("static/" <> faviconPrefix runnersJobs <> "-favicon.ico")]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "static/normalize-d6d444a732.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "static/jobs-2e8833d035.css"]
      script_ [type_ "text/javascript", src_ "static/script-32964cd17f.js"] ("" :: String)
      meta_ [makeAttribute "version" (show gitCommit)]

faviconPrefix :: (IsString a) => Maybe RunnersJobs -> a
faviconPrefix (Just (RunnersJobs (_, jobs))) | not (all null jobs) = "running"
faviconPrefix Nothing = "failed"
faviconPrefix _ = "idle"

pageBody :: DataUpdateIntervalSeconds -> UTCTime -> RunnersJobs -> Html ()
pageBody dataUpdateInterval now runnersJobs = body_ $ runnersJobsToHtml runnersJobs <> section_ [class_ "jobs-meta"] (lastUpdated runnersJobs)
  where
    lastUpdated :: RunnersJobs -> Html ()
    lastUpdated NoSuccessfulUpdateYet = mempty
    lastUpdated (RunnersJobs (t, _)) = lastUpdatedToHtml dataUpdateInterval now t

runnersJobsToHtml :: RunnersJobs -> Html ()
runnersJobsToHtml NoSuccessfulUpdateYet = div_ [class_ "job no-successful-update"] $ p_ "There was no successful update yet"
runnersJobsToHtml (RunnersJobs (_, runners)) | null runners = div_ [class_ "job empty-results"] $ p_ "No online runners found"
runnersJobsToHtml (RunnersJobs (_, runners)) = traverse_ runnerJobsToHtml (sortOn (\(runner, _) -> runnerId runner) (Data.Map.toList runners))

runnerJobsToHtml :: (Runner, [Job]) -> Html ()
runnerJobsToHtml (runner, jobs) = div_ [class_ "runner-container"] $ runnerToHtml runner <> section_ [class_ "jobs"] (jobsToHtml jobs)

runnerToHtml :: Runner -> Html ()
runnerToHtml Runner {..} =
  div_ [class_ "runner-info"]
    $ "#"
    <> toHtml runnerId
    <> " - "
    <> toHtml runnerDescription
    <> " - @"
    <> toHtml runnerIpAddress
    <> " -"
    <> foldMap (\t -> " #" <> toHtml t) runnerTagList

jobsToHtml :: [Job] -> Html ()
jobsToHtml [] = div_ [class_ "job empty"] $ p_ "No running jobs"
jobsToHtml jobs = foldl' (\acc j -> acc <> jobToHtml j) mempty jobs

jobToHtml :: Job -> Html ()
jobToHtml Job {..} = a_ [href_ (show jobWebUrl), target_ "_blank", class_ "job"] $ do
  div_ [class_ "job-id"] $ "#" <> toHtml jobId
  div_ [class_ "project-name"] $ toHtml jobProjectName
  div_ [class_ "job-ref"] $ truncateRef jobRef
  div_ $ toHtml jobStage <> " > " <> toHtml jobName

deriving newtype instance ToHtml Stage

deriving newtype instance ToHtml IpAddress

deriving newtype instance ToHtml Description

deriving newtype instance ToHtml Core.Runners.Tag

truncateRef :: Ref -> Html ()
truncateRef (Ref ref) | T.length ref <= 26 = toHtml ref
truncateRef (Ref ref) = toHtml $ T.take 10 ref <> "..." <> T.drop (T.length ref - 13) ref
