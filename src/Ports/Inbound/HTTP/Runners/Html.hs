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
import Lucid.Base (commuteHtmlT, makeAttribute)
import Polysemy
import Polysemy.Reader qualified as R
import Polysemy.Time (Time)
import Polysemy.Time qualified as Time
import Ports.Inbound.HTTP.Util (AutoRefresh (Refresh), lastUpdatedToHtml)
import Relude
import Servant (Get, QueryFlag, (:>))
import Servant.HTML.Lucid

type API = "jobs" :> QueryFlag "norefresh" :> Get '[HTML] (Html ())

type Frontend = HtmlT (Reader FrontendState) ()

data FrontendState = FrontendState
  { frontendStateRunnersJobs :: RunnersJobs,
    frontendStateDataUpdateInterval :: DataUpdateIntervalSeconds,
    frontendStateUiUpdateInterval :: UiUpdateIntervalSeconds,
    frontendStateAutoRefresh :: AutoRefresh,
    frontendStateGitCommit :: GitCommit,
    frontendStateNow :: UTCTime
  }

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
  jobsView <- R.ask
  frontendState <- FrontendState <$> R.getJobs <*> R.ask <*> R.ask <*> pure autoRefresh <*> R.ask <*> Time.now
  pure
    $ usingReader frontendState
    $ commuteHtmlT
    $ do
      pageHeader
      if jobsView == Enabled
        then pageBody
        else body_ $ div_ [class_ "job no-successful-update"] $ p_ "Runners view is disabled. Update your config to enable it"

pageHeader :: Frontend
pageHeader = do
  autoRefresh <- asks frontendStateAutoRefresh
  (UiUpdateIntervalSeconds updateInterval) <- asks frontendStateUiUpdateInterval
  gitCommit <- asks frontendStateGitCommit
  runnersJobs <- asks frontendStateRunnersJobs
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

faviconPrefix :: (IsString a) => RunnersJobs -> a
faviconPrefix ((RunnersJobs (_, jobs))) | not (all null jobs) = "running"
faviconPrefix _ = "idle"

pageBody :: Frontend
pageBody = do
  dataUpdateInterval <- asks frontendStateDataUpdateInterval
  runnersJobs <- asks frontendStateRunnersJobs
  now <- asks frontendStateNow
  let lastUpdated :: RunnersJobs -> Frontend
      lastUpdated NoSuccessfulUpdateYet = mempty
      lastUpdated (RunnersJobs (t, _)) = lastUpdatedToHtml dataUpdateInterval now t
  body_ $ runnersJobsToHtml runnersJobs <> section_ [class_ "jobs-meta"] (lastUpdated runnersJobs)

runnersJobsToHtml :: (Monad m) => RunnersJobs -> HtmlT m ()
runnersJobsToHtml NoSuccessfulUpdateYet = div_ [class_ "job no-successful-update"] $ p_ "There was no successful update yet"
runnersJobsToHtml (RunnersJobs (_, runners)) | null runners = div_ [class_ "job empty-results"] $ p_ "No online runners found"
runnersJobsToHtml (RunnersJobs (_, runners)) = traverse_ runnerJobsToHtml (sortOn (\(runner, _) -> runnerId runner) (Data.Map.toList runners))

runnerJobsToHtml :: (Monad m) => (Runner, [Job]) -> HtmlT m ()
runnerJobsToHtml (runner, jobs) = div_ [class_ "runner-container"] $ runnerToHtml runner <> section_ [class_ "jobs"] (jobsToHtml jobs)

runnerToHtml :: (Monad m) => Runner -> HtmlT m ()
runnerToHtml Runner {..} =
  div_ [class_ "runner-info"]
    $ "#"
    <> toHtml runnerId
    <> " - "
    <> toHtml runnerDescription
    <> foldMap (\ip -> " - @" <> toHtml ip) runnerIpAddress
    <> " -"
    <> foldMap (\t -> " #" <> toHtml t) runnerTagList

jobsToHtml :: (Monad m) => [Job] -> HtmlT m ()
jobsToHtml [] = div_ [class_ "job empty"] $ p_ "No running jobs"
jobsToHtml jobs = foldl' (\acc j -> acc <> jobToHtml j) mempty jobs

jobToHtml :: (Monad m) => Job -> HtmlT m ()
jobToHtml Job {..} = a_ [href_ (show jobWebUrl), target_ "_blank", class_ "job"] $ do
  div_ [class_ "job-id"] $ "#" <> toHtml jobId
  div_ [class_ "project-name"] $ toHtml jobProjectName
  div_ [class_ "job-ref"] $ truncateRef jobRef
  div_ $ toHtml jobStage <> " > " <> toHtml jobName

deriving newtype instance ToHtml Stage

deriving newtype instance ToHtml IpAddress

deriving newtype instance ToHtml Description

deriving newtype instance ToHtml Core.Runners.Tag

truncateRef :: (Monad m) => Ref -> HtmlT m ()
truncateRef (Ref ref) | T.length ref <= 26 = toHtml ref
truncateRef (Ref ref) = toHtml $ T.take 10 ref <> "..." <> T.drop (T.length ref - 13) ref
