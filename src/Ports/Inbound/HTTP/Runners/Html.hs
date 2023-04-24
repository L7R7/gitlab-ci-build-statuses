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
import Polysemy
import Polysemy.Reader qualified as R
import Polysemy.Time (Time)
import Polysemy.Time qualified as Time
import Ports.Inbound.HTTP.Util (AutoRefresh (Refresh), lastUpdatedToHtml)
import Relude
import Servant (Get, QueryFlag, (:>))
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (icon, name)

type API = "jobs" :> QueryFlag "norefresh" :> Get '[HTML] H.Html

template ::
  ( Member RunnersJobsApi r,
    Member (Time UTCTime d) r,
    Member (R.Reader JobsView) r,
    Member (R.Reader DataUpdateIntervalSeconds) r,
    Member (R.Reader UiUpdateIntervalSeconds) r,
    Member (R.Reader GitCommit) r
  ) =>
  AutoRefresh ->
  Sem r Html
template autoRefresh = do
  dataUpdateInterval <- R.ask
  uiUpdateInterval <- R.ask
  gitCommit <- R.ask
  jobsView <- R.ask
  now <- Time.now
  if jobsView == Enabled
    then template' now dataUpdateInterval uiUpdateInterval gitCommit autoRefresh <$> R.getJobs
    else pure $ runnersViewDisabled uiUpdateInterval gitCommit autoRefresh

runnersViewDisabled :: UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> Html
runnersViewDisabled uiUpdateInterval gitCommit autoRefresh = do
  pageHeader uiUpdateInterval gitCommit autoRefresh Nothing
  H.body $ H.div ! class_ "job no-successful-update" $ p "Runners view is disabled. Update your config to enable it"

template' :: UTCTime -> DataUpdateIntervalSeconds -> UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> RunnersJobs -> Html
template' now dataUpdateInterval uiUpdateInterval gitCommit autoRefresh runnersJobs = do
  pageHeader uiUpdateInterval gitCommit autoRefresh (Just runnersJobs)
  pageBody dataUpdateInterval now runnersJobs

pageHeader :: UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> Maybe RunnersJobs -> Html
pageHeader (UiUpdateIntervalSeconds updateInterval) gitCommit autoRefresh runnersJobs =
  docTypeHtml ! lang "en" $
    H.head $
      do
        meta ! charset "UTF-8"
        when (autoRefresh == Refresh) $ meta ! httpEquiv "Refresh" ! content (toValue updateInterval)
        H.title "Running jobs per runner"
        link ! rel "icon" ! type_ "image/png" ! href ("static/" <> faviconPrefix runnersJobs <> "-favicon.ico")
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/normalize-d6d444a732.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/jobs-2e8833d035.css"
        script ! type_ "text/javascript" ! src "static/script-32964cd17f.js" $ mempty
        textComment . toText $ ("Version: " <> show gitCommit :: String)

faviconPrefix :: Maybe RunnersJobs -> AttributeValue
faviconPrefix (Just (RunnersJobs (_, jobs))) | not (all null jobs) = "running"
faviconPrefix Nothing = "failed"
faviconPrefix _ = "idle"

pageBody :: DataUpdateIntervalSeconds -> UTCTime -> RunnersJobs -> Html
pageBody dataUpdateInterval now runnersJobs = H.body $ runnersJobsToHtml runnersJobs <> (section ! class_ "jobs-meta" $ lastUpdated runnersJobs)
  where
    lastUpdated :: RunnersJobs -> Html
    lastUpdated NoSuccessfulUpdateYet = mempty
    lastUpdated (RunnersJobs (t, _)) = lastUpdatedToHtml dataUpdateInterval now t

runnersJobsToHtml :: RunnersJobs -> Html
runnersJobsToHtml NoSuccessfulUpdateYet = H.div ! class_ "job no-successful-update" $ p "There was no successful update yet"
runnersJobsToHtml (RunnersJobs (_, runners)) | null runners = H.div ! class_ "job empty-results" $ p "No online runners found"
runnersJobsToHtml (RunnersJobs (_, runners)) = toHtml $ runnerJobsToHtml <$> sortOn (\(runner, _) -> runnerId runner) (Data.Map.toList runners)

runnerJobsToHtml :: (Runner, [Job]) -> Html
runnerJobsToHtml (runner, jobs) = H.div ! class_ "runner-container" $ runnerToHtml runner <> (section ! class_ "jobs" $ jobsToHtml jobs)

runnerToHtml :: Runner -> Html
runnerToHtml Runner {..} =
  H.div ! class_ "runner-info" $
    "#"
      <> toHtml runnerId
      <> " - "
      <> toHtml runnerDescription
      <> " - @"
      <> toHtml runnerIpAddress
      <> " -"
      <> foldMap (\t -> " #" <> toHtml t) runnerTagList

jobsToHtml :: [Job] -> Html
jobsToHtml [] = H.div ! class_ "job empty" $ p "No running jobs"
jobsToHtml jobs = foldl' (\acc j -> acc <> jobToHtml j) mempty jobs

jobToHtml :: Job -> Html
jobToHtml Job {..} = a ! href (toValue jobWebUrl) ! target "_blank" ! class_ "job" $ do
  H.div ! class_ "job-id" $ "#" <> toHtml jobId
  H.div ! class_ "project-name" $ toHtml jobProjectName
  H.div ! class_ "job-ref" $ truncateRef jobRef
  H.div $ toHtml jobStage <> " > " <> toHtml jobName

deriving newtype instance ToMarkup Stage

deriving newtype instance ToMarkup IpAddress

deriving newtype instance ToMarkup Description

deriving newtype instance ToMarkup Core.Runners.Tag

truncateRef :: Ref -> Html
truncateRef (Ref ref) | T.length ref <= 26 = toHtml ref
truncateRef (Ref ref) = toHtml $ T.take 10 ref <> "..." <> T.drop (T.length ref - 13) ref
