{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ports.Inbound.HTTP.BuildStatuses.Html
  ( API,
    template,
  )
where

import Config.Backbone
import Config.Config
import Core.BuildStatuses
import Core.OverallStatus (isRunning, overallStatus)
import Core.OverallStatus qualified as O (OverallStatus (Successful, Unknown, Warning))
import Core.Shared
import Data.Map qualified as M
import Data.Time (UTCTime)
import Path (toFilePath)
import Polysemy
import Polysemy.Reader qualified as R
import Polysemy.Time (Time)
import Polysemy.Time qualified as Time
import Ports.Inbound.HTTP.Util
import Relude
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (icon, name)

type API = "statuses" :> QueryParam "view" ViewMode :> QueryFlag "norefresh" :> Get '[HTML] H.Html

template ::
  ( Member BuildStatusesApi r,
    Member (Time UTCTime d) r,
    Member (R.Reader DataUpdateIntervalSeconds) r,
    Member (R.Reader UiUpdateIntervalSeconds) r,
    Member (R.Reader GitCommit) r,
    Member (R.Reader JobsView) r
  ) =>
  ViewMode ->
  AutoRefresh ->
  Sem r Html
template viewMode autoRefresh = do
  jobsView <- R.ask
  dataUpdateInterval <- R.ask
  uiUpdateInterval <- R.ask
  gitCommit <- R.ask
  now <- Time.now
  template' now jobsView dataUpdateInterval uiUpdateInterval gitCommit autoRefresh viewMode <$> getStatuses

template' :: UTCTime -> JobsView -> DataUpdateIntervalSeconds -> UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> ViewMode -> BuildStatuses -> Html
template' now jobsView dataUpdateInterval uiUpdateInterval gitCommit autoRefresh viewMode buildStatuses = do
  pageHeader uiUpdateInterval gitCommit autoRefresh buildStatuses
  pageBody viewMode dataUpdateInterval jobsView now buildStatuses

pageHeader :: UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> BuildStatuses -> Html
pageHeader (UiUpdateIntervalSeconds updateInterval) gitCommit autoRefresh buildStatuses =
  docTypeHtml ! lang "en" $
    H.head $
      do
        meta ! charset "UTF-8"
        when (autoRefresh == Refresh) $ meta ! httpEquiv "Refresh" ! content (toValue updateInterval)
        H.title "Build Statuses"
        link ! rel "icon" ! type_ "image/png" ! href ("static/" <> prefix <> "-favicon.ico")
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/normalize-d6d444a732.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/statuses-229a13b850.css"
        script ! type_ "text/javascript" ! src "static/script-32964cd17f.js" $ mempty
        textComment . toText $ ("Version: " <> show gitCommit :: String)
  where
    prefix = faviconPrefix (overallStatus buildStatuses)

faviconPrefix :: (IsString p) => O.OverallStatus -> p
faviconPrefix status
  | status == O.Successful = "success"
  | isRunning status = "running"
  | status `elem` [O.Warning, O.Unknown] = "warning"
  | otherwise = "failed"

pageBody :: ViewMode -> DataUpdateIntervalSeconds -> JobsView -> UTCTime -> BuildStatuses -> Html
pageBody viewMode dataUpdateInterval jobsView now buildStatuses = H.body $ do
  (if viewMode == Plain then section ! class_ "statuses" else Relude.id) $ do
    statusesToHtml viewMode dataUpdateInterval now buildStatuses
  section ! class_ "statuses" $ do
    linkToViewToggle viewMode
    linkToJobs jobsView

statusesToHtml :: ViewMode -> DataUpdateIntervalSeconds -> UTCTime -> BuildStatuses -> Html
statusesToHtml viewMode _ _ NoSuccessfulUpdateYet = (if viewMode == Grouped then section ! class_ "statuses-grouped" else Relude.id) $ H.div ! class_ "status no-successful-update" $ p "There was no successful update yet"
statusesToHtml viewMode dataUpdateInterval now (Statuses (lastUpdated, [])) = (if viewMode == Grouped then section ! class_ "statuses-grouped" else Relude.id) $ do
  emptyResults
  lastUpdatedToHtml dataUpdateInterval now lastUpdated
statusesToHtml Grouped dataUpdateInterval now (Statuses (lastUpdated, results)) = do
  let grouped = M.toAscList $ M.fromListWith (flip (<>)) ((\r -> (namespace r, [r])) <$> results)
  forM_ grouped $ \(ProjectNamespaceFullPath path, r) -> section ! class_ "statuses-grouped" $ do
    H.div ! class_ "status subgroup-name" $ H.div $ toHtml (toFilePath path)
    toHtml (resultToHtml <$> r)
  lastUpdatedToHtml dataUpdateInterval now lastUpdated
statusesToHtml Plain dataUpdateInterval now (Statuses (lastUpdated, results)) = do
  toHtml (resultToHtml <$> results)
  lastUpdatedToHtml dataUpdateInterval now lastUpdated

resultToHtml :: Result -> Html
resultToHtml Result {..} =
  a ! href (either toValue toValue url) ! target "_blank" ! classesForStatus buildStatus ! A.title (toValue buildStatus) $ H.div (toHtml name)
  where
    classesForStatus Unknown = class_ "status unknown"
    classesForStatus Cancelled = class_ "status cancelled"
    classesForStatus Created = class_ "status created"
    classesForStatus Failed = class_ "status failed"
    classesForStatus Manual = class_ "status manual"
    classesForStatus Pending = class_ "status pending"
    classesForStatus Preparing = class_ "status preparing"
    classesForStatus Running = class_ "status running"
    classesForStatus Scheduled = class_ "status scheduled"
    classesForStatus Skipped = class_ "status skipped"
    classesForStatus Successful = class_ "status successful"
    classesForStatus SuccessfulWithWarnings = class_ "status passed-with-warnings"
    classesForStatus WaitingForResource = class_ "status waiting-for-resource"

emptyResults :: Html
emptyResults = H.div ! class_ "status empty-results" $ p "No pipeline results for default branches found"

linkToJobs :: JobsView -> Html
linkToJobs Disabled = mempty
linkToJobs Enabled = H.div ! class_ "status" $ H.div $ a ! class_ "link-to-jobs" ! href "/builds/jobs" $ "Go to the currently running jobs"

linkToViewToggle :: ViewMode -> Html
linkToViewToggle Grouped = H.div ! class_ "status" $ H.div $ a ! class_ "link-to-view-toggle" ! href "?view=plain" $ "Switch to plain view"
linkToViewToggle Plain = H.div ! class_ "status" $ H.div $ a ! class_ "link-to-view-toggle" ! href "?view=grouped" $ "Switch to grouped view"
