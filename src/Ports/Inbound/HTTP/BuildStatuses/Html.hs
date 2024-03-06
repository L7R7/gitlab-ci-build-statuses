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
import Lucid
import Lucid.Base (commuteHtmlT, makeAttribute)
import Path (toFilePath)
import Polysemy
import Polysemy.Reader qualified as R
import Polysemy.Time (Time)
import Polysemy.Time qualified as Time
import Ports.Inbound.HTTP.Util
import Relude
import Servant
import Servant.HTML.Lucid

type API = "statuses" :> QueryParam "view" ViewMode :> QueryFlag "norefresh" :> Get '[HTML] (Html ())

type Frontend = HtmlT (Reader FrontendState) ()

data FrontendState = FrontendState
  { frontendStateViewMode :: ViewMode,
    frontendStateJobsView :: JobsView,
    frontendStateBuildStatuses :: BuildStatuses,
    frontendStateDataUpdateInterval :: DataUpdateIntervalSeconds,
    frontendStateUiUpdateInterval :: UiUpdateIntervalSeconds,
    frontendStateAutoRefresh :: AutoRefresh,
    frontendStateGitCommit :: GitCommit,
    frontendStateNow :: UTCTime
  }

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
  Sem r (Html ())
template viewMode autoRefresh = do
  frontendState <- FrontendState viewMode <$> R.ask <*> getStatuses <*> R.ask <*> R.ask <*> pure autoRefresh <*> R.ask <*> Time.now
  pure $ usingReader frontendState $ commuteHtmlT $ do
    pageHeader
    pageBody

pageHeader :: Frontend
pageHeader = do
  autoRefresh <- asks frontendStateAutoRefresh
  (UiUpdateIntervalSeconds updateInterval) <- asks frontendStateUiUpdateInterval
  gitCommit <- asks frontendStateGitCommit
  buildStatuses <- asks frontendStateBuildStatuses
  let prefix = faviconPrefix (overallStatus buildStatuses)
  doctype_
  html_ [lang_ "en"]
    $ head_
    $ do
      meta_ [charset_ "UTF-8"]
      when (autoRefresh == Refresh) $ meta_ [httpEquiv_ "Refresh", content_ (show updateInterval)]
      title_ "Build Statuses"
      link_ [rel_ "icon", type_ "image/png", href_ ("static/" <> prefix <> "-favicon.ico")]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "static/normalize-d6d444a732.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "static/statuses-229a13b850.css"]
      script_ [type_ "text/javascript", src_ "static/script-32964cd17f.js"] ("" :: String)
      meta_ [makeAttribute "version" (show gitCommit)]

faviconPrefix :: (IsString p) => O.OverallStatus -> p
faviconPrefix status
  | status == O.Successful = "success"
  | isRunning status = "running"
  | status `elem` [O.Warning, O.Unknown] = "warning"
  | otherwise = "failed"

pageBody :: Frontend
pageBody = body_ $ do
  viewMode <- asks frontendStateViewMode
  dataUpdateInterval <- asks frontendStateDataUpdateInterval
  now <- asks frontendStateNow
  buildStatuses <- asks frontendStateBuildStatuses
  (if viewMode == Plain then section_ [class_ "statuses"] else Relude.id) $ statusesToHtml viewMode dataUpdateInterval now buildStatuses
  section_ [class_ "statuses"] $ do
    linkToViewToggle
    linkToJobs

statusesToHtml :: ViewMode -> DataUpdateIntervalSeconds -> UTCTime -> BuildStatuses -> Frontend
statusesToHtml viewMode _ _ NoSuccessfulUpdateYet = (if viewMode == Grouped then section_ [class_ "statuses-grouped"] else Relude.id) $ div_ [class_ "status no-successful-update"] $ p_ "There was no successful update yet"
statusesToHtml viewMode dataUpdateInterval now (Statuses (lastUpdated, [])) = (if viewMode == Grouped then section_ [class_ "statuses-grouped"] else Relude.id) $ do
  emptyResults
  lastUpdatedToHtml dataUpdateInterval now lastUpdated
statusesToHtml Grouped dataUpdateInterval now (Statuses (lastUpdated, results)) = do
  let grouped = M.toAscList $ M.fromListWith (flip (<>)) ((\r -> (namespace r, [r])) <$> results)
  forM_ grouped $ \(ProjectNamespaceFullPath path, r) -> section_ [class_ "statuses-grouped"] $ do
    div_ [class_ "status subgroup-name"] $ div_ $ toHtml (toFilePath path)
    traverse resultToHtml r
  lastUpdatedToHtml dataUpdateInterval now lastUpdated
statusesToHtml Plain dataUpdateInterval now (Statuses (lastUpdated, results)) = do
  traverse_ resultToHtml results
  lastUpdatedToHtml dataUpdateInterval now lastUpdated

resultToHtml :: (Monad m) => Result -> HtmlT m ()
resultToHtml Result {..} =
  a_ [href_ (either show show url), target_ "_blank", classesForStatus buildStatus, title_ (buildStatusToString buildStatus)] $ div_ (toHtml name)
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

    buildStatusToString Unknown = "unknown"
    buildStatusToString Cancelled = "cancelled"
    buildStatusToString Created = "created"
    buildStatusToString Failed = "failed"
    buildStatusToString Manual = "manual"
    buildStatusToString Pending = "pending"
    buildStatusToString Preparing = "preparing"
    buildStatusToString Running = "running"
    buildStatusToString Scheduled = "scheduled"
    buildStatusToString Skipped = "skipped"
    buildStatusToString Successful = "successful"
    buildStatusToString SuccessfulWithWarnings = "successful with warnings"
    buildStatusToString WaitingForResource = "waiting for resource"

emptyResults :: (Monad m) => HtmlT m ()
emptyResults = div_ [class_ "status empty-results"] $ p_ "No pipeline results for default branches found"

linkToJobs :: Frontend
linkToJobs = do
  jobsView <- asks frontendStateJobsView
  case jobsView of
    Enabled -> div_ [class_ "status"] $ div_ $ a_ [class_ "link-to-jobs", href_ "/builds/jobs"] "Go to the currently running jobs"
    Disabled -> mempty

linkToViewToggle :: Frontend
linkToViewToggle = do
  viewMode <- asks frontendStateViewMode
  let (href, txt) = case viewMode of
        Plain -> ("?view=grouped", "Switch to grouped view")
        Grouped -> ("?view=plain", "Switch to plain view")
  div_ [class_ "status"] $ div_ $ a_ [class_ "link-to-view-toggle", href_ href] txt
