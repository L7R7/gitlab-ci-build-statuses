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

type API = "statuses" :> QueryParam "view" ViewMode :> QueryFlag "norefresh" :> QueryParam "filter" FilterMode :> QueryParam "ui" UiMode :> Get '[HTML] (Html ())

type Frontend = HtmlT (Reader FrontendState) ()

data FrontendState = FrontendState
  { frontendStateViewMode :: ViewMode,
    frontendStateFilterMode :: FilterMode,
    frontendStateUiMode :: UiMode,
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
  FilterMode ->
  UiMode ->
  AutoRefresh ->
  Sem r (Html ())
template viewMode filterMode uiMode autoRefresh = do
  frontendState <- FrontendState viewMode filterMode uiMode <$> R.ask <*> getStatuses <*> R.ask <*> R.ask <*> pure autoRefresh <*> R.ask <*> Time.now
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
      link_ [rel_ "stylesheet", type_ "text/css", href_ "static/statuses-11c70488b8.css"]
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
  filterMode <- asks frontendStateFilterMode
  dataUpdateInterval <- asks frontendStateDataUpdateInterval
  now <- asks frontendStateNow
  buildStatuses <- asks frontendStateBuildStatuses
  let buildStatusesFiltered = case filterMode of
        ShowAll -> buildStatuses
        DontShowSuccessful -> filterResults buildStatuses (\res -> buildStatus res /= Successful)
  (if viewMode == Plain then section_ [class_ "statuses"] else Relude.id) $ statusesToHtml viewMode dataUpdateInterval now buildStatusesFiltered
  section_ [classes_ ["statuses", "controls"]] $ do
    linkToViewToggle
    linkToAutoRefreshToggle
    linkToFilterModeToggle
    linkToUiModeToggle
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

resultToHtml :: Result -> Frontend
resultToHtml result = do
  uiMode <- asks frontendStateUiMode
  case uiMode of
    Colored -> resultToHtml' result
    NoColor -> resultToHtml'' result

resultToHtml' :: (Monad m) => Result -> HtmlT m ()
resultToHtml' Result {..} =
  a_ [href_ (either show show url), target_ "_blank", classesForStatus buildStatus, title_ (buildStatusToString buildStatus)] $ do
    div_ [class_ "status-content"] $ p_ (toHtml name)
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

resultToHtml'' :: (Monad m) => Result -> HtmlT m ()
resultToHtml'' Result {..} =
  a_ [href_ (either show show url), target_ "_blank", classes_ ["status", "no-color"], title_ (buildStatusToString buildStatus), style_ "position: relative"] $ do
    div_ [class_ "status-content"] $ do
      p_ (toHtml name)
      p_ [class_ "textual-status"] (buildStatusToString buildStatus)
    section_ [class_ "icon-box", style_ "position: absolute; height: 100%; width: 100%; display: grid; grid-template-columns: repeat(4, 1fr); grid-auto-rows: 48px;"] $ do
      if isBroken buildStatus
        then replicateM_ 16 (toHtmlRaw ("<svg viewBox=\"0 0 48 48\"><polygon fill=\"#c41934\" points=\"36,0 0,37.4 2.8,40.2 38.8,2.7\"/><polygon fill=\"#c41934\" points=\"0,2.8 2.8,0 38.8,37.4 36,40.2\"/></svg>" :: String))
        else pure ()
      if isHealthy buildStatus
        then replicateM_ 16 (toHtmlRaw ("<svg viewBox=\"0 0 48 48\"><polygon fill=\"#43A047\" points=\"36,0 12.4,23.6 2.8,14.0 0,16.9 12.4,29.1 38.8,2.7\"/></svg>" :: String))
        else pure ()

buildStatusToString :: (IsString a) => BuildStatus -> a
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

emptyResults :: Frontend
emptyResults = do
  filterMode <- asks frontendStateFilterMode
  let txt = case filterMode of
        ShowAll -> "No pipeline results for default branches found"
        DontShowSuccessful -> "No pipeline results for default branches that are not successful"
  div_ [class_ "status empty-results"] $ p_ txt

linkToJobs :: Frontend
linkToJobs = do
  jobsView <- asks frontendStateJobsView
  case jobsView of
    Enabled -> div_ [class_ "status"] $ div_ $ a_ [class_ "link-to-jobs", href_ "/builds/jobs"] "Go to the currently running jobs"
    Disabled -> mempty

linkToViewToggle :: Frontend
linkToViewToggle = do
  frontendState <- ask
  let viewMode = frontendStateViewMode frontendState
      toggle Grouped = Plain
      toggle Plain = Grouped
      txt = case viewMode of
        Plain -> "Switch to grouped view"
        Grouped -> "Switch to plain view"
  div_ [class_ "status"] $ div_ $ a_ [class_ "link-control", href_ (toUrlPiece (linkForState (frontendState {frontendStateViewMode = toggle viewMode})))] txt

linkToAutoRefreshToggle :: Frontend
linkToAutoRefreshToggle = do
  frontendState <- ask
  let autoRefresh = frontendStateAutoRefresh frontendState
      toggle Refresh = NoRefresh
      toggle NoRefresh = Refresh
      txt = case autoRefresh of
        Refresh -> "Disable auto refresh"
        NoRefresh -> "Enable auto refresh"
  div_ [class_ "status"] $ div_ $ a_ [class_ "link-control", href_ (toUrlPiece (linkForState (frontendState {frontendStateAutoRefresh = toggle autoRefresh})))] txt

linkToFilterModeToggle :: Frontend
linkToFilterModeToggle = do
  frontendState <- ask
  let filterMode = frontendStateFilterMode frontendState
      toggle ShowAll = DontShowSuccessful
      toggle DontShowSuccessful = ShowAll
      txt = case filterMode of
        ShowAll -> "Don't show successful pipelines"
        DontShowSuccessful -> "Show all pipelines"
  div_ [class_ "status"] $ div_ $ a_ [class_ "link-control", href_ (toUrlPiece (linkForState (frontendState {frontendStateFilterMode = toggle filterMode})))] txt

linkToUiModeToggle :: Frontend
linkToUiModeToggle = do
  frontendState <- ask
  let uiMode = frontendStateUiMode frontendState
      toggle Colored = NoColor
      toggle NoColor = Colored
      txt = case uiMode of
        Colored -> "Switch to accessibility mode"
        NoColor -> "Switch to colored mode"
  div_ [class_ "status"] $ div_ $ a_ [class_ "link-control", href_ (toUrlPiece (linkForState (frontendState {frontendStateUiMode = toggle uiMode})))] txt

linkForState :: FrontendState -> Link
linkForState frontendState = safeLink (Proxy @API) (Proxy @API) (Just (frontendStateViewMode frontendState)) (frontendStateAutoRefresh frontendState == NoRefresh) (Just (frontendStateFilterMode frontendState)) (Just (frontendStateUiMode frontendState))
