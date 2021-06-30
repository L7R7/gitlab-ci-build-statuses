{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Inbound.HTTP.BuildStatuses.Html
  ( API,
    template,
  )
where

import Config.Backbone
import Config.Config
import Core.BuildStatuses
import Core.OverallStatus (isRunning, overallStatus)
import qualified Core.OverallStatus as O (OverallStatus (Successful, Unknown, Warning))
import Core.Shared
import Data.Time (UTCTime)
import Inbound.HTTP.Util
import Polysemy
import qualified Polysemy.Reader as R
import Polysemy.Time (Time)
import qualified Polysemy.Time as Time
import Relude
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (icon, name)

type API = "statuses" :> QueryFlag "norefresh" :> Get '[HTML] H.Html

template :: (Member BuildStatusesApi r, Member (Time UTCTime d) r, Member (R.Reader DataUpdateIntervalSeconds) r, Member (R.Reader UiUpdateIntervalSeconds) r, Member (R.Reader GitCommit) r, Member (R.Reader JobsView) r) => AutoRefresh -> Sem r Html
template autoRefresh = do
  jobsView <- R.ask
  dataUpdateInterval <- R.ask
  uiUpdateInterval <- R.ask
  gitCommit <- R.ask
  now <- Time.now
  template' now jobsView dataUpdateInterval uiUpdateInterval gitCommit autoRefresh <$> getStatuses

template' :: UTCTime -> JobsView -> DataUpdateIntervalSeconds -> UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> BuildStatuses -> Html
template' now jobsView dataUpdateInterval uiUpdateInterval gitCommit autoRefresh buildStatuses = do
  pageHeader uiUpdateInterval gitCommit autoRefresh buildStatuses
  pageBody dataUpdateInterval jobsView now buildStatuses

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
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/statuses-484548c1a7.css"
        script ! type_ "text/javascript" ! src "static/script-32964cd17f.js" $ mempty
        textComment . toText $ ("Version: " <> show gitCommit :: String)
  where
    prefix = faviconPrefix (overallStatus buildStatuses)

faviconPrefix :: IsString p => O.OverallStatus -> p
faviconPrefix status
  | status == O.Successful = "success"
  | isRunning status = "running"
  | status `elem` [O.Warning, O.Unknown] = "warning"
  | otherwise = "failed"

pageBody :: DataUpdateIntervalSeconds -> JobsView -> UTCTime -> BuildStatuses -> Html
pageBody dataUpdateInterval jobsView now buildStatuses = H.body $
  section ! class_ "statuses" $ do
    statusesToHtml dataUpdateInterval jobsView now buildStatuses

statusesToHtml :: DataUpdateIntervalSeconds -> JobsView -> UTCTime -> BuildStatuses -> Html
statusesToHtml _ _ _ NoSuccessfulUpdateYet = H.div ! class_ "status no-successful-update" $ p "There was no successful update yet"
statusesToHtml dataUpdateInterval jobsView now (Statuses (lastUpdated, [])) = do
  emptyResults
  lastUpdatedToHtml dataUpdateInterval now lastUpdated
  linkToJobs jobsView
statusesToHtml dataUpdateInterval jobsView now (Statuses (lastUpdated, results)) = do
  toHtml (resultToHtml <$> results)
  lastUpdatedToHtml dataUpdateInterval now lastUpdated
  linkToJobs jobsView

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
linkToJobs Enabled = H.div ! class_ "status" $ H.div $ a ! class_ "link-to-jobs" ! href "/builds/jobs" $ "Go to the current running jobs"

instance ToMarkup BuildStatus where
  toMarkup Unknown = string "unknown"
  toMarkup Cancelled = string "cancelled"
  toMarkup Created = string "created"
  toMarkup Failed = string "failed"
  toMarkup Manual = string "manual"
  toMarkup Pending = string "pending"
  toMarkup Preparing = string "preparing"
  toMarkup Running = string "running"
  toMarkup Scheduled = string "scheduled"
  toMarkup Skipped = string "skipped"
  toMarkup Successful = string "successful"
  toMarkup SuccessfulWithWarnings = string "successful with warnings"
  toMarkup WaitingForResource = string "waiting for resource"

instance ToValue BuildStatus where
  toValue = toValue @String . show
