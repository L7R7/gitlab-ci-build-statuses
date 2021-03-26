{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Inbound.HTTP.Html
  ( template,
    AutoRefresh (..),
  )
where

import Config
import Core.Effects (Timer, getCurrentTime)
import Core.Lib
import Core.OverallStatus (isRunning, overallStatus)
import qualified Core.OverallStatus as O (OverallStatus (Successful, Unknown, Warning))
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Polysemy
import Relude
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (icon, name)

data AutoRefresh = Refresh | NoRefresh deriving (Eq)

template :: (Member BuildStatusesApi r, Member Timer r) => UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> Sem r Html
template updateInterval gitCommit autoRefresh = do
  now <- getCurrentTime
  template' now updateInterval gitCommit autoRefresh <$> getStatuses

template' :: UTCTime -> UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> BuildStatuses -> Html
template' now updateInterval gitCommit autoRefresh buildStatuses = do
  pageHeader updateInterval gitCommit autoRefresh buildStatuses
  pageBody now buildStatuses

pageHeader :: UiUpdateIntervalSeconds -> GitCommit -> AutoRefresh -> BuildStatuses -> Html
pageHeader (UiUpdateIntervalSeconds updateInterval) gitCommit autoRefresh buildStatuses =
  docTypeHtml ! lang "en" $
    H.head $
      do
        meta ! charset "UTF-8"
        unless (autoRefresh == NoRefresh) $ meta ! httpEquiv "Refresh" ! content (toValue updateInterval)
        H.title "Build Statuses"
        link ! rel "icon" ! type_ "image/png" ! href ("static/" <> prefix <> "-favicon.ico")
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/normalize-d6d444a732.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/statuses-0a92ee62ac.css"
        textComment . toText $ ("Version: " <> show gitCommit :: String)
  where
    prefix = faviconPrefix (overallStatus buildStatuses)

faviconPrefix :: IsString p => O.OverallStatus -> p
faviconPrefix status
  | status == O.Successful = "success"
  | isRunning status = "running"
  | status `elem` [O.Warning, O.Unknown] = "warning"
  | otherwise = "failed"

pageBody :: UTCTime -> BuildStatuses -> Html
pageBody now buildStatuses = H.body $ section ! class_ "statuses" $ statusesToHtml now buildStatuses

statusesToHtml :: UTCTime -> BuildStatuses -> Html
statusesToHtml _ NoSuccessfulUpdateYet = H.div ! class_ "status no-successful-update" $ p "There was no successful update yet"
statusesToHtml now (Statuses (lastUpdated, [])) = do
  emptyResults
  lastUpdatedToHtml now lastUpdated
statusesToHtml now (Statuses (lastUpdated, results)) = do
  toHtml (resultToHtml <$> results)
  lastUpdatedToHtml now lastUpdated

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

lastUpdatedToHtml :: UTCTime -> UTCTime -> Html
lastUpdatedToHtml now lastUpdate = H.div ! class_ classes ! staleDataTitle $
  H.div $ do
    p "Last Update at:"
    p (toHtml $ unwords [toText $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lastUpdate, "UTC"])
  where
    lastUpdateTooOld = diffUTCTime now lastUpdate > 360
    staleDataTitle
      | lastUpdateTooOld = A.title "data is stale. Please check the logs"
      | otherwise = mempty
    classes
      | lastUpdateTooOld = "status timestamp cancelled"
      | otherwise = "status timestamp"

emptyResults :: Html
emptyResults = H.div ! class_ "status empty-results" $ p "No pipeline results for default branches found"

instance ToMarkup ProjectName where
  toMarkup (ProjectName pN) = toMarkup pN

instance ToValue (Url a) where
  toValue (Url url) = toValue @String (show url)

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
