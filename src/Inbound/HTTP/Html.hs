{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Inbound.HTTP.Html
  ( template,
  )
where

import Config
import Core.Effects (Timer, getCurrentTime)
import Core.Lib
import Data.Text (pack)
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Polysemy
import RIO hiding (link)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (icon, name)

template :: (Member BuildStatusesApi r, Member Timer r) => UiUpdateIntervalSeconds -> GitCommit -> Sem r Html
template updateInterval gitCommit = do
  now <- getCurrentTime
  template' now updateInterval gitCommit <$> getStatuses

template' :: UTCTime -> UiUpdateIntervalSeconds -> GitCommit -> BuildStatuses -> Html
template' now updateInterval gitCommit buildStatuses = do
  pageHeader updateInterval gitCommit buildStatuses
  pageBody now buildStatuses

pageHeader :: UiUpdateIntervalSeconds -> GitCommit -> BuildStatuses -> Html
pageHeader (UiUpdateIntervalSeconds updateInterval) gitCommit buildStatuses =
  docTypeHtml ! lang "en" $
    H.head $
      do
        meta ! charset "UTF-8"
        meta ! httpEquiv "Refresh" ! content (toValue updateInterval)
        H.title $ titleIcon buildStatuses <> " Build Statuses"
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/normalize.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/statuses.css"
        textComment . pack $ "Version: " <> show gitCommit

titleIcon :: BuildStatuses -> Html
titleIcon NoSuccessfulUpdateYet = mempty
titleIcon (Statuses (_, results)) = H.preEscapedToHtml icon
  where
    icon :: String
    icon = if all (isHealthy . buildStatus) results then "&#10003;" else "&#10007"

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
  a ! href (either toValue toValue url) ! target "_blank" ! classesForStatus buildStatus ! A.title (toValue buildStatus) $ h3 (toHtml name)
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
lastUpdatedToHtml now lastUpdate = H.div ! class_ classes ! staleDataTitle $ do
  p "Last Update at:"
  p (toHtml $ unwords [formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lastUpdate, "UTC"])
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
  toValue (Url url) = toValue $ show url

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
  toValue = toValue . show
