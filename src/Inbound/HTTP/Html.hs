{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Inbound.HTTP.Html
  ( template,
  )
where

import Config
import Core.Lib
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Env
import RIO hiding (link)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (icon, name)

template :: (HasUiUpdateInterval env, HasBuildStatuses env) => RIO env Html
template = do
  updateInterval <- view uiUpdateIntervalL
  template' updateInterval <$> getStatuses

template' :: UiUpdateIntervalSeconds -> BuildStatuses -> Html
template' updateInterval buildStatuses = do
  pageHeader updateInterval buildStatuses
  pageBody buildStatuses

pageHeader :: UiUpdateIntervalSeconds -> BuildStatuses -> Html
pageHeader (UiUpdateIntervalSeconds updateInterval) buildStatuses =
  docTypeHtml ! lang "en" $
    H.head $
      do
        meta ! charset "UTF-8"
        meta ! httpEquiv "Refresh" ! content (toValue updateInterval)
        H.title $ titleIcon buildStatuses <> " Build Statuses"
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/normalize.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/statuses.css"

titleIcon :: BuildStatuses -> Html
titleIcon NoSuccessfulUpdateYet = mempty
titleIcon (Statuses (_, results)) = H.preEscapedToHtml icon
  where
    icon :: String
    icon = if all (isHealthy . buildStatus) results then "&#10003;" else "&#10007"

pageBody :: BuildStatuses -> Html
pageBody buildStatuses = H.body $ section ! class_ "statuses" $ statusesToHtml buildStatuses

statusesToHtml :: BuildStatuses -> Html
statusesToHtml NoSuccessfulUpdateYet = H.div ! class_ "status no-successful-update" $ p "There was no successful update yet"
statusesToHtml (Statuses (lastUpdated, [])) = do
  emptyResults
  lastUpdatedToHtml lastUpdated
statusesToHtml (Statuses (lastUpdated, results)) = do
  toHtml (resultToHtml <$> results)
  lastUpdatedToHtml lastUpdated

resultToHtml :: Result -> Html
resultToHtml Result {..} =
  H.div ! classesForStatus buildStatus ! A.title (toValue buildStatus) $
    a ! href (toValue url) ! target "_blank" $ h3 (toHtml name)
  where
    classesForStatus Unknown = class_ "status unknown"
    classesForStatus Running = class_ "status running"
    classesForStatus Failed = class_ "status failed"
    classesForStatus Cancelled = class_ "status cancelled"
    classesForStatus Pending = class_ "status pending"
    classesForStatus Skipped = class_ "status skipped"
    classesForStatus Successful = class_ "status successful"
    classesForStatus Created = class_ "status created"
    classesForStatus Manual = class_ "status manual"

lastUpdatedToHtml :: UTCTime -> Html
lastUpdatedToHtml t = H.div ! class_ "status timestamp" $ do
  p "Last Update at:"
  p (toHtml $ unwords [formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t, "UTC"])

emptyResults :: Html
emptyResults = H.div ! class_ "status empty-results" $ p "No pipeline results for default branches found"

instance ToMarkup ProjectName where
  toMarkup (ProjectName pN) = toMarkup pN

instance ToValue ProjectUrl where
  toValue (ProjectUrl uri) = toValue $ show uri

instance ToMarkup BuildStatus where
  toMarkup Unknown = string "unknown"
  toMarkup Running = string "running"
  toMarkup Failed = string "failed"
  toMarkup Cancelled = string "cancelled"
  toMarkup Pending = string "pending"
  toMarkup Skipped = string "skipped"
  toMarkup Successful = string "successful"
  toMarkup Created = string "created"
  toMarkup Manual = string "manual"

instance ToValue BuildStatus where
  toValue = toValue . show
