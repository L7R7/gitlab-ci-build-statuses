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
import RIO
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
  pageFooter

pageHeader :: UiUpdateIntervalSeconds -> BuildStatuses -> Html
pageHeader (UiUpdateIntervalSeconds updateInterval) buildStatuses =
  docTypeHtml ! lang "en" $
    H.head $
      do
        meta ! charset "UTF-8"
        meta ! httpEquiv "Refresh" ! content (toValue updateInterval)
        H.title $ titleIcon buildStatuses <> " Build Statuses"

titleIcon :: BuildStatuses -> Html
titleIcon NoSuccessfulUpdateYet = mempty
titleIcon (Statuses (_, results)) = H.preEscapedToHtml icon
  where
    icon :: String
    icon = if all (\r -> buildStatus r == Successful) results then "&#10003;" else "&#10007"

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

pageFooter :: Html
pageFooter = do
  H.style "/*! normalize.css v8.0.1 | MIT License | github.com/necolas/normalize.css */ html {line-height: 1.15; /* 1 */-webkit-text-size-adjust: 100%; /* 2 */} body {margin: 0;}  main {display: block;}  h1 {font-size: 2em;margin: 0.67em 0;}  hr {box-sizing: content-box; /* 1 */height: 0; /* 1 */overflow: visible; /* 2 */}  pre {font-family: monospace, monospace; /* 1 */font-size: 1em; /* 2 */}  a {background-color: transparent;}  abbr[title] {border-bottom: none; /* 1 */text-decoration: underline; /* 2 */text-decoration: underline dotted; /* 2 */}  b, strong {font-weight: bolder;}  code, kbd, samp {font-family: monospace, monospace; /* 1 */font-size: 1em; /* 2 */}  small {font-size: 80%;}  sub, sup {font-size: 75%;line-height: 0;position: relative;vertical-align: baseline;}  sub {bottom: -0.25em;}  sup {top: -0.5em;}  img {border-style: none;}  button, input, optgroup, select, textarea {font-family: inherit; /* 1 */font-size: 100%; /* 1 */line-height: 1.15; /* 1 */margin: 0; /* 2 */}  button, input { /* 1 */overflow: visible;}  button, select { /* 1 */text-transform: none;}  button, [type=\"button\"], [type=\"reset\"], [type=\"submit\"] {-webkit-appearance: button;}  button::-moz-focus-inner, [type=\"button\"]::-moz-focus-inner, [type=\"reset\"]::-moz-focus-inner, [type=\"submit\"]::-moz-focus-inner {border-style: none;padding: 0;}  button:-moz-focusring, [type=\"button\"]:-moz-focusring, [type=\"reset\"]:-moz-focusring, [type=\"submit\"]:-moz-focusring {outline: 1px dotted ButtonText;}  fieldset {padding: 0.35em 0.75em 0.625em;}  legend {box-sizing: border-box; /* 1 */color: inherit; /* 2 */display: table; /* 1 */max-width: 100%; /* 1 */padding: 0; /* 3 */white-space: normal; /* 1 */}  progress {vertical-align: baseline;}  textarea {overflow: auto;}  [type=\"checkbox\"], [type=\"radio\"] {box-sizing: border-box; /* 1 */padding: 0; /* 2 */}  [type=\"number\"]::-webkit-inner-spin-button, [type=\"number\"]::-webkit-outer-spin-button {height: auto;}  [type=\"search\"] {-webkit-appearance: textfield; /* 1 */outline-offset: -2px; /* 2 */}  [type=\"search\"]::-webkit-search-decoration {-webkit-appearance: none;}  ::-webkit-file-upload-button {-webkit-appearance: button; /* 1 */font: inherit; /* 2 */}  details {display: block;}  summary {display: list-item;}  template {display: none;}  [hidden] {display: none;}"
  H.style ".status {padding: 1em;text-align: center;background: white;justify-content: center;} .empty-results, .no-successful-update, .timestamp {background-color: transparent; color: white} .successful {background-color: green;} .failed {background-color: red;} .running {background-color: blue;} .cancelled {background-color: orange;} .statuses {width: 100%;display: grid;grid-column-gap: 0.4em;grid-row-gap: 0.4em;grid-template-columns: repeat(auto-fit, minmax(11em, 10fr));} a {text-decoration: none;color: black;} html {height: 100%;font-family: Noto Sans, Arial, sans-serif;background: black;} body {margin: 0;padding: 8px}"

resultToHtml :: Result -> Html
resultToHtml Result {..} =
  H.div ! classesForStatus buildStatus $
    a ! href (toValue url) ! target "_blank" $ do
      h3 (toHtml name)
      p (toHtml buildStatus)
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
