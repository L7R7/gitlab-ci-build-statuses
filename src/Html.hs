{-# LANGUAGE OverloadedStrings #-}

module Html
  ( template
  ) where

import           Lib
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A hiding (name)

template :: [Result] -> Html
template results =
  docTypeHtml ! lang "en" $ do
    H.head $ do
      meta ! charset "UTF-8"
      meta ! httpEquiv "Refresh" ! content "300"
      H.title "Build Statuses"
    H.body $ section ! class_ "statuses" $ toHtml (resultToHtml <$> results)
    H.style
      ".status {padding: 1em;text-align: center;background: white;justify-content: center;} .successful {background-color: green;} .failed {background-color: red;} .running {background-color: blue;} .cancelled {background-color: orange;} .statuses {width: 100%;display: grid;grid-column-gap: 0.4em;grid-row-gap: 0.4em;grid-template-columns: repeat(auto-fit, minmax(11em, 10fr));} html {height: 100%;font-family: Noto Sans, Arial, sans-serif;background: black;} body {min-height: 100%}"

resultToHtml :: Result -> Html
resultToHtml (Result projectName status) =
  H.div ! classesForStatus status $ do
    h3 (toHtml projectName)
    p (toHtml status)
  where
    classesForStatus Unknown    = class_ "status unknown"
    classesForStatus Running    = class_ "status running"
    classesForStatus Failed     = class_ "status failed"
    classesForStatus Cancelled  = class_ "status cancelled"
    classesForStatus Pending    = class_ "status pending"
    classesForStatus Skipped    = class_ "status skipped"
    classesForStatus Successful = class_ "status successful"

instance ToMarkup BuildStatus where
  toMarkup Unknown    = string "unknown"
  toMarkup Running    = string "running"
  toMarkup Failed     = string "failed"
  toMarkup Cancelled  = string "cancelled"
  toMarkup Pending    = string "pending"
  toMarkup Skipped    = string "skipped"
  toMarkup Successful = string "successful"
