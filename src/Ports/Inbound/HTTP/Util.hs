{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ports.Inbound.HTTP.Util (AutoRefresh (..), ViewMode (..), lastUpdatedToHtml) where

import Core.Shared
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Lucid
import Relude
import Servant (FromHttpApiData (..))

data AutoRefresh = Refresh | NoRefresh deriving stock (Eq)

data ViewMode = Plain | Grouped deriving stock (Bounded, Eq, Enum)

viewModeToText :: ViewMode -> Text
viewModeToText Plain = "plain"
viewModeToText Grouped = "grouped"

instance FromHttpApiData ViewMode where
  parseQueryParam = maybeToRight "can't parse ViewMode param" . inverseMap viewModeToText

instance ToHtml (Id a) where
  toHtml (Id i) = toHtml @String (show i)
  toHtmlRaw (Id i) = toHtmlRaw @String (show i)

deriving newtype instance ToHtml (Name a)

lastUpdatedToHtml :: DataUpdateIntervalSeconds -> UTCTime -> UTCTime -> Html ()
lastUpdatedToHtml (DataUpdateIntervalSeconds updateInterval) now lastUpdate = div_ ([class_ classes] <> staleDataTitle) $ div_ $ do
  p_ "Last Update at:"
  p_ [id_ "update-timestamp"] $ toHtml (iso8601Show lastUpdate)
  where
    lastUpdateTooOld = diffUTCTime now lastUpdate > fromIntegral (3 * updateInterval)
    staleDataTitle
      | lastUpdateTooOld = [title_ "data is stale. Please check the logs"]
      | otherwise = mempty
    classes
      | lastUpdateTooOld = "job status timestamp cancelled"
      | otherwise = "job status timestamp"
