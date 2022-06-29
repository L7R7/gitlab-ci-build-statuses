{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ports.Inbound.HTTP.Util (AutoRefresh (..), ViewMode (..), lastUpdatedToHtml) where

import Core.BuildStatuses (BuildStatus (..))
import Core.Shared
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Relude
import Servant (FromHttpApiData (..))
import Text.Blaze.Html
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

data AutoRefresh = Refresh | NoRefresh deriving stock (Eq)

data ViewMode = Plain | Grouped deriving stock (Bounded, Eq, Enum)

viewModeToText :: ViewMode -> Text
viewModeToText Plain = "plain"
viewModeToText Grouped = "grouped"

instance FromHttpApiData ViewMode where
  parseQueryParam = maybeToRight "can't parse ViewMode param" . inverseMap viewModeToText

instance ToValue (Url a) where
  toValue (Url url) = toValue @String (show url)

deriving newtype instance ToMarkup (Id a)

deriving newtype instance ToMarkup (Name a)

lastUpdatedToHtml :: DataUpdateIntervalSeconds -> UTCTime -> UTCTime -> Html
lastUpdatedToHtml (DataUpdateIntervalSeconds updateInterval) now lastUpdate = H.div ! class_ classes ! staleDataTitle $
  H.div $ do
    p "Last Update at:"
    p ! A.id "update-timestamp" $ toHtml (iso8601Show lastUpdate)
  where
    lastUpdateTooOld = diffUTCTime now lastUpdate > fromIntegral (3 * updateInterval)
    staleDataTitle
      | lastUpdateTooOld = A.title "data is stale. Please check the logs"
      | otherwise = mempty
    classes
      | lastUpdateTooOld = "job status timestamp cancelled"
      | otherwise = "job status timestamp"

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
