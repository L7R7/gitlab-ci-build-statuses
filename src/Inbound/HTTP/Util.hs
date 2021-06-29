{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Inbound.HTTP.Util (AutoRefresh (..), lastUpdatedToHtml) where

import Core.Shared
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Relude
import Text.Blaze.Html
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

data AutoRefresh = Refresh | NoRefresh deriving stock (Eq)

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
