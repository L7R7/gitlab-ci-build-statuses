{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Inbound.HTTP.Util (AutoRefresh (..)) where

import Core.Shared
import Relude
import Text.Blaze.Html (ToMarkup, ToValue (..))

data AutoRefresh = Refresh | NoRefresh deriving stock (Eq)

instance ToValue (Url a) where
  toValue (Url url) = toValue @String (show url)

deriving newtype instance ToMarkup (Id a)

deriving newtype instance ToMarkup (Name a)
