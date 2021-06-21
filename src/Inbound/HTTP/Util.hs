module Inbound.HTTP.Util (AutoRefresh (..)) where

import Relude

data AutoRefresh = Refresh | NoRefresh deriving stock (Eq)
