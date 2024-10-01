{-# OPTIONS_GHC -fno-warn-orphans #-}

module UseCases.Shared () where

import Data.Aeson (ToJSON)
import Gitlab.Lib (Id (..))

deriving newtype instance ToJSON (Id a)
