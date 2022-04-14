{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UseCases.Shared () where

import Core.Shared
import Data.Aeson (ToJSON)

deriving newtype instance ToJSON (Id a)
