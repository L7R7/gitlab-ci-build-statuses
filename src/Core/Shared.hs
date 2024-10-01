{-# LANGUAGE DerivingVia #-}

module Core.Shared
  ( DataUpdateIntervalSeconds (..),
    UpdateError (..),
  )
where

import Data.ByteString.Lazy qualified as L
import Network.HTTP.Simple (HttpException, Request, Response)
import Network.HTTP.Types (Status)
import Relude

data UpdateError
  = HttpError HttpException
  | JSONError Request (Response L.ByteString) String
  | RequestFailedWithStatus Request Status
  | EmptyResult
  deriving stock (Show)

newtype DataUpdateIntervalSeconds = DataUpdateIntervalSeconds Int
  deriving stock (Show)
  deriving (Num) via Int
  deriving newtype (Eq)
