{-# LANGUAGE DerivingVia #-}

module Core.Shared
  ( DataUpdateIntervalSeconds (..),
    Group,
    Id (..),
    Url (..),
    Name (..),
    UpdateError (..),
    Ref (..),
  )
where

import qualified Data.ByteString.Lazy as L
import Network.HTTP.Simple (HttpException, Request, Response)
import Network.HTTP.Types (Status)
import Network.URI
import Relude

data Group

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

newtype Id a = Id Int deriving newtype (Eq, Hashable, Ord, Show)

newtype Url a = Url URI deriving newtype (Eq, Show)

newtype Ref = Ref Text deriving newtype (Eq, Ord, Show)

newtype Name a = Name Text deriving newtype (Eq, Show)
