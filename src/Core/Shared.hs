{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Network.HTTP.Simple (HttpException, JSONException)
import Network.URI
import Relude

data Group

data UpdateError
  = HttpError HttpException
  | ConversionError JSONException
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
