{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Metrics.Health (API, getCurrentHealthStatus, HealthStatus) where

import Data.Aeson (ToJSON)
import GitHash (giBranch, giHash, giTag, tGitInfoCwd)
import Polysemy
import Relude
import Servant (Get, JSON, (:>))

type API = "health" :> Get '[JSON] HealthStatus

getCurrentHealthStatus :: Sem r HealthStatus
getCurrentHealthStatus = pure $ HealthStatus HEALTHY buildInfo

data HealthStatus = HealthStatus {status :: Status, build :: String}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

buildInfo :: String
buildInfo = giTag gitCommit <> "/" <> giBranch gitCommit <> "@" <> giHash gitCommit
  where
    gitCommit = $$tGitInfoCwd

data Status = HEALTHY
  deriving stock (Generic)
  deriving anyclass (ToJSON)
