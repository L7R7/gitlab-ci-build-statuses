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

module Metrics.Health (API, getCurrentHealthStatus, initThreads, initHealth, healthToIO, HealthStatus) where

import Control.Concurrent (ThreadId)
import Control.Exception (throw)
import Core.Effects (Health (IsHealthy), isHealthy)
import Data.Aeson (ToJSON, encode)
import GHC.Conc (ThreadStatus (ThreadDied, ThreadFinished), threadStatus)
import GitHash (GitInfo, giBranch, giHash, giTag, tGitInfoCwd)
import Network.HTTP.Types (hContentType)
import Polysemy
import Relude
import Servant (Get, JSON, err503, errHeaders, (:>))
import Servant.Server (errBody)

type API = "health" :> Get '[JSON] HealthStatus

getCurrentHealthStatus :: Member Health r => Sem r HealthStatus
getCurrentHealthStatus = ifM isHealthy (pure healthy) (throw errorResponse)
  where
    errorResponse =
      err503
        { errBody = encode unhealthy,
          errHeaders = [(hContentType, "application/json;charset=utf-8")]
        }

healthToIO :: Member (Embed IO) r => IORef Bool -> IORef [(ThreadId, Text)] -> InterpreterFor Health r
healthToIO healthIORef threadsIORef = interpret $ \case
  IsHealthy -> embed $ do
    threads <- readIORef threadsIORef
    health <- readIORef healthIORef
    (&&) health <$> allM (isThreadHealthy . fst) threads

isThreadHealthy :: ThreadId -> IO Bool
isThreadHealthy = fmap (`notElem` [ThreadFinished, ThreadDied]) . threadStatus

initThreads :: IO (IORef [(ThreadId, Text)])
initThreads = newIORef []

initHealth :: IO (IORef Bool)
initHealth = newIORef True

data HealthStatus = HealthStatus {status :: Status, build :: String}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

healthy :: HealthStatus
healthy = HealthStatus HEALTHY buildInfo

unhealthy :: HealthStatus
unhealthy = HealthStatus UNHEALTHY buildInfo

buildInfo :: String
buildInfo = giTag gitCommit <> "/" <> giBranch gitCommit <> "@" <> giHash gitCommit

gitCommit :: GitInfo
gitCommit = $$tGitInfoCwd

data Status = HEALTHY | UNHEALTHY
  deriving stock (Generic)
  deriving anyclass (ToJSON)
