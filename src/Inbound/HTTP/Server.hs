{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Inbound.HTTP.Server
  ( startServer,
  )
where

import Config.Backbone (Backbone (..), GitCommit)
import Config.Config (Config (..), JobsView, UiUpdateIntervalSeconds)
import Control.Concurrent (ThreadId)
import Control.Exception (try)
import Core.BuildStatuses (BuildStatuses, BuildStatusesApi)
import Core.Effects (Health)
import Core.Runners (RunnersJobs, RunnersJobsApi)
import Core.Shared (DataUpdateIntervalSeconds)
import Data.Time
import qualified Inbound.HTTP.BuildStatuses.Html as BuildStatuses
import qualified Inbound.HTTP.Runners.Html as Runners
import Inbound.HTTP.Util
import Metrics.Health (getCurrentHealthStatus, healthToIO)
import qualified Metrics.Health as Health
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Outbound.Storage.BuildStatuses.InMemory (buildStatusesApiToIO)
import Outbound.Storage.Runners.InMemory (runnersJobsApiToIO)
import Polysemy hiding (run)
import Polysemy.Time (Time, interpretTimeGhc)
import Relude
import Servant
import System.Posix.Signals hiding (Handler)

type API =
  Health.API
    :<|> BuildStatuses.API
    :<|> Runners.API
    :<|> "static" :> Raw

api :: Proxy API
api = Proxy

server :: (Member BuildStatusesApi r, Member RunnersJobsApi r, Member (Time UTCTime d) r, Member Health r) => JobsView -> DataUpdateIntervalSeconds -> UiUpdateIntervalSeconds -> GitCommit -> ServerT API (Sem r)
server jobsView dataUpdateInterval uiUpdateInterval gitCommit =
  getCurrentHealthStatus
    :<|> (BuildStatuses.template dataUpdateInterval uiUpdateInterval gitCommit . norefreshFlag)
    :<|> (Runners.template jobsView dataUpdateInterval uiUpdateInterval gitCommit . norefreshFlag)
    :<|> serveDirectoryWebApp "/service/static"

norefreshFlag :: Bool -> AutoRefresh
norefreshFlag True = NoRefresh
norefreshFlag False = Refresh

hoist :: Config -> Backbone -> ServerT API Handler
hoist Config {..} Backbone {..} = hoistServer api (liftServer statuses runners health threads) (server jobsView dataUpdateIntervalSecs uiUpdateIntervalSecs gitCommit)

liftServer :: IORef BuildStatuses -> IORef RunnersJobs -> IORef Bool -> IORef [(ThreadId, Text)] -> Sem '[BuildStatusesApi, RunnersJobsApi, Time UTCTime Day, Health, Embed IO] a -> Handler a
liftServer statuses runners health threads sem =
  sem
    & buildStatusesApiToIO statuses
    & runnersJobsApiToIO runners
    & interpretTimeGhc
    & healthToIO health threads
    & runM
    & Handler . ExceptT . try

startServer :: Config -> Backbone -> IO ()
startServer config backbone =
  serve api (hoist config backbone)
    & prometheus def
    & gzip def
    & runSettings (setPort 8282 . setGracefulShutdownTimeout (Just 2) . setShutdownHandlerDisablingHealth (health backbone) $ defaultSettings)

setShutdownHandlerDisablingHealth :: IORef Bool -> Settings -> Settings
setShutdownHandlerDisablingHealth health = setInstallShutdownHandler shutdownHandler
  where
    shutdownHandler closeSocket = void $ installHandler sigTERM (CatchOnce $ atomicWriteIORef health False >> closeSocket) Nothing
