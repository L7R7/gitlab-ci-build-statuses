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
import Config.Config (Config (..), UiUpdateIntervalSeconds)
import Control.Concurrent (ThreadId)
import Control.Exception (try)
import Core.BuildStatuses (BuildStatuses, BuildStatusesApi, DataUpdateIntervalSeconds)
import Core.Effects (Health)
import Data.Time
import Inbound.HTTP.BuildStatuses.Html (AutoRefresh (..))
import qualified Inbound.HTTP.BuildStatuses.Html as BuildStatuses
import Metrics.Health (getCurrentHealthStatus, healthToIO)
import qualified Metrics.Health as Health
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Outbound.Storage.BuildStatuses.InMemory (buildStatusesApiToIO)
import Polysemy hiding (run)
import Polysemy.Time (Time, interpretTimeGhc)
import Relude
import Servant
import System.Posix.Signals hiding (Handler)

type API =
  Health.API
    :<|> BuildStatuses.API
    :<|> "static" :> Raw

api :: Proxy API
api = Proxy

server :: (Member BuildStatusesApi r, Member (Time UTCTime d) r, Member Health r) => DataUpdateIntervalSeconds -> UiUpdateIntervalSeconds -> GitCommit -> ServerT API (Sem r)
server dataUpdateInterval uiUpdateInterval gitCommit =
  getCurrentHealthStatus
    :<|> (BuildStatuses.template dataUpdateInterval uiUpdateInterval gitCommit . norefreshFlag)
    :<|> serveDirectoryWebApp "/service/static"

norefreshFlag :: Bool -> AutoRefresh
norefreshFlag True = NoRefresh
norefreshFlag False = Refresh

hoist :: Config -> Backbone -> ServerT API Handler
hoist Config {..} Backbone {..} = hoistServer api (liftServer statuses health threads) (server dataUpdateIntervalSecs uiUpdateIntervalSecs gitCommit)

liftServer :: IORef BuildStatuses -> IORef Bool -> IORef [(ThreadId, Text)] -> Sem '[BuildStatusesApi, Time UTCTime Day, Health, Embed IO] a -> Handler a
liftServer statuses health threads sem =
  sem
    & buildStatusesApiToIO statuses
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
