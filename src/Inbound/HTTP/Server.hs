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
import qualified Polysemy.Reader as R
import Polysemy.Time (Time, interpretTimeGhc)
import Relude
import Servant
import System.Posix.Signals hiding (Handler)

type API = "builds" :> API' :<|> API'

type API' =
  Health.API
    :<|> BuildStatuses.API
    :<|> Runners.API
    :<|> "static" :> Raw

api :: Proxy API
api = Proxy

server :: (Member BuildStatusesApi r, Member RunnersJobsApi r, Member (Time UTCTime d) r, Member Health r, Member (R.Reader DataUpdateIntervalSeconds) r, Member (R.Reader UiUpdateIntervalSeconds) r, Member (R.Reader GitCommit) r, Member (R.Reader JobsView) r) => ServerT API (Sem r)
server = s :<|> s
  where
    s =
      getCurrentHealthStatus
        :<|> (BuildStatuses.template . norefreshFlag)
        :<|> (Runners.template . norefreshFlag)
        :<|> serveDirectoryWebApp "/service/static"

norefreshFlag :: Bool -> AutoRefresh
norefreshFlag True = NoRefresh
norefreshFlag False = Refresh

hoist :: Config -> Backbone -> ServerT API Handler
hoist config backbone = hoistServer api (liftServer config backbone) server

liftServer :: Config -> Backbone -> Sem '[BuildStatusesApi, RunnersJobsApi, Time UTCTime Day, Health, R.Reader DataUpdateIntervalSeconds, R.Reader UiUpdateIntervalSeconds, R.Reader GitCommit, R.Reader JobsView, R.Reader (IORef BuildStatuses), R.Reader (IORef RunnersJobs), Embed IO] a -> Handler a
liftServer Config {..} Backbone {..} sem =
  sem
    & buildStatusesApiToIO
    & runnersJobsApiToIO
    & interpretTimeGhc
    & healthToIO health threads
    & R.runReader dataUpdateIntervalSecs
    & R.runReader uiUpdateIntervalSecs
    & R.runReader gitCommit
    & R.runReader jobsView
    & R.runReader statuses
    & R.runReader runners
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
