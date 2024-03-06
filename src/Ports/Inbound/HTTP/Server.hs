{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Ports.Inbound.HTTP.Server
  ( startServer,
  )
where

import Config.Backbone (Backbone (..), GitCommit)
import Config.Config (Config (..), JobsView, UiUpdateIntervalSeconds)
import Control.Exception (try)
import Core.BuildStatuses (BuildStatuses, BuildStatusesApi)
import Core.Runners (RunnersJobs, RunnersJobsApi)
import Core.Shared (DataUpdateIntervalSeconds)
import Data.Time
import Metrics.Health (getCurrentHealthStatus)
import Metrics.Health qualified as Health
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Polysemy hiding (run)
import Polysemy.Reader qualified as R
import Polysemy.Time (Time, interpretTimeGhc)
import Ports.Inbound.HTTP.BuildStatuses.Html qualified as BuildStatuses
import Ports.Inbound.HTTP.Runners.Html qualified as Runners
import Ports.Inbound.HTTP.Util
import Ports.Outbound.Storage.BuildStatuses.InMemory (buildStatusesApiToIO)
import Ports.Outbound.Storage.Runners.InMemory (runnersJobsApiToIO)
import Relude
import Servant
import System.Posix.Signals hiding (Handler)

type API = "builds" :> API' :<|> API'

type API' =
  Health.API
    :<|> BuildStatuses.API
    :<|> Runners.API
    :<|> "static"
      :> Raw

api :: Proxy API
api = Proxy

server :: (Member BuildStatusesApi r, Member RunnersJobsApi r, Member (Time UTCTime d) r, Member (R.Reader DataUpdateIntervalSeconds) r, Member (R.Reader UiUpdateIntervalSeconds) r, Member (R.Reader GitCommit) r, Member (R.Reader JobsView) r) => ServerT API (Sem r)
server = s :<|> s
  where
    s =
      getCurrentHealthStatus
        :<|> (\mvm b mfm -> BuildStatuses.template (fromMaybe Plain mvm) (fromMaybe ShowAll mfm) (norefreshFlag b))
        :<|> (Runners.template . norefreshFlag)
        :<|> serveDirectoryWebApp "/service/static"

norefreshFlag :: Bool -> AutoRefresh
norefreshFlag True = NoRefresh
norefreshFlag False = Refresh

hoist :: Config -> Backbone -> ServerT API Handler
hoist config backbone = hoistServer api (liftServer config backbone) server

liftServer :: Config -> Backbone -> Sem '[BuildStatusesApi, RunnersJobsApi, Time UTCTime Day, R.Reader DataUpdateIntervalSeconds, R.Reader UiUpdateIntervalSeconds, R.Reader GitCommit, R.Reader JobsView, R.Reader (IORef BuildStatuses), R.Reader (IORef RunnersJobs), Embed IO] a -> Handler a
liftServer Config {..} Backbone {..} sem =
  sem
    & buildStatusesApiToIO
    & runnersJobsApiToIO
    & interpretTimeGhc
    & R.runReader dataUpdateIntervalSecs
    & R.runReader uiUpdateIntervalSecs
    & R.runReader gitCommit
    & R.runReader jobsView
    & R.runReader statuses
    & R.runReader runners
    & runM
    & Handler
    . ExceptT
    . try

startServer :: Config -> Backbone -> IO ()
startServer config backbone =
  serve api (hoist config backbone)
    & prometheus def
    & gzip def
    & runSettings (setPort 8282 . setGracefulShutdownTimeout (Just 2) . setShutdownHandler $ defaultSettings)

setShutdownHandler :: Settings -> Settings
setShutdownHandler = setInstallShutdownHandler shutdownHandler
  where
    shutdownHandler closeSocket = void $ installHandler sigTERM (CatchOnce closeSocket) Nothing
