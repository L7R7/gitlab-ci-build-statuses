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
import Core.Effects (Health)
import Core.Lib (BuildStatuses, BuildStatusesApi, DataUpdateIntervalSeconds)
import Data.Time
import Inbound.HTTP.Html
import Metrics.Health (HealthStatus, getCurrentHealthStatus, healthToIO)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Outbound.Storage.InMemory (buildStatusesApiToIO)
import Polysemy hiding (run)
import Polysemy.Time (Time, interpretTimeGhc)
import Relude
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H

type API = "health" :> Get '[JSON] HealthStatus :<|> "statuses" :> QueryFlag "norefresh" :> Get '[HTML] H.Html :<|> "static" :> Raw

api :: Proxy API
api = Proxy

server :: (Member BuildStatusesApi r, Member (Time UTCTime d) r, Member Health r) => DataUpdateIntervalSeconds -> UiUpdateIntervalSeconds -> GitCommit -> ServerT API (Sem r)
server dataUpdateInterval uiUpdateInterval gitCommit = getCurrentHealthStatus :<|> (template dataUpdateInterval uiUpdateInterval gitCommit . norefreshFlag) :<|> serveDirectoryWebApp "/service/static"

norefreshFlag :: Bool -> AutoRefresh
norefreshFlag True = NoRefresh
norefreshFlag False = Refresh

hoist :: Config -> Backbone -> ServerT API Handler
hoist Config {..} Backbone {..} = hoistServer api (liftServer statuses threads) (server dataUpdateIntervalSecs uiUpdateIntervalSecs gitCommit)

liftServer :: IORef BuildStatuses -> IORef [(ThreadId, Text)] -> Sem '[BuildStatusesApi, Time UTCTime Day, Health, Embed IO] a -> Handler a
liftServer statuses threads sem =
  sem
    & buildStatusesApiToIO statuses
    & interpretTimeGhc
    & healthToIO threads
    & runM
    & Handler . ExceptT . try

startServer :: Config -> Backbone -> IO ()
startServer config backbone =
  serve api (hoist config backbone)
    & prometheus def
    & gzip def
    & runSettings ((setPort 8282 . setGracefulShutdownTimeout (Just 1)) defaultSettings)
