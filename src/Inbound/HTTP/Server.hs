{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Inbound.HTTP.Server
  ( startServer,
  )
where

import Config (Config (..), GitCommit, UiUpdateIntervalSeconds)
import Control.Concurrent (ThreadId)
import Control.Exception (try)
import Core.Effects (Health, Timer)
import Core.Lib (BuildStatuses, BuildStatusesApi)
import Inbound.HTTP.Html
import Metrics.Health (HealthStatus, getCurrentHealthStatus, healthToIO)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Outbound.Storage.InMemory (buildStatusesApiToIO)
import Polysemy hiding (run)
import Relude
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import Util (timerToIO)

type API = "health" :> Get '[JSON] HealthStatus :<|> "statuses" :> QueryFlag "norefresh" :> Get '[HTML] H.Html :<|> "static" :> Raw

api :: Proxy API
api = Proxy

server :: (Member BuildStatusesApi r, Member Timer r, Member Health r) => UiUpdateIntervalSeconds -> GitCommit -> ServerT API (Sem r)
server uiUpdateInterval gitCommit = getCurrentHealthStatus :<|> (template uiUpdateInterval gitCommit . norefreshFlag) :<|> serveDirectoryWebApp "/service/static"

norefreshFlag :: Bool -> AutoRefresh
norefreshFlag True = NoRefresh
norefreshFlag False = Refresh

hoist :: Config -> ServerT API Handler
hoist Config {..} = hoistServer api (liftServer statuses threads) (server uiUpdateIntervalSecs gitCommit)

liftServer :: IORef BuildStatuses -> IORef [(ThreadId, Text)] -> Sem '[BuildStatusesApi, Timer, Health, Embed IO] a -> Handler a
liftServer statuses threads sem =
  sem
    & buildStatusesApiToIO statuses
    & timerToIO
    & healthToIO threads
    & runM
    & Handler . ExceptT . try

startServer :: Config -> IO ()
startServer config =
  serve api (hoist config)
    & prometheus def
    & gzip def
    & run 8282
