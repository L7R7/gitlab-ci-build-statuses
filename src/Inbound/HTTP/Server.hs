{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Inbound.HTTP.Server
  ( startServer,
  )
where

import Config (Config (..), GitCommit, UiUpdateIntervalSeconds)
import Control.Monad.Except (ExceptT (..))
import Core.Effects (Timer)
import Core.Lib (BuildStatuses, BuildStatusesApi)
import qualified Data.Text as T
import Inbound.HTTP.Html
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Outbound.Storage.InMemory (buildStatusesApiToIO)
import Polysemy hiding (run)
import RIO hiding (Handler, logInfo)
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import Util (timerToIO)

type API = "health" :> Get '[PlainText] T.Text :<|> "statuses" :> Get '[HTML] H.Html :<|> "static" :> Raw

api :: Proxy API
api = Proxy

server :: (Member BuildStatusesApi r, Member Timer r) => UiUpdateIntervalSeconds -> GitCommit -> ServerT API (Sem r)
server uiUpdateInterval gitCommit = return "UP" :<|> template uiUpdateInterval gitCommit :<|> serveDirectoryWebApp "/service/static"

hoist :: Config -> ServerT API Handler
hoist Config {..} = do
  hoistServer api (liftServer statuses) (server uiUpdateIntervalSecs gitCommit)

liftServer :: IORef BuildStatuses -> Sem '[BuildStatusesApi, Timer, Embed IO] a -> Handler a
liftServer statuses sem =
  sem
    & buildStatusesApiToIO statuses
    & timerToIO
    & runM
    & Handler . ExceptT . try

startServer :: Config -> IO ()
startServer config =
  serve api (hoist config)
    & prometheus def
    & run 8282
