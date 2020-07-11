{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Inbound.HTTP.Server
  ( startServer,
  )
where

import Control.Monad.Except (ExceptT (..))
import Core.Lib (HasBuildStatuses)
import qualified Data.Text as T
import Env
import Inbound.HTTP.Html
import Katip
import Network.Wai.Handler.Warp
import Network.Wai.Metrics
import RIO hiding (Handler)
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H

type API = "health" :> Get '[PlainText] T.Text :<|> "statuses" :> Get '[HTML] H.Html

api :: Proxy API
api = Proxy

server :: (HasConfig env, HasBuildStatuses env) => ServerT API (RIO env)
server = liftIO (return "UP") :<|> template

startServer :: (HasConfig env, HasBuildStatuses env, HasStore env, KatipContext (RIO env)) => RIO env ()
startServer = do
  env <- ask
  store <- view storeL
  waiMetrics <- liftIO $ registerWaiMetrics store
  let middleware = metrics waiMetrics
  logLocM InfoS "Starting server"
  liftIO $ run 8282 $ middleware . serve api . hoist $ env

hoist :: forall env. (HasConfig env, HasBuildStatuses env) => env -> Server API
hoist env = hoistServer api nat server
  where
    nat :: RIO env a -> Servant.Handler a
    nat act = Servant.Handler $ ExceptT $ try $ runRIO env act
