{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server where

import Control.Monad.Except (ExceptT (..))
import qualified Data.Text as T
import Env
import Html
import Katip
import Network.Wai.Handler.Warp
import RIO hiding (Handler)
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H

type API = "health" :> Get '[PlainText] T.Text :<|> Get '[HTML] H.Html

api :: Proxy API
api = Proxy

server :: (HasConfig env, HasStatuses env) => ServerT API (RIO env)
server = liftIO (return "UP") :<|> template

startServer :: (HasConfig env, HasStatuses env, KatipContext (RIO env)) => RIO env ()
startServer = do
  srvr <- buildServer
  liftIO $ run 8282 srvr

buildServer :: (HasConfig env, HasStatuses env, KatipContext (RIO env)) => RIO env Application
buildServer = do
  env <- ask
  pure . serve api . hoist $ env

hoist :: forall env. (HasConfig env, HasStatuses env) => env -> Server API
hoist env = hoistServer api nat server
  where
    nat :: RIO env a -> Servant.Handler a
    nat act = Servant.Handler $ ExceptT $ try $ runRIO env act
