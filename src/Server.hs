{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Colog
import Config
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Text as T
import Html
import Lib
import Metrics
import Network.Wai.Handler.Warp
import Network.Wai.Metrics
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H

type API = "health" :> Get '[PlainText] T.Text :<|> "metrics" :> Get '[PlainText] T.Text :<|> Get '[HTML] H.Html

api :: Proxy API
api = Proxy

server :: Config -> IORef [Result] -> Server API
server config ioref = return "UP" :<|> metrics :<|> htmlUi
  where
    metrics :: Handler T.Text
    metrics =
      liftIO $ do
        results <- readIORef ioref
        pure $ createMetrics results
    htmlUi :: Handler H.Html
    htmlUi =
      liftIO $ do
        results <- readIORef ioref
        pure $ template (uiUpdateIntervalSecs config) results

runServer :: Config -> IORef [Result] -> LoggerT Message IO ()
runServer config ioref = liftIO $ do
  ridleyWaiMetrics <- initializeMetricsMiddleware ioref
  let metricsMiddleware = maybe id metrics ridleyWaiMetrics
  run 8282 . metricsMiddleware . serve api $ server config ioref
