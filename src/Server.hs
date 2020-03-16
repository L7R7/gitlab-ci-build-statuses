{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           Config
import           Control.Monad.IO.Class   (liftIO)
import           Data.IORef
import qualified Data.Text                as T
import           Html
import           Lib
import           Metrics
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Blaze
import qualified Text.Blaze.Html5         as H

type API = "health" :> Get '[ PlainText] T.Text :<|> "metrics" :> Get '[ PlainText] T.Text :<|> Get '[ HTML] H.Html

api :: Proxy API
api = Proxy

server :: Config -> IORef [Result] -> Server API
server config ioref = return "UP" :<|> liftIO metrics :<|> liftIO htmlUi
  where
    metrics :: IO T.Text
    metrics = do
      results <- readIORef ioref
      pure $ createMetrics results
    htmlUi :: IO H.Html
    htmlUi = do
      results <- readIORef ioref
      pure $ template (uiUpdateIntervalSecs config) results

runServer :: Config -> IORef [Result] -> IO ()
runServer config ioref = run 8282 . serve api $ server config ioref
