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

type Health = "health" :> Get '[ PlainText] T.Text

type Metrics = "metrics" :> Get '[ PlainText] T.Text

type Ui = "ui" :> Get '[ HTML] H.Html

type API = Health :<|> Metrics :<|> Ui

api :: Proxy API
api = Proxy

runServer :: Config -> IORef [Result] -> IO ()
runServer config ioref = run 8080 . serve api $ return "health" :<|> liftIO mx :<|> liftIO htmlUi
  where
    mx :: IO T.Text
    mx = do
      results <- readIORef ioref
      pure $ createMetrics results
    htmlUi = do
      results <- readIORef ioref
      pure $ template (uiUpdateIntervalSecs config) results
