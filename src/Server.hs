{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Config
import           Control.Monad.IO.Class        (liftIO)
import           Data.IORef
import           Html
import           Lib
import           Metrics
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           TextShow                      (fromText, toLazyText)
import           Web.Scotty                    hiding (status)

runServer :: Config -> IORef [Result] -> IO ()
runServer config ioref =
  scotty 8080 $ do
    get "/" $ do
      results <- liftIO $ readIORef ioref
      html . renderHtml $ template (uiUpdateIntervalSecs config) results
    get "/metrics" $ do
      results <- liftIO $ readIORef ioref
      text $ toLazyText . fromText $ createMetrics results
    get "/health" $ text "UP"
