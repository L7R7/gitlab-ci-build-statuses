{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Config
import           Control.Monad.IO.Class        (liftIO)
import           Data.IORef
import           Html
import           Lib
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Web.Scotty                    hiding (status)

runServer :: Config -> IORef [Result] -> IO ()
runServer (Config _ _ _ _ uiUpdateInterval) ioref =
  scotty 8080 $
  get "/" $ do
    results <- liftIO $ readIORef ioref
    html . renderHtml $ template uiUpdateInterval results
