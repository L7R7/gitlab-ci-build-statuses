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
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H

type API = "health" :> Get '[PlainText] T.Text :<|> Get '[HTML] H.Html

api :: Proxy API
api = Proxy

server :: Config -> IORef [Result] -> Server API
server config ioref = return "UP" :<|> htmlUi
  where
    htmlUi :: Handler H.Html
    htmlUi = liftIO $ template (uiUpdateIntervalSecs config) <$> readIORef ioref

runServer :: Config -> IORef [Result] -> LoggerT Message IO ()
runServer config ioref = liftIO $ run 8282 . serve api $ server config ioref
