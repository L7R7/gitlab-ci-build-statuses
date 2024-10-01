{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ports.Outbound.Gitlab.Helpers (fetchData, fetchDataPaginated) where

import Burrito
import Config.Config (ApiToken (..), GitlabHost)
import Control.Exception (try)
import Core.Shared (UpdateError (..))
import Data.Aeson hiding (Result, Value)
import Data.Either.Combinators (mapLeft)
import Gitlab.Lib (Url (..))
import Metrics.Metrics (OutgoingHttpRequestsHistogram (..))
import Metrics.PrometheusUtils (VectorWithLabel (VectorWithLabel))
import Network.HTTP.Simple (Request, getResponseBody, getResponseStatus, httpLBS, parseRequest)
import Network.HTTP.Types
import Ports.Outbound.Gitlab.RequestResponseUtils
import Prometheus (observeDuration)
import Relude

fetchData :: (FromJSON a) => Url GitlabHost -> ApiToken -> Template -> [(String, Value)] -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError a)
fetchData baseUrl apiToken template vars histogram = do
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    Left invalidUrl -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchData' apiToken request template histogram

fetchData' :: (FromJSON a) => ApiToken -> Request -> Template -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError a)
fetchData' apiToken request template histogram = do
  measure histogram template $ do
    responseE <- try (httpLBS (setTimeout $ addToken apiToken request))
    let result = case responseE of
          Left err -> Left (HttpError err)
          Right response -> do
            let responseStatus = getResponseStatus response
            if responseStatus == status200
              then mapLeft (JSONError request response) $ eitherDecode (getResponseBody response)
              else Left (RequestFailedWithStatus request responseStatus)
    pure $ mapLeft removeApiTokenFromUpdateError result

fetchDataPaginated :: (FromJSON a) => Url GitlabHost -> ApiToken -> Template -> [(String, Value)] -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError [a])
fetchDataPaginated baseUrl apiToken template vars histogram = do
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    (Left invalidUrl) -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchDataPaginated' apiToken request template histogram []

fetchDataPaginated' :: (FromJSON a) => ApiToken -> Request -> Template -> OutgoingHttpRequestsHistogram -> [a] -> IO (Either UpdateError [a])
fetchDataPaginated' apiToken request template histogram acc = do
  result <- try $ do
    measure histogram template $ do
      response <- httpLBS (setTimeout $ addToken apiToken request)
      let responseStatus = getResponseStatus response
      let next = parseNextRequest response
      if responseStatus == status200
        then case eitherDecode (getResponseBody response) of
          Left err -> pure $ Left (JSONError request response err)
          Right as -> maybe (pure $ Right (as <> acc)) (\req -> fetchDataPaginated' apiToken req template histogram (as <> acc)) next
        else pure $ Left (RequestFailedWithStatus request responseStatus)
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

measure :: OutgoingHttpRequestsHistogram -> Template -> IO a -> IO a
measure (OutgoingHttpRequestsHistogram histogram) template = observeDuration (VectorWithLabel histogram ((toText . render) template))
