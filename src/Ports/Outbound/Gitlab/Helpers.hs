{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ports.Outbound.Gitlab.Helpers (fetchData, fetchDataPaginated) where

import Burrito
import Config.Config (ApiToken (..), GitlabHost, UserAgent)
import Control.Exception (try)
import Core.Shared (UpdateError (..), Url (..))
import Data.Aeson hiding (Result, Value)
import Data.Either.Combinators (mapLeft)
import Metrics.Metrics (OutgoingHttpRequestsHistogram (..))
import Metrics.PrometheusUtils (VectorWithLabel (VectorWithLabel))
import Network.HTTP.Simple (Request, getResponseBody, getResponseStatus, httpLBS, parseRequest)
import Network.HTTP.Types
import Ports.Outbound.Gitlab.RequestResponseUtils
import Prometheus (observeDuration)
import Relude

fetchData :: (FromJSON a) => Url GitlabHost -> ApiToken -> UserAgent -> Template -> [(String, Value)] -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError a)
fetchData baseUrl apiToken userAgent template vars histogram = do
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    Left invalidUrl -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchData' apiToken userAgent request template histogram

fetchData' :: (FromJSON a) => ApiToken -> UserAgent -> Request -> Template -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError a)
fetchData' apiToken userAgent request template histogram = do
  measure histogram template $ do
    responseE <- try (httpLBS (setUserAgent userAgent $ setTimeout $ addToken apiToken request))
    let result = case responseE of
          Left err -> Left (HttpError err)
          Right response -> do
            let responseStatus = getResponseStatus response
            if responseStatus == status200
              then mapLeft (JSONError request response) $ eitherDecode (getResponseBody response)
              else Left (RequestFailedWithStatus request responseStatus)
    pure $ mapLeft removeApiTokenFromUpdateError result

fetchDataPaginated :: (FromJSON a) => Url GitlabHost -> ApiToken -> UserAgent -> Template -> [(String, Value)] -> OutgoingHttpRequestsHistogram -> IO (Either UpdateError [a])
fetchDataPaginated baseUrl apiToken userAgent template vars histogram = do
  try (parseRequest (show baseUrl <> "/" <> expand vars template)) >>= \case
    (Left invalidUrl) -> pure $ Left $ HttpError invalidUrl
    Right request -> fetchDataPaginated' apiToken userAgent request template histogram []

fetchDataPaginated' :: (FromJSON a) => ApiToken -> UserAgent -> Request -> Template -> OutgoingHttpRequestsHistogram -> [a] -> IO (Either UpdateError [a])
fetchDataPaginated' apiToken userAgent request template histogram acc = do
  result <- try $ do
    measure histogram template $ do
      response <- httpLBS (setUserAgent userAgent $ setTimeout $ addToken apiToken request)
      let responseStatus = getResponseStatus response
      let next = parseNextRequest response
      if responseStatus == status200
        then case eitherDecode (getResponseBody response) of
          Left err -> pure $ Left (JSONError request response err)
          Right as -> maybe (pure $ Right (as <> acc)) (\req -> fetchDataPaginated' apiToken userAgent req template histogram (as <> acc)) next
        else pure $ Left (RequestFailedWithStatus request responseStatus)
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

measure :: OutgoingHttpRequestsHistogram -> Template -> IO a -> IO a
measure (OutgoingHttpRequestsHistogram histogram) template = observeDuration (VectorWithLabel histogram ((toText . render) template))
