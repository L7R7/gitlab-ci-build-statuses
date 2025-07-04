{-# LANGUAGE OverloadedStrings #-}

module Ports.Outbound.Gitlab.RequestResponseUtils (privateToken, removeApiTokenFromUpdateError, parseNextRequest, addToken, setTimeout, setUserAgent) where

import Config.Config
import Control.Lens (Lens', Prism', Traversal', filtered, lens, prism', set, _1, _2)
import Core.Shared (UpdateError (..))
import Network.HTTP.Client.Conduit
import Network.HTTP.Link
import Network.HTTP.Simple
import Network.HTTP.Types (HeaderName)
import Network.URI
import Relude

parseNextRequest :: Response a -> Maybe Request
parseNextRequest response = parseNextHeader response >>= requestFromURI

parseNextHeader :: Response a -> Maybe URI
parseNextHeader response = href <$> find isNextLink (getResponseHeader "link" response >>= concat . parseLinkHeaderBS)

isNextLink :: Link uri -> Bool
isNextLink (Link _ [(Rel, "next")]) = True
isNextLink _ = False

setUserAgent :: UserAgent -> Request -> Request
setUserAgent (UserAgent userAgent) = setRequestHeader (fromString userAgent) ["Backbone"]

addToken :: ApiToken -> Request -> Request
addToken (ApiToken token) = setRequestHeader privateToken [token]

setTimeout :: Request -> Request
setTimeout request = request {responseTimeout = responseTimeoutMicro 5000000}

-- TODO: 2020-08-31 can we use lenses for that (does it make sense to do that?)
removeApiTokenFromUpdateError :: UpdateError -> UpdateError
removeApiTokenFromUpdateError (HttpError httpException) = HttpError (removeApiTokenFromHttpException httpException)
removeApiTokenFromUpdateError EmptyResult = EmptyResult
removeApiTokenFromUpdateError (RequestFailedWithStatus request s) = RequestFailedWithStatus (removeApiTokenFromRequest request) s
removeApiTokenFromUpdateError (JSONError request response s) = JSONError (removeApiTokenFromRequest request) response s

removeApiTokenFromHttpException :: HttpException -> HttpException
removeApiTokenFromHttpException = set (reqPrism . _1 . headers . tokenHeader) "xxxxx"

reqPrism :: Prism' HttpException (Request, HttpExceptionContent)
reqPrism = prism' (uncurry HttpExceptionRequest) extract
  where
    extract (HttpExceptionRequest request reason) = Just (request, reason)
    extract _ = Nothing

tokenHeader :: Traversal' RequestHeaders ByteString
tokenHeader = traverse . filtered (\header -> fst header == privateToken) . _2

privateToken :: HeaderName
privateToken = "PRIVATE-TOKEN"

headers :: Lens' Request RequestHeaders
headers = lens getter setter
  where
    getter = requestHeaders
    setter r h = r {requestHeaders = h}

removeApiTokenFromRequest :: Request -> Request
removeApiTokenFromRequest = set (headers . tokenHeader) "xxxxx"
