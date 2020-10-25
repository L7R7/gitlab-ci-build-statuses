{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Outbound.Gitlab.RequestResponseUtils (privateToken, removeApiTokenFromUpdateError) where

import Control.Lens (Prism', Traversal', filtered, prism', _1, _2)
import Core.Lib (UpdateError (..))
import Network.HTTP.Conduit (HttpExceptionContent, requestHeaders)
import Network.HTTP.Simple (HttpException (HttpExceptionRequest), JSONException (..), Request, RequestHeaders)
import Network.HTTP.Types (HeaderName)
import RIO

-- TODO: 2020-08-31 can we use lenses for that (does it make sense to do that?)
removeApiTokenFromUpdateError :: UpdateError -> UpdateError
removeApiTokenFromUpdateError (HttpError httpException) = HttpError (removeApiTokenFromHttpException httpException)
removeApiTokenFromUpdateError (ConversionError jsonException) = ConversionError (removeApiTokenFromJsonException jsonException)
removeApiTokenFromUpdateError EmptyPipelinesResult = EmptyPipelinesResult
removeApiTokenFromUpdateError NoPipelineForDefaultBranch = NoPipelineForDefaultBranch

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

removeApiTokenFromJsonException :: JSONException -> JSONException
removeApiTokenFromJsonException (JSONParseException request response parseError) = JSONParseException (removeApiTokenFromRequest request) response parseError
removeApiTokenFromJsonException (JSONConversionException request response s) = JSONConversionException (removeApiTokenFromRequest request) response s

removeApiTokenFromRequest :: Request -> Request
removeApiTokenFromRequest = set (headers . tokenHeader) "xxxxx"
