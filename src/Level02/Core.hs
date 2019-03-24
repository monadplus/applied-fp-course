{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response, ResponseReceived,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody, lazyRequestBody)

import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           Level02.Types            (ContentType (..), Error (..), RqType (..),
                                           mkCommentText, mkTopic,
                                           renderContentType, getTopic, getCommentText)

-- |-------------------------------------------|
-- |- Helpers  -|
-- |-------------------------------------------|

-- This is a helper function to assist us in going 
-- from a Lazy ByteString, to a Strict Text
lazyByteStringToStrictText :: LBS.ByteString -> Text 
lazyByteStringToStrictText = decodeUtf8 . LBS.toStrict

textToLazyByteString :: Text -> LBS.ByteString
textToLazyByteString = LBS.fromStrict . encodeUtf8

mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status cType content =
  let contentHeader = (hContentType, renderContentType cType)
  in responseLBS status [contentHeader] content

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t c = do
  topic   <- mkTopic t
  comment <- mkCommentText . lazyByteStringToStrictText $ c
  return $ AddRq topic comment 

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest t =
  fmap ViewRq (mkTopic t)

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse EmptyTopicName = 
  resp404 PlainText "Topic not found"
mkErrorResponse EmptyComment = 
  resp400 PlainText "Invalid comment: empty"
mkErrorResponse (InvalidMethod method) = 
  resp400 PlainText (LBS.fromStrict $ "Invalid method: " <> method)
mkErrorResponse InvalidRoute = 
  resp400 PlainText "Invalid route"

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest req = do 
  body <- bodyIO
  return $ 
    case method of 
      "POST"  -> case path of 
        [t, "add"] -> mkAddRequest t body 
        _ -> invalidRoute
      "GET"   -> case path of 
        [t, "view"] -> mkViewRequest t
        ["list"]    -> mkListRequest
        _           -> invalidRoute
      invalid -> invalidMethod invalid
  where
    bodyIO        = lazyRequestBody req
    method        = requestMethod req
    path          = pathInfo req
    invalidRoute  = Left InvalidRoute 
    invalidMethod = Left . InvalidMethod

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq topic comment) =
  let content = "Add comment: " <> getCommentText comment <> " to topic " 
                  <> getTopic topic <> " not implemented!"
  in Right $ resp200 PlainText $ textToLazyByteString content
handleRequest (ViewRq topic) =
  let content = "View " <> getTopic topic <> " not implemented!"
  in Right $ resp200 PlainText $ textToLazyByteString content
handleRequest ListRq =
  Right $ resp200 PlainText "Not implemented!"
  

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
--  type Application = ...
app :: Request
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
app rq respond = do
  rqType <- mkRequest rq 
  let responseOrError = rqType >>= handleRequest
  respond (either mkErrorResponse id responseOrError)

runApp :: IO ()
runApp = run 3000 app
