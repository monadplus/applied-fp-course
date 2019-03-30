{-# LANGUAGE OverloadedStrings #-}
module Level04.Core
  ( runApp
  , prepareAppReqs
  , app
  , Conf (Conf)
  , DB.closeDB
  , encodeComment
  , Comment (..)
  ) where

import           Control.Applicative                (liftA2)
import           Control.Monad                      (join, mzero)

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod, responseLBS,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Network.HTTP.Types                 (Status, hContentType,
                                                     status200, status400,
                                                     status404, status500)

import qualified Data.ByteString.Lazy.Char8         as LBS

import           Data.Either                        (Either (Left, Right),
                                                     either)

import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Text.Lazy.Encoding            (encodeUtf8)

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Waargonaut.Encode                  (Encoder')
import qualified Waargonaut.Encode                  as E

import           Level04.Conf                       (Conf (Conf), firstAppConfig, dbFilePath)
import qualified Level04.DB                         as DB
import           Level04.Types                      (ContentType (JSON, PlainText),
                                                     Error (..),
                                                     RqType (AddRq, ListRq, ViewRq, RmRq),
                                                     mkCommentText, getTopic, mkTopic,
                                                     encodeComment, encodeTopic,
                                                     renderContentType, decodeComment,
                                                     Comment (..) )
import qualified Data.Bifunctor                     as Bi
import Debug.Trace

newtype StartUpError = DBInitErr SQLiteResponse
  deriving Show

runApp :: IO ()
runApp = do
  dbOrError <- prepareAppReqs firstAppConfig
  case dbOrError of 
    Left e   -> print e
    Right db -> run 3000 (app db) >> DB.closeDB db

prepareAppReqs :: Conf 
               -> IO ( Either StartUpError DB.FirstAppDB )
prepareAppReqs conf = 
  Bi.first DBInitErr <$> DB.initDB (dbFilePath conf)
  
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse sts ct =
  responseLBS sts [(hContentType, renderContentType ct)]

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

-- Some new helpers for different statuses and content types
resp500
  :: ContentType
  -> LBS.ByteString
  -> Response
resp500 =
  mkResponse status500

resp200Json
  :: Encoder' a
  -> a
  -> Response
resp200Json e =
  mkResponse status200 JSON . encodeUtf8 . E.simplePureEncodeTextNoSpaces e

app :: DB.FirstAppDB
    -> Application
app _db rq cb = do
  rq' <- mkRequest rq
  resp <- handleRespErr <$> handleRErr rq'
  cb resp
  where
    handleRespErr :: Either Error Response -> Response
    handleRespErr = either mkErrorResponse id

    handleRErr :: Either Error RqType -> IO (Either Error Response)
    handleRErr = either ( pure . Left ) ( handleRequest _db )

handleRequest
  :: DB.FirstAppDB
  -> RqType
  -> IO (Either Error Response)
handleRequest _db (AddRq topic comment) =
  (resp200 PlainText "Success" <$) <$> DB.addCommentToTopic _db topic comment
handleRequest _db (ViewRq topic) =
  (resp200Json (E.list encodeComment) <$>) <$> DB.getComments _db topic
handleRequest _db ListRq =
  (resp200Json (E.list encodeTopic) <$>) <$> DB.getTopics _db 
handleRequest _db (RmRq topic) =
  (resp200 PlainText "Removed" <$) <$> DB.deleteTopic _db topic

mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  case ( pathInfo rq, requestMethod rq ) of
    ( [t, "add"], "POST" )    -> mkAddRequest t <$> strictRequestBody rq
    ( [t, "view"], "GET" )    -> pure ( mkViewRequest t )
    ( ["list"],    "GET" )    -> pure mkListRequest
    ( [t, "rm"],   "DELETE" ) -> pure ( mkRmRequest t )
    _                         -> pure ( Left UnknownRoute )

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkRmRequest
  :: Text 
  -> Either Error RqType
mkRmRequest ti = 
  RmRq <$> mkTopic ti    

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute =
  resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 PlainText "Empty Topic"
mkErrorResponse (DBError err) =
  -- Don't leak your DB errors over the internet.
  trace (show err) (resp500 PlainText "Oh noes")