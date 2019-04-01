{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Level04.Types
  ( Error (..)
  , RqType (..)
  , ContentType (..)
  , Topic
  , CommentText
  , Comment (..)
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  , fromDBComment
  , encodeComment
  , encodeTopic
  , decodeComment
  ) where

import           GHC.Generics               (Generic)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text, pack, unpack)
import           Data.List                  (stripPrefix)
import           Data.Maybe                 (fromMaybe)
import           Data.Functor.Contravariant ((>$<))
import           Data.Time                  (UTCTime)
import qualified Data.Time.Format           as TF 
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Level04.DB.Types           (DBComment, DBComment(..))
import           Level04.Types.CommentText  (CommentText, getCommentText,
                                             mkCommentText, encodeCommentText, decodeCommentText)
import           Level04.Types.Topic        (Topic, getTopic, mkTopic, encodeTopic, decodeTopic)
import           Level04.Types.Error        (Error (..))

newtype CommentId = CommentId { getId :: Int }
  deriving (Eq, Show)

encodeCommentId :: Applicative f
                => Encoder f CommentId
encodeCommentId = 
  getId >$< E.int

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentBody  :: CommentText
  , commentTime  :: UTCTime
  }
  deriving Show

encodeComment :: Applicative f => Encoder f Comment
encodeComment = E.mapLikeObj $ \c ->
    E.atKey' "commentId"  encodeCommentId (commentId c).
    E.atKey' "topic" encodeTopic (commentTopic c) .
    E.atKey' "commentText" encodeCommentText (commentBody c) .
    E.atKey' "commentTime" encodeISO8601DateTime (commentTime c)

decodeComment :: Monad f => Decoder f Comment
decodeComment = D.withCursor $ \c -> do
  o <- D.down c
  _commentId <- D.fromKey "commentId"   (CommentId <$> D.int) o 
  _topic <- D.fromKey "topic"       decodeTopic o
  _commentBody <- D.fromKey "commentText" decodeCommentText o
  _commentTime <- D.fromKey "commentTime" decodeISO8601DateTime o
  pure $ Comment _commentId _topic _commentBody _commentTime 

fromDBComment
  :: DBComment
  -> Either Error Comment
fromDBComment db = do
  topic <- mkTopic $ dbCommentTopic db
  commentText <- mkCommentText $ dbCommentComment db
  let _time = dbCommentTime db
      _id   = CommentId $ dbCommentId db
  return $ Comment _id topic commentText _time
  
data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq
  | RmRq Topic

data ContentType
  = PlainText
  | JSON

renderContentType
  :: ContentType
  -> ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON      = "application/json"

encodeISO8601DateTime :: Applicative f => Encoder f UTCTime
encodeISO8601DateTime = pack . TF.formatTime loc fmt >$< E.text
  where
    fmt = TF.iso8601DateFormat (Just "%H:%M:%S")
    loc = TF.defaultTimeLocale { TF.knownTimeZones = [] }

decodeISO8601DateTime :: Monad f => Decoder f UTCTime
decodeISO8601DateTime = D.text >>= TF.parseTimeM True loc fmt . unpack
 where
  fmt = TF.iso8601DateFormat (Just "%H:%M:%S")
  loc = TF.defaultTimeLocale { TF.knownTimeZones = [] }
  
-- | Move on to ``src/Level04/DB.hs`` next.
