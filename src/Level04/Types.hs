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
  ) where

import           GHC.Generics               (Generic)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text, pack)
import           Data.List                  (stripPrefix)
import           Data.Maybe                 (fromMaybe)
import           Data.Functor.Contravariant ((>$<))
import           Data.Time                  (UTCTime)
import qualified Data.Time.Format           as TF 
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Level04.DB.Types           (DBComment, DBComment(..))
import           Level04.Types.CommentText  (CommentText, getCommentText,
                                             mkCommentText, encodeCommentText)
import           Level04.Types.Topic        (Topic, getTopic, mkTopic, encodeTopic)
import           Level04.Types.Error        (Error (EmptyCommentText, EmptyTopic, UnknownRoute))

newtype CommentId = CommentId { getId :: Int }
  deriving (Eq, Show)

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentBody  :: CommentText
  , commentTime  :: UTCTime
  }
  deriving Show

encodeComment :: Applicative f => Encoder f Comment
encodeComment = E.mapLikeObj $ \c ->
    E.intAt "commentId" (getId $ commentId c) .
    E.textAt "topic" (getTopic $ commentTopic c) .
    E.textAt "commentText" (getCommentText $ commentBody c) .
    E.atKey' "commentTime" encodeISO8601DateTime (commentTime c)

-- | For safety we take our stored `DBComment` and try to construct a `Comment`
-- that we would be okay with showing someone. However unlikely it may be, this
-- is a nice method for separating out the back and front end of a web app and
-- providing greater guarantees about data cleanliness.
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
