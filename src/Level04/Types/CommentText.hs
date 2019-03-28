module Level04.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  , encodeCommentText
  , decodeCommentText
  ) where

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Level04.Types.Error        (Error (EmptyCommentText),
                                             nonEmptyText)
import           Data.Functor.Contravariant (contramap)
import           Data.Text                  (Text)
import           Database.SQLite.Simple.FromRow (FromRow, fromRow, field)

newtype CommentText = CommentText Text
  deriving Show

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText =
  nonEmptyText CommentText EmptyCommentText

getCommentText
  :: CommentText
  -> Text
getCommentText (CommentText t) = t

encodeCommentText :: Applicative f 
                  => Encoder f CommentText
encodeCommentText = 
  contramap getCommentText E.text

decodeCommentText :: Monad f 
                  => Decoder f CommentText
decodeCommentText = 
  fmap CommentText D.text
