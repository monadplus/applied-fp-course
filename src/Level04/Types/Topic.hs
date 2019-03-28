module Level04.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  , encodeTopic
  , decodeTopic
  ) where

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Data.Functor.Contravariant (contramap)
import           Data.Text                  (Text)
import           Level04.Types.Error        (Error (EmptyTopic), nonEmptyText)
import           Database.SQLite.Simple.FromRow (FromRow, fromRow, field)

newtype Topic = Topic Text
  deriving Show

mkTopic :: Text -> Either Error Topic
mkTopic = nonEmptyText Topic EmptyTopic

getTopic :: Topic -> Text
getTopic (Topic t) = t

encodeTopic :: Applicative f => Encoder f Topic
encodeTopic = contramap getTopic E.text

-- TODO: should fail parsing   
decodeTopic :: Monad f => Decoder f Topic
decodeTopic = fmap Topic D.text
