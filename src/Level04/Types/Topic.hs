module Level04.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  , encodeTopic
  ) where

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Data.Functor.Contravariant (contramap)
import           Data.Text                  (Text)
import           Level04.Types.Error        (Error (EmptyTopic), nonEmptyText)
import           Database.SQLite.Simple.FromRow (FromRow, fromRow, field)

newtype Topic = Topic Text
  deriving Show

instance FromRow Topic where
  fromRow = Topic <$> field

mkTopic :: Text 
        -> Either Error Topic
mkTopic = nonEmptyText Topic EmptyTopic

getTopic :: Topic 
         -> Text
getTopic (Topic t) = t

encodeTopic :: Applicative f 
            => Encoder f Topic
encodeTopic = 
  contramap getTopic E.text