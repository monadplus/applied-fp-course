{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM, liftEither)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  con <- Sql.open fp
  Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB f a = do
  r <- liftIO $ first DBError <$> Sql.runDBAction a
  liftEither $ r >>= f

getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments db ti =
  let q = "SELECT id, topic, comment, time FROM comments WHERE topic = ?"
  in
    runDB (traverse fromDBComment) $ 
      Sql.query (dbConn db) q (Sql.Only . getTopic $ ti)

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic db ti c =
  let q = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  in do
    now <- liftIO getCurrentTime
    runDB Right $ Sql.execute (dbConn db) q (getTopic ti, getCommentText c, now)

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics db =
  let q = "SELECT DISTINCT topic FROM comments"
  in 
    runDB (traverse (mkTopic . Sql.fromOnly)) $
      Sql.query_ (dbConn db) q

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic db ti =
  let q = "DELETE FROM comments WHERE topic = ?"
  in
    runDB Right $ Sql.execute (dbConn db) q (Sql.Only . getTopic $ ti)
  

-- Go to 'src/Level05/Core.hs' next.
