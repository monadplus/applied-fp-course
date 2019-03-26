{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level04.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query), Only (Only))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level04.Types                      (Comment, CommentText,
                                                     Error, Topic, getTopic)

newtype FirstAppDB = FirstAppDB { 
    dbConn :: Connection 
  }

closeDB
  :: FirstAppDB
  -> IO ()
closeDB (FirstAppDB conn) = 
  Sql.execute_ conn dropTableQ >> Sql.close conn
  where
    dropTableQ = "DROP TABLE comments" :: Query

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = 
  fmap FirstAppDB <$> Sql.runDBAction action
  where
    action = do
      conn <- Sql.open fp
      Sql.execute_ conn createTableQ
      return conn
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- There are several possible implementations of this function. Particularly
  -- there may be a trade-off between deciding to throw an Error if a DBComment
  -- cannot be converted to a Comment, or simply ignoring any DBComment that is
  -- not valid.
getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
-- 
-- 
getComments (FirstAppDB conn) t =
  let
    topic = getTopic t
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- 
  in do
    xs <- Sql.query conn sql (Only topic)
    (\(id, t, c, time) -> DBComment id t c time) <$> xs
    -- Aixo retornara Either parsingError DbComment
    -- Retornar Either Error [Comment] amb forM 


addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  in
    error "addCommentToTopic not implemented"

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics =
  let
    sql = "SELECT DISTINCT topic FROM comments"
  in
    error "getTopics not implemented"

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
  in
    error "deleteTopic not implemented"
