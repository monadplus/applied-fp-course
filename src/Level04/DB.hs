{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level04.DB
  ( FirstAppDB (FirstAppDB, dbConn)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Exception                  (bracket)
import           Control.Monad
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import Data.Time (getCurrentTime, UTCTime(..))

import           Database.SQLite.Simple             (Connection, Query (Query), NamedParam( (:=) ))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types

import           Level04.Types                      (Comment, CommentText
                                                    , Error (..), Topic
                                                    , mkTopic, getTopic, getCommentText
                                                    , fromDBComment)
import           Level04.DB.Types                   (DBComment (DBComment))
import           Data.Bifunctor                     (first)

newtype FirstAppDB = FirstAppDB { 
    dbConn :: Connection 
  }

closeDB
  :: FirstAppDB
  -> IO ()
closeDB (FirstAppDB conn) = 
  Sql.execute_ conn dropTableQ >> Sql.close conn
  where
    dropTableQ = "DROP TABLE comments"

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp =
    Sql.runDBAction $ do
    conn <- Sql.open fp
    Sql.execute_ conn createTableQ
    pure $ FirstAppDB conn
  where  
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

runDB
  :: (a -> Either Error b)
  -> IO a
  -> IO (Either Error b)
runDB f a = do
  r <- first DBError <$> Sql.runDBAction a
  pure $ f =<< r

getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments db t =
  let
    q = "SELECT id, topic, comment, time FROM comments WHERE topic = ?"
  in 
    runDB (traverse fromDBComment) $ 
      Sql.query (dbConn db) q (Sql.Only . getTopic $ t)
    

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic db topic commentText =
  let sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  in do
    time <- getCurrentTime
    runDB Right $! Sql.execute (dbConn db) sql (getTopic topic, getCommentText commentText, time)

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics db =
  let q = "SELECT DISTINCT topic FROM comments"
  in 
    runDB (traverse ( mkTopic . Sql.fromOnly )) $ Sql.query_ (dbConn db) q

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic db topic =
  let q = "DELETE FROM comments WHERE topic = ?"
  in 
    runDB Right $ Sql.execute (dbConn db) q (Sql.Only . getTopic $ topic)