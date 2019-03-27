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

import           Control.Exception                  (bracket)
import           Control.Monad
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query), Only (Only), NamedParam( (:=) ))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types

import           Level04.Types                      (Comment, CommentText
                                                    , Error (..), Topic
                                                    , getTopic, getCommentText
                                                    , fromDBComment)
import           Level04.DB.Types                   (DBComment (DBComment))

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
  Sql.runDBAction $ onFinalize (FirstAppDB <$> action)
  where
    action = do
      conn <- Sql.open fp
      Sql.execute_ conn createTableQ
      return conn

    onFinalize ioa = 
      bracket ioa closeDB pure
      
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments (FirstAppDB conn) topic =
  let
    -- TODO: this is failing and I have no idea why ...
    -- sql = "SELECT id, topic, comment, time FROM comments WHERE topic = pato"
    sql = "SELECT topic, comment FROM comments"
  in do
    xs <- Sql.query_ conn sql {- (Only $ getTopic topic) -}{-  :: IO [Int] -}
    forM_  xs $ \(ti, comment) -> putStrLn $ (ti :: String) <> (comment :: String)
    -- pure $ traverse fromDBComment xs
    pure $ Right []

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic (FirstAppDB conn) topic commentText =
  let sql = "INSERT INTO comments (topic,comment,time) VALUES (:topic, :comment, :time)"
  in 
    handleErrors $ do
      time <- getCurrentTime
      Sql.executeNamed conn sql [ ":topic" := getTopic topic
                                  , ":comment" := getCommentText commentText
                                  , ":time" := time]

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics (FirstAppDB conn) =
  let
    sql = "SELECT DISTINCT topic FROM comments"
  in
    handleErrors (Sql.query_ conn sql :: IO [Topic])

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic (FirstAppDB conn) topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
  in
    handleErrors $ Sql.execute conn sql (Only $ getTopic topic)

handleErrors :: IO a -> IO (Either Error a)
handleErrors action = fmap f (Sql.runDBAction action)
  where
    f (Right a) = f (Right a)
    f (Left (SQLConstraintError _ reason)) = Left $ SQLError reason
    f (Left (SQLFormatError err)) = Left $ SQLError (Text.pack $ Sql.fmtMessage err)
    f (Left (SQLResultError err)) = Left $ SQLError "SQLResultError"
    f (Left (SQLOtherError err)) = Left $ SQLError (Sql.sqlErrorDetails err)