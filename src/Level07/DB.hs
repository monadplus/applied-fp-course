{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                      (App, Env (envConfig, envDB), liftEither)

import           Level07.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Conf (port, dbFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: App Connection
getDBConn =
  (liftIO . Sql.open) =<< a
  where 
    a :: App FilePath
    a = asks (getDBFilePath . dbFilePath . envConfig)

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB h f = do
  conn <- getDBConn
  ea <- liftIO $ first DBError <$> (Sql.runDBAction . f $ conn)
  liftEither $ h =<< ea

getComments
  :: Topic
  -> App [Comment]
getComments topic =
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
      query conn = Sql.query conn q (Sql.Only . getTopic $ topic)
  in runDB (traverse fromDBComment) query

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic topic comment = do
  nowish <- liftIO getCurrentTime
  let q = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
      query conn = Sql.execute conn q (getTopic topic, getCommentText comment, nowish)
  runDB Right query

getTopics
  :: App [Topic]
getTopics =
  let q = "SELECT DISTINCT topic FROM comments"
      query conn = Sql.query_ conn q
  in runDB (traverse ( mkTopic . Sql.fromOnly )) query
  
deleteTopic
  :: Topic
  -> App ()
deleteTopic topic =
  let q = "DELETE FROM comments WHERE topic = ?"
      query conn = Sql.execute conn q (Sql.Only . getTopic $ topic)
  in runDB Right query 

-- Go on to 'src/Level07/Core.hs' next.
