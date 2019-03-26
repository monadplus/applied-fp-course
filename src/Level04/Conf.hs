{-# LANGUAGE OverloadedStrings #-}
module Level04.Conf
    ( Conf (..)
    , firstAppConfig
    ) where

newtype Conf = Conf { dbFilePath :: FilePath }
  deriving (Eq, Show)

firstAppConfig :: Conf
firstAppConfig = Conf "app_db.db"
