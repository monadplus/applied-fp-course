{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level06.Conf
    ( parseOptions
    ) where

import           GHC.Word                 (Word16)
import           Data.Bifunctor           (first)
import           Data.Monoid              ((<>), Last (Last, getLast))

import           Level06.AppM             (AppM(..))
import           Level06.Types            (Conf (Conf), ConfigError (..),
                                           DBFilePath (DBFilePath), PartialConf(..),
                                           Port (Port))

import           Level06.Conf.CommandLine (commandLineParser)
import           Level06.Conf.File        (parseJSONConfigFile)

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf =
  PartialConf {
    pcPort = Last (Just $ Port 3000),
    pcDBFilePath = Last (Just $ DBFilePath ":memory:")
  }

-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig pc = Conf
  <$> lastToEither MissingPort pcPort
  <*> lastToEither MissingDBFilePath pcDBFilePath
  where
    lastToEither
      :: ConfigError 
      -> (PartialConf -> Last a)
      -> Either ConfigError a
    lastToEither err f = 
      maybe (Left err) Right (getLast $ f pc)   

-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> AppM ConfigError Conf
parseOptions fp =
  let mkCfg cli file = makeConfig (defaultConf <> file <> cli)
  in AppM $ do
  cli' <- commandLineParser
  (mkCfg cli' =<<) <$> parseJSONConfigFile fp
