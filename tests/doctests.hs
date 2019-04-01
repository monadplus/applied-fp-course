module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest
  [ 
    "src/Level06/Conf/File.hs"
  ]
