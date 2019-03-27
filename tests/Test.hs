{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Wai     (assertBody, assertStatus'
                                    , buildRequest, buildRequestWithBody
                                    , get, post, testWai
                                    , assertContentType)
import qualified Test.Tasty.HUnit   as HU
import           Network.HTTP.Types as HTTP
import           Network.HTTP.Types.Method (StdMethod(..))
import           Network.Wai.Test   (SResponse(..), request, srequest)         
import qualified Level04.Core       as Core
import           Data.ByteString.Lazy.Char8 as LBS


main :: IO ()
main = do
    dbOrError <- Core.prepareAppReqs $ Core.Conf ":memory:"
    case dbOrError of 
      Left _ -> HU.assertFailure "db error"
      Right _db -> test _db
  where
    test _db = 
      let app = Core.app _db

        --   assertComments :: SResponse 
        --                  -> Session ()
        --   assertComments SResponse{simpleBody = body} =
        --     undefined
--             assertFailure :: String -> Session ()
-- assertFailure msg = msg `deepseq` liftIO (throwIO (WaiTestFailure msg))
      in defaultMain $ testGroup "Applied FP Course - Tests"        
        [ 
            testGroup "Status 2XX" 
               [
                  testWai app "Add comment" $ do
                    resp <- post "fudge/add" "sweet"
                    assertStatus' HTTP.status200 resp
                    assertBody "Success" resp
                
                , testWai app "View topic comments" $ do
                    resp <- get "fudge/view"
                    assertStatus' HTTP.status200 resp
                    assertContentType "application/json" resp
                
                , testWai app "List topics" $ do
                    resp <- get "list"
                    assertStatus' HTTP.status200 resp
                    assertContentType "application/json" resp

                , testWai app "Remove topic" $ do
                    resp <- request $ buildRequest DELETE "fudge/rm"
                    assertStatus' HTTP.status200 resp
                    assertBody "Removed" resp
               ],
        
            testGroup "Status 4XX-5XX"
               [
                  testWai app "Not defined route" $ do
                    resp <- get "fudge/all"
                    assertStatus' HTTP.status404 resp
                    assertBody "Unknown Route" resp  
            
                , testWai app "Empty comment" $ do
                    resp <- post "fudge/add" ""
                    assertStatus' HTTP.status400 resp
                    assertBody "Empty Comment" resp
                
                , testWai app "Empty topic" $ do
                    resp <- post "//add" ""
                    assertStatus' HTTP.status400 resp
                    assertBody "Empty Topic" resp
               ]
        ]
