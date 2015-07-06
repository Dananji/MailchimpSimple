{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module MailchimpSimple 
( addSubscriber
, listSubscribers
, listMailingLists
, getHistory) where

import           Data.Aeson
import           GHC.Generics hiding ( head )
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Conduit ( parseUrl, RequestBody (RequestBodyLBS), requestBody, method, withManager, httpLbs, Response (..)
                                      , HttpException (..), Cookie(..))
import           Network.HTTP.Types ( methodPost, Status(..), http11 )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Exception as E ( catch )

_API_KEY = "sample_apikey-datacenter"

addSubscriber    = getActivities "lists" "subscribe"
listSubscribers  = getActivities "lists" "members"
listMailingLists = getActivities "lists" "list"
getHistory       = getActivities "lists" "activity"

-- | Reading the JSON file from the local machine
getJSON jsonFile = BL.readFile jsonFile

endPointUrl :: IO String
endPointUrl = return ("https://" ++ (last (splitString '-' _API_KEY)) ++ ".api.mailchimp.com/2.0/")

splitString :: Char -> String -> [String]
splitString d [] = []
splitString d s = x : splitString d (drop 1 y) where (x,y) = span (/= d) s

getActivities :: String -> String -> IO (Response BL.ByteString)
getActivities section method = do
  url <- endPointUrl
  activity <- getJSON (method ++ ".json")
  let aURL = url ++ ("/" ++ section ++ "/" ++ method ++ ".json")
  initReq <- liftIO $ parseUrl $ aURL
  let req = initReq { requestBody = RequestBodyLBS activity
                    , method = methodPost }
  E.catch (withManager $ httpLbs req)
    (\(StatusCodeException s h c) -> (getResponse s h c))
  
getResponse s h c = do
  initReq <- parseUrl "http://www.example.com"
  let req = initReq { method = methodPost }
  response <- withManager $ httpLbs req
  let errorRes = response { responseStatus = s
                     , responseVersion = http11
					 , responseBody = ""
   	                 , responseHeaders = h
                     , responseCookieJar =  c }
  return errorRes