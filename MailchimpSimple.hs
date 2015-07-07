{-# LANGUAGE OverloadedStrings #-}

module MailchimpSimple 
( addSubscriber
, listSubscribers
, listMailingLists
, getHistory ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLS ( unpack )
import qualified Data.ByteString.Char8 as BS ( unpack )
import           Network.HTTP.Conduit ( parseUrl, RequestBody (RequestBodyLBS), requestBody, method, withManager, httpLbs, Response (..)
                                      , HttpException (..), Cookie(..))
import           Network.HTTP.Types ( methodPost, Status(..), http11 )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Exception ( catch, IOException )


_API_KEY = "sample_apikey-datacenter"

-- | Add new subscriber
addSubscriber    = getActivities "lists" "subscribe"
  
-- | List the subscribers in a particular list
listSubscribers  = getActivities "lists" "members"
  
-- | List mailing lists in a particular account
listMailingLists = getActivities "lists" "list"
  
-- | Get activity history of a particular mailing list
getHistory       = getActivities "lists" "activity"
  
-- | Reading the JSON file from the local machine
getJSON jsonFile = catch (BL.readFile jsonFile)
                    (\e -> do let ex = show (e :: IOException)
                              return BL.empty)

-- | Construct the end-point URL
endPointUrl :: IO String
endPointUrl = return ("https://" ++ (last (splitString '-' _API_KEY)) ++ ".api.mailchimp.com/2.0/")

-- | Utility function to split strings
splitString :: Char -> String -> [String]
splitString d [] = []
splitString d s = x : splitString d (drop 1 y) where (x,y) = span (/= d) s

-- | General method to retrieve HTTP responses
getActivities :: String -> String -> IO (Response BL.ByteString)
getActivities section method = do
  url <- endPointUrl
  let fileName = (method ++ ".json") 
  activity <- getJSON fileName
  let aURL = url ++ ("/" ++ section ++ "/" ++ method ++ ".json") 
  initReq <- liftIO $ parseUrl $ aURL
  let req = initReq { requestBody = RequestBodyLBS activity
                    , method = methodPost }
  catch (withManager $ httpLbs req)
    (\(StatusCodeException s h c) -> getResponse s h c)
  
-- | Construct the erroneous HTTP responses when an exception occurs
getResponse s h c = do
  url <- endPointUrl
  initReq <- parseUrl url
  let req = initReq { method = methodPost }
  response <- withManager $ httpLbs req
  let errorRes = response { responseStatus = s
                     , responseVersion = http11
                     , responseBody = ""
                     , responseHeaders = h
                     , responseCookieJar =  c }
  return errorRes