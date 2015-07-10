{-# LANGUAGE OverloadedStrings #-}

module MailchimpSimple
( addSubscriber
, batchSubscribe
, listSubscribers
, listMailingLists
, getActivity ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLS ( unpack )
import qualified Data.ByteString.Char8 as BS ( unpack )
import           Network.HTTP.Conduit ( parseUrl, RequestBody (RequestBodyLBS), requestBody, method, withManager, httpLbs, Response (..)
                                      , HttpException (..), Cookie(..))
import           Network.HTTP.Types ( methodPost, Status(..), http11 )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Exception ( catch, IOException )
import           Data.Aeson ( encode, decode )
import           Data.List

-- App module
import           Types

_API_KEY = "sample_apikey"
_LIST_ID = "sample_listid"

-- | Add new subscriber
addSubscriber email emailType = do
  url <- endPointUrl
  let subscription = Subscription { s_apikey     = _API_KEY
                                  , s_id         = _LIST_ID
                                  , s_email      = (Email email)
                                  , s_email_type = emailType
                                  , s_dou_opt    = True 
                                  , s_up_ex      = True
                                  , s_rep_int    = True
                                  , s_send       = True }
  let sUrl = url ++ "/lists/subscribe.json"
  processResponse sUrl subscription

-- | Add a batch of subscribers
batchSubscribe fileName = do
  url <- endPointUrl
  emails <- constructBatch fileName
  let batchSubscription = BatchSubscription { b_apikey  = _API_KEY
                                            , b_id      = _LIST_ID
                                            , b_batch   = emails
                                            , b_dou_opt = True
                                            , b_up_ex   = True
                                            , b_rep_int = True
                                            }
  let bUrl = url ++ "lists/batch-subscribe.json"
  processResponse bUrl batchSubscription
  
-- | List mailing lists in a particular account
listMailingLists = do
  url <- endPointUrl
  let mList =   MailList { l_apikey     = _API_KEY
                         , l_filters    = Filters { list_id   = ""
                                                  , list_name = "" }
	                     , l_start      = 0
	                     , l_limit      = 25
	                     , l_sort_field = "web"
	                     , l_sort_dir   = "DESC" }
  let lUrl = url ++ "/lists/list.json"
  processResponse lUrl mList

-- | List subscribers in a mailing list
listSubscribers = do
  url <- endPointUrl
  let sList = Subscribers { su_apikey = _API_KEY
                          , su_id     = _LIST_ID
                          , su_status = "subscribed" }
  let lUrl = url ++ "/lists/members.json"
  processResponse lUrl sList
	
-- | Get the activity history on an account
getActivity = do
  url <- endPointUrl
  let activity = Activity { a_apikey = _API_KEY
                          , a_id     = _LIST_ID }
  let aUrl = url ++ "/lists/activity.json"
  processResponse aUrl activity
  
-- | Get the created campaigns
getCampaigns :: FilePath -> IO [Campaign]
getCampaigns fileName = do
  list <- readCampaignsCSV fileName
  let trans = transpose list
  let h = head trans
  let t = last trans
  return [ Campaign { cid = fst tuple, title = snd tuple } | tuple <- (zip h t)]
  
-- | Read the CSV file exported from the Campaigns in Mailchimp web interface
readCampaignsCSV :: FilePath -> IO [[String]]
readCampaignsCSV fileName = do
  input <- readFile fileName
  let values = tail $ lines input
  let finalVal = getValues $ processValues values
  return finalVal
  where processValues [] = [] 
        processValues (x:xs) = (splitString ',' x) : processValues xs
        getValues [] = []
        getValues (x:xs) = (map remove ((last x) : (take 1 x))) : getValues xs
        remove s = intercalate " " (map (filter (/='"')) (words s))
  
sendEmail fileName cid = do
  url <- endPointUrl
  emails <- readSubscribersCSV fileName
  let mail = SendMail { m_apikey      = _API_KEY
                      , m_cid         = cid
                      , m_test_emails = emails
                      , m_send_type   = "html" }
  let mUrl = url ++ "campaigns/send-test.json"
  processResponse mUrl mail                
       
-- | Read the CSV exported from Mailchimp web interface        
readSubscribersCSV :: FilePath -> IO [String]
readSubscribersCSV fileName = do
  input <- readFile fileName
  let values = tail $ lines input
  let processed = concat $ processValues values
  return processed
  where processValues [] = [] 
        processValues (x:xs) = (take 1 (splitString ',' x)) : processValues xs
  
-- | Build the response from URL and JSON data
processResponse url jsonData = do
  initReq <- liftIO $ parseUrl url
  let req = initReq { requestBody = RequestBodyLBS $ encode jsonData
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
                     , responseVersion     = http11
                     , responseBody        = ""
                     , responseHeaders     = h
                     , responseCookieJar   =  c }
  return errorRes

-- | Construction of array of Batch data read from an external text file
constructBatch :: FilePath -> IO [Batch]
constructBatch fileName = do
  input <- readFile fileName
  let emails = splitString ',' input
  let emailArry = [ Batch { b_email = (Email x), b_email_type = "html"} | x <- emails]
  return emailArry
  
-- | Construct the end-point URL
endPointUrl :: IO String
endPointUrl = return ("https://" ++ (last (splitString '-' _API_KEY)) ++ ".api.mailchimp.com/2.0/")

-- | Utility function to split strings  
splitString :: Char -> String -> [String]
splitString d [] = []
splitString d s = x : splitString d (drop 1 y) where (x,y) = span (/= d) s

-- | Utility function to convert Lazy.ByteString to Strict ByteString
convertToStrict :: BL.ByteString -> B.ByteString
convertToStrict = B.concat . BL.toChunks