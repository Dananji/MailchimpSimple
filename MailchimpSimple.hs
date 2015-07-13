{-# LANGUAGE OverloadedStrings #-}

module MailchimpSimple
( addSubscriber
, batchSubscribe
, listMailingLists
, listSubscribers
, getActivity 
, sendEmail ) where

import           Network.HTTP.Conduit ( parseUrl, RequestBody (RequestBodyLBS), requestBody, method, withManager, httpLbs, Response (..)
                                      , HttpException (..), Cookie(..))
import           Network.HTTP.Types ( methodPost, Status(..), http11 )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Exception ( catch, IOException )
import           Data.Aeson ( encode, decode )
import           Data.List
import           System.Exit

-- App module
import           Types
import           Logger

_API_KEY = "sample_apikey"
_LIST_ID = "sample_list_id"

-- | Add a new subscriber
addSubscriber email emailType = do
  writeLog INFO "addSubscriber" (email ++ "," ++ emailType) "Entry"
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
  writeLog INFO "addSubscriber" (sUrl ++ "," ++ show subscription) "OK"
  
-- | Add a batch of subscribers
batchSubscribe fileName = do
  writeLog INFO "batchSubscribe" fileName "Entry"
  url <- endPointUrl
  emails <- getEmailAddresses fileName
  let emailArry = [ Batch { b_email = (Email x), b_email_type = "html"} | x <- emails]
  let batchSubscription = BatchSubscription { b_apikey  = _API_KEY
                                            , b_id      = _LIST_ID
                                            , b_batch   = emailArry
                                            , b_dou_opt = True
                                            , b_up_ex   = True
                                            , b_rep_int = True
                                            }
  let bUrl = url ++ "lists/batch-subscribe.json"
  processResponse bUrl batchSubscription
  writeLog INFO "batchSubscribe" (bUrl ++ "," ++ show batchSubscription) "OK"
  where getEmailAddresses fileName = do
                     input <- readInputFile fileName
                     let emails = splitString ',' input
                     return emails
  
-- | List mailing lists in a particular account
listMailingLists = do
  writeLog INFO "listMailingLists" _API_KEY "Entry"
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
  writeLog INFO "listMailingLists" (lUrl ++ "," ++ show mList) "OK"

-- | List subscribers in a mailing list
listSubscribers = do
  writeLog INFO "listSubscribers" (_API_KEY ++ "," ++ _LIST_ID) "Entry"
  url <- endPointUrl
  let sList = Subscribers { su_apikey = _API_KEY
                          , su_id     = _LIST_ID
                          , su_status = "subscribed" }
  let lUrl = url ++ "/lists/members.json"
  processResponse lUrl sList
  writeLog INFO "listSubscribers" (lUrl ++ "," ++ show sList) "OK"
	
-- | Get the activity history on an account
getActivity = do
  writeLog INFO "getActivity" (_API_KEY ++ "," ++ _LIST_ID) "Entry"
  url <- endPointUrl
  let activity = Activity { a_apikey = _API_KEY
                          , a_id     = _LIST_ID }
  let aUrl = url ++ "/lists/activity.json"
  processResponse aUrl activity
  writeLog INFO "getActivity" (aUrl ++ "," ++ show activity) "OK"
  
-- | Get the created campaigns
getCampaigns :: FilePath -> IO [Campaign]
getCampaigns fileName = do
  writeLog INFO "getCampaigns" fileName "Entry"
  list <- readCampaings fileName
  let trans = transpose list
  let h = head trans 
  let t = last trans
  let campaigns = [ Campaign { cid = fst tuple, title = snd tuple } | tuple <- (zip h t)]
  writeLog INFO "getCampaigns" (show campaigns) "OK"  
  return campaigns
 
-- | Read the CSV file exported from the Campaigns in Mailchimp web interface
readCampaings :: FilePath -> IO [[String]]
readCampaings fileName = do
  writeLog INFO "readCampaings" fileName "Entry"
  input <- readInputFile fileName
  let values = tail $ lines input
  let finalVal = getValues $ processValues values
  writeLog INFO "readCampaings" (show finalVal) "OK"
  return finalVal
  where processValues [] = [] 
        processValues (x:xs) = (splitString ',' x) : processValues xs
        getValues [] = []
        getValues (x:xs) = (map remove ((last x) : (take 1 x))) : getValues xs
        remove s = intercalate " " (map (filter (/='"')) (words s))
        
-- | Send an already existing campaign to a list of subscribers 
sendEmail fileName cid = do
  writeLog INFO "sendEmail" (fileName ++ "," ++ cid) "Entry"
  url <- endPointUrl
  emails <- getSubscribers fileName
  let mail = SendMail { m_apikey      = _API_KEY
                      , m_cid         = cid
                      , m_test_emails = emails
                      , m_send_type   = "html" }
  let mUrl = url ++ "campaigns/send-test.json"
  processResponse mUrl mail
  writeLog INFO "sendEmail" (mUrl ++ "," ++ show mail) "OK"

-- | Read the CSV file exported from the Lists in Mailchimp web interface  
getSubscribers :: FilePath -> IO [String]
getSubscribers fileName = do
  writeLog INFO "getSubscribers" fileName "Entry"
  input <- readInputFile fileName
  let values = tail $ lines input
  let processed = concat $ processValues values
  writeLog INFO "getSubscribers" (show processed) "OK"
  return processed
  where processValues [] = [] 
        processValues (x:xs) = (take 1 (splitString ',' x)) : processValues xs
        
-- | Build the response from URL and JSON data        
processResponse url jsonData = do
  writeLog INFO "processResponse" (url ++ "," ++ show jsonData) "Entry"
  initReq <- liftIO $ parseUrl url
  let req = initReq { requestBody = RequestBodyLBS $ encode jsonData
                    , method = methodPost }
  catch (withManager $ httpLbs req)
    (\(StatusCodeException s h c) -> do let ex = (show s ++ "," ++ show h ++ "," ++ show c)
                                        writeLog ERROR "processResponse" (url ++ "," ++ show jsonData) ("HttpException= " ++ ex)
                                        getResponse s h c
                                        writeLog ERROR "MailchimpSimple" (url ++ "," ++ show jsonData) "Exit"
                                        exitWith (ExitFailure 0))  
  
-- | Construct the erroneous HTTP responses when an exception occurs
getResponse s h c = do
  writeLog INFO "getResponse" (show s ++ "," ++ show h ++ "," ++ show c) "Entry"
  url <- endPointUrl
  initReq <- parseUrl url
  let req = initReq { method = methodPost }
  response <- withManager $ httpLbs req
  let errorRes = response { responseStatus = s
                     , responseVersion     = http11
                     , responseBody        = ""
                     , responseHeaders     = h
                     , responseCookieJar   = c }
  writeLog INFO "getResponse" (show errorRes) "OK"
  return errorRes
  
-- | Construct the end-point URL
endPointUrl :: IO String
endPointUrl = return ("https://" ++ (last (splitString '-' _API_KEY)) ++ ".api.mailchimp.com/2.0/")

-- | Utility function to split strings  
splitString :: Char -> String -> [String]
splitString d [] = []
splitString d s = x : splitString d (drop 1 y) where (x,y) = span (/= d) s
 
-- | Utility function to read external files
readInputFile :: FilePath -> IO String 
readInputFile fileName = catch (readFile fileName)
                        (\e -> do let ex = show (e :: IOException)
                                  writeLog ERROR "readInputFile" fileName ("IOException= " ++ ex) 
                                  writeLog ERROR "MailchimpSimple" fileName "Exit"
                                  exitWith (ExitFailure 0))