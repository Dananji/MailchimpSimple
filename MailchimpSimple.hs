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
import           Control.Exception ( catch, IOException, Exception )
import           Data.Aeson ( encode, decode, eitherDecode, Value, Array )
import           Data.List ( transpose, intercalate )
import           System.Exit ( exitWith, ExitCode(..) )
import           Data.ConfigFile ( readfile, OptionSpec, has_section, items, emptyCP )
import           Data.Either.Utils ( forceEither )
import           System.FilePath.Posix ( pathSeparator )
import qualified Data.ByteString.Lazy as BL ( ByteString )

import           Data.Aeson.Lens ( key )
import           Data.Maybe ( Maybe(..))
import           Control.Lens.Getter ( (^.))
import qualified Data.Text as T ( pack )
import qualified Data.Vector as V ( head, tail, empty )

-- App modules
import           MailchimpSimple.Types
import           MailchimpSimple.Logger

_CONFIG_FILE = "web.config"


-- | Add a new subscriber
addSubscriber
    :: String
        -> String
        -> IO (Either String SubscriptionResponse, Response BL.ByteString)
addSubscriber email emailType = do
  apiKey <- getConfig _CONFIG_FILE "api_key"
  listID <- getConfig _CONFIG_FILE "list_id"
  url <- endPointUrl
  let subscription = Subscription { s_apikey     = apiKey
                                  , s_id         = listID
                                  , s_email      = (Email email)
                                  , s_email_type = emailType
                                  , s_dou_opt    = True 
                                  , s_up_ex      = True
                                  , s_rep_int    = True
                                  , s_send       = True }
  let sUrl = url ++ "/lists/subscribe.json"
  response <- processResponse sUrl subscription
  let resBody = eitherDecode (responseBody response) :: Either String SubscriptionResponse
  return (resBody, response)
  
-- | Add a batch of subscribers
batchSubscribe 
    :: [String] 
        -> IO (BatchSubscriptionResponse, Response BL.ByteString)
batchSubscribe emails = do
  apiKey <- getConfig _CONFIG_FILE "api_key"
  listID <- getConfig _CONFIG_FILE "list_id"
  url <- endPointUrl
  let emailArry = [ Batch { b_email = (Email x), b_email_type = "html"} | x <- emails]
  let batchSubscription = BatchSubscription { b_apikey  = apiKey
                                            , b_id      = listID
                                            , b_batch   = emailArry
                                            , b_dou_opt = True
                                            , b_up_ex   = True
                                            , b_rep_int = True }
  let bUrl = url ++ "/lists/batch-subscribe.json"
  response <- processResponse bUrl batchSubscription
  let resBody = decode (responseBody response) :: Maybe Value
  let batchResponse = BatchSubscriptionResponse { add_count = resBody ^. key (T.pack "add_count") :: Maybe Int
                                                , adds = getValues (resBody ^. key (T.pack "adds") :: Maybe Array) }
  return (batchResponse, response)
  where getValues ls
          | ls /= (Just V.empty) = constructBSRes (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise = []
        constructBSRes elem = decode (encode elem) :: Maybe SubscriptionResponse
   
-- | List mailing lists in a particular account
listMailingLists :: IO [MailListResponse]
listMailingLists = do
  apiKey <- getConfig _CONFIG_FILE "api_key"
  url <- endPointUrl
  let mList =   MailList { l_apikey     = apiKey
                         , l_filters    = Filters { list_id   = ""
                                                  , list_name = "" }
	                     , l_start      = 0
	                     , l_limit      = 25
	                     , l_sort_field = "web"
	                     , l_sort_dir   = "DESC" }
  let lUrl = url ++ "/lists/list.json"
  vArray <- constructJSONResponse lUrl mList
  let listResponse = getValues vArray
  return listResponse
  where getValues ls
          | ls /= (Just V.empty) = constructMLRes (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise = []
        constructMLRes elem = do let lName = elem ^. key (T.pack "name") :: Maybe String
                                 let lID = elem ^. key (T.pack "id") :: Maybe String
                                 (MailListResponse { l_name = lName, l_id = lID})
  
-- | List subscribers in a mailing list
listSubscribers :: IO [SubscribersResponse]
listSubscribers = do
  apiKey <- getConfig _CONFIG_FILE "api_key"
  listID <- getConfig _CONFIG_FILE "list_id"
  url <- endPointUrl
  let sList = Subscribers { su_apikey = apiKey
                          , su_id     = listID
                          , su_status = "subscribed" }
  let lUrl = url ++ "/lists/members.json"
  vArray <- constructJSONResponse lUrl sList
  let listSubResponse = getValues vArray
  return listSubResponse
  where getValues ls
          | ls /= (Just V.empty) = constructMLRes (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise = []
        constructMLRes elem = do let sName = elem ^. key (T.pack "email") :: Maybe String
                                 let sListName = elem ^. key (T.pack "list_name") :: Maybe String
                                 let sEmailType = elem ^. key (T.pack "email_type") :: Maybe String
                                 (SubscribersResponse { s_name = sName, s_list_name = sListName, s_emailType = sEmailType })
                                 
constructJSONResponse url jsonData = do
  response <- processResponse url jsonData
  let resBody = decode (responseBody response) :: Maybe Value 
  let vArray = resBody ^. key (T.pack "data") :: Maybe Array
  return vArray

-- | Get the activity history on an account
getActivity = do
  apiKey <- getConfig _CONFIG_FILE "api_key"
  listID <- getConfig _CONFIG_FILE "list_id"
  url <- endPointUrl
  let activity = Activity { a_apikey = apiKey
                          , a_id     = listID }
  let aUrl = url ++ "/lists/activity.json"
  processResponse aUrl activity
  
-- | Get the created campaigns
getCampaigns :: FilePath -> IO [Campaign]
getCampaigns fileName = do
  list <- readCampaings fileName
  let trans = transpose list
  let h = head trans 
  let t = last trans
  let campaigns = [ Campaign { cid = fst tuple, title = snd tuple } | tuple <- (zip h t)]
  return campaigns
 
-- | Read the CSV file exported from the Campaigns in Mailchimp web interface
readCampaings :: FilePath -> IO [[String]]
readCampaings fileName = do
  input <- readInputFile fileName
  let values = tail $ lines input
  let finalVal = getValues $ processValues values
  return finalVal
  where processValues [] = [] 
        processValues (x:xs) = (splitString ',' x) : processValues xs
        getValues [] = []
        getValues (x:xs) = (map remove ((last x) : (take 1 x))) : getValues xs
        remove s = intercalate " " (map (filter (/='"')) (words s))
        
-- | Send an already existing campaign to a list of subscribers 
sendEmail fileName cid = do
  apiKey <- getConfig _CONFIG_FILE "api_key"
  url <- endPointUrl
  emails <- getSubscribers fileName
  let mail = SendMail { m_apikey      = apiKey
                      , m_cid         = cid
                      , m_test_emails = emails
                      , m_send_type   = "html" }
  let mUrl = url ++ "/campaigns/send-test.json"
  processResponse mUrl mail

-- | Read the CSV file exported from the Lists in Mailchimp web interface  
getSubscribers :: FilePath -> IO [String]
getSubscribers fileName = do
  input <- readInputFile fileName
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
    (\(StatusCodeException s h c) -> do let ex = (show s ++ "," ++ show h ++ "," ++ show c)
                                        getResponse s h c
                                        exitWith (ExitFailure 0))  
  
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
                          , responseCookieJar   = c }
  return errorRes
  
-- | Read the user specified configuration
getConfig :: FilePath -> OptionSpec -> IO String
getConfig fileName valueType = do           
  val <- readfile emptyCP fileName
  let cp = forceEither val
  let keyVals = if (has_section cp "USER")
                  then forceEither $ items cp "USER"
                  else forceEither $ items cp "DEFAULT"
  return $ snd $ head $ filter ((==valueType).fst) keyVals
  
-- | Construct the end-point URL
endPointUrl :: IO String
endPointUrl = do
  apikey <- getConfig _CONFIG_FILE "api_key"
  return ("https://" ++ (last (splitString '-' apikey)) ++ ".api.mailchimp.com/2.0")

-- | Utility function to split strings  
splitString :: Char -> String -> [String]
splitString d [] = []
splitString d s = x : splitString d (drop 1 y) where (x,y) = span (/= d) s
 
-- | Utility function to read external files
readInputFile :: FilePath -> IO String 
readInputFile fileName = do
  filePath <- getConfig _CONFIG_FILE "file_path"
  let file = filePath ++ [pathSeparator] ++ fileName
  catch (readFile fileName)
    (\e -> do let ex = show (e :: IOException)
              exitWith (ExitFailure 0))