{-# LANGUAGE OverloadedStrings #-}

module MailchimpSimple
( addSubscriber
, batchSubscribe
, listMailingLists
, listSubscribers ) where

import           Network.HTTP.Conduit ( parseUrl, RequestBody (RequestBodyLBS), requestBody, method, withManager, httpLbs, Response (..)
                                      , HttpException (..), Cookie(..))
import           Network.HTTP.Types ( methodPost, Status(..), http11 )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Exception ( catch, IOException, Exception )
import           Data.Aeson ( encode, decode, eitherDecode, Value, Array )
import           Data.List ( transpose, intercalate )
import           System.Exit ( exitWith, ExitCode(..) )
import           System.FilePath.Posix ( pathSeparator )
import qualified Data.ByteString.Lazy as BL ( ByteString )

import           Data.Aeson.Lens ( key )
import           Data.Maybe ( Maybe(..), fromJust )
import           Control.Lens.Getter ( (^.))
import qualified Data.Text as T ( pack )
import qualified Data.Vector as V ( head, tail, empty )

-- App modules
import           MailchimpSimple.Types
-- import           MailchimpSimple.Logger

_API_KEY = "sample_apikey"
_LIST_ID = "sample_listid"

-- | Add a new subscriber
addSubscriber
    :: String
        -> String
        -> String
        -> String
        -> IO (Either String SubscriptionResponse, Response BL.ByteString)
addSubscriber apiKey listID email emailType = do
  url <- endPointUrl apiKey
  let subscription = Subscription { s_apikey     = apiKey
                                  , s_id         = listID
                                  , s_email      = (Email email)
                                  , s_email_type = emailType
                                  , s_dou_opt    = True 
                                  , s_up_ex      = True
                                  , s_rep_int    = True
                                  , s_send       = True }
  let sUrl    = url ++ "/lists/subscribe.json"
  response    <- processResponse sUrl subscription apiKey
  let resBody = eitherDecode (responseBody response) :: Either String SubscriptionResponse
  return (resBody, response)
  
-- | Add a batch of subscribers
batchSubscribe 
    :: String
        -> String
        -> [String]
        -> IO (BatchSubscriptionResponse, Response BL.ByteString)
batchSubscribe apiKey listID emails = do
  url <- endPointUrl apiKey
  let emailArry = [ Batch { b_email = (Email x), b_email_type = "html"} | x <- emails]
  let batchSubscription = BatchSubscription { b_apikey  = apiKey
                                            , b_id      = listID
                                            , b_batch   = emailArry
                                            , b_dou_opt = True
                                            , b_up_ex   = True
                                            , b_rep_int = True }
  let bUrl          = url ++ "/lists/batch-subscribe.json"
  response          <- processResponse bUrl batchSubscription apiKey
  let resBody       = decode (responseBody response) :: Maybe Value
  let batchResponse = BatchSubscriptionResponse { add_count = fromJust (resBody ^. key (T.pack "add_count") :: Maybe Int)
                                                , adds      = getValues (resBody ^. key (T.pack "adds") :: Maybe Array) }
  return (batchResponse, response)
  where getValues ls
          | ls /= (Just V.empty) = constructBSRes (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise            = []
        constructBSRes elem      = fromJust (decode (encode elem) :: Maybe SubscriptionResponse)
   
-- | List mailing lists in a particular account
listMailingLists 
    :: String 
        -> IO ([MailListResponse], Response BL.ByteString)
listMailingLists apiKey = do
  url <- endPointUrl apiKey
  let mList =   MailList { l_apikey     = apiKey
                         , l_filters    = Filters { list_id   = ""
                                                  , list_name = "" }
	                     , l_start      = 0
	                     , l_limit      = 25
	                     , l_sort_field = "web"
	                     , l_sort_dir   = "DESC" }
  let lUrl         = url ++ "/lists/list.json"
  response         <- processResponse lUrl mList apiKey
  let resBody      = decode (responseBody response) :: Maybe Value 
  let vArray       = resBody ^. key (T.pack "data") :: Maybe Array
  let listResponse = getValues vArray
  return (listResponse, response)
  where getValues ls
          | ls /= (Just V.empty) = constructMLRes (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise            = []
        constructMLRes elem = do let lName = fromJust (elem ^. key (T.pack "name") :: Maybe String)
                                 let lID   = fromJust (elem ^. key (T.pack "id") :: Maybe String)
                                 MailListResponse { l_name = lName, l_id = lID}
  
-- | List subscribers in a mailing list
listSubscribers 
    :: String
        -> String
        -> IO ([ListSubscribersResponse], Response BL.ByteString)
listSubscribers apiKey listID = do
  url <- endPointUrl apiKey
  let sList = Subscribers { su_apikey = apiKey
                          , su_id     = listID
                          , su_status = "subscribed" }
  let lUrl = url ++ "/lists/members.json"
  response <- processResponse lUrl sList apiKey
  let resBody = decode (responseBody response) :: Maybe Value 
  let vArray = resBody ^. key (T.pack "data") :: Maybe Array
  let listSubResponse = getValues vArray
  return (listSubResponse, response)
  where getValues ls
          | ls /= (Just V.empty) = constructMLRes (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise = []
        constructMLRes elem = do let sName      = fromJust (elem ^. key (T.pack "email") :: Maybe String)
                                 let sEuid      = fromJust (elem ^. key (T.pack "euid") :: Maybe String)
                                 let sListName  = fromJust (elem ^. key (T.pack "list_name") :: Maybe String)
                                 let sEmailType = fromJust (elem ^. key (T.pack "email_type") :: Maybe String)
                                 (ListSubscribersResponse { s_name = sName
                                                          , s_euid = sEuid
                                                          , s_list_name = sListName
                                                          , s_emailType = sEmailType })

-------------------------------------------------------------------------------------------------------------------------------------

-- | Build the response from URL and JSON data 
processResponse url jsonData apiKey = do
  initReq <- liftIO $ parseUrl url
  let req = initReq { requestBody = RequestBodyLBS $ encode jsonData
                    , method      = methodPost }
  catch (withManager $ httpLbs req)
    (\(StatusCodeException s h c) -> do let ex = (show s ++ "," ++ show h ++ "," ++ show c)
                                        getResponse s h c apiKey
                                        exitWith (ExitFailure 0))  
  
-- | Construct the erroneous HTTP responses when an exception occurs
getResponse s h c apiKey = do
  url      <- endPointUrl apiKey
  initReq  <- parseUrl url
  let req  = initReq { method = methodPost }
  response <- withManager $ httpLbs req
  let errorRes = response { responseStatus      = s
                          , responseVersion     = http11
                          , responseBody        = ""
                          , responseHeaders     = h
                          , responseCookieJar   = c }
  return errorRes

-- | Construct the end-point URL
endPointUrl :: String -> IO String
endPointUrl apiKey = return ("https://" ++ (last (splitString '-' apiKey)) ++ ".api.mailchimp.com/2.0")

-- | Utility function to split strings  
splitString :: Char -> String -> [String]
splitString d [] = []
splitString d s  = x : splitString d (drop 1 y) where (x,y) = span (/= d) s