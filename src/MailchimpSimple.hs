{-# LANGUAGE OverloadedStrings #-}

module MailchimpSimple
( listMailingLists
, listSubscribers
, getTemplates
, getCampaigns
, createCampaign
, sendEmail
, addSubscriber
, removeSubscriber
, batchSubscribe ) where

import           Network.HTTP.Conduit
import           Network.HTTP.Types ( methodPost, methodGet, methodDelete, Status(..), http11, ResponseHeaders, hContentType )
import           Control.Monad.IO.Class ( liftIO )
import           Network.HTTP.Client.Auth

import           Safe
import           Control.Exception ( catch, IOException, Exception )
import           Control.Lens.Getter ( (^.))
import           System.Exit ( exitWith, ExitCode(..) )
import           System.FilePath.Posix ( pathSeparator )

import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy ( ByteString )
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T

import           Data.Aeson ( encode, decode, eitherDecode, Value, Array, ToJSON )
import           Data.List ( transpose, intercalate )
import           Data.Aeson.Lens ( key )

import           Data.Maybe ( Maybe(..), fromJust )
import qualified Data.Vector as V
import           Crypto.Hash.MD5 as MD5


-- App modules
import           Utils.Types

-- | List mailing lists in a particular account
listMailingLists 
    :: String 
        -> IO [MailListResponse]
listMailingLists apiKey = do
  let url = endPointUrl apiKey
  let lUrl         = url ++ "/lists?fields=lists.id,lists.name"
  response         <- processGET lUrl apiKey
  let resBody      = decode (responseBody response) :: Maybe Value
  let vArray       = resBody ^. key "lists" :: Maybe Array
  let listResponse = getValues vArray
  return listResponse
  where getValues ls
          | ls /= (Just V.empty) = constructMLRes (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise            = []
        constructMLRes elem = do let lName = elem ^. key "name" :: Maybe String
                                 let lID   = elem ^. key "id" :: Maybe String
                                 MailListResponse lName lID

-- | List subscribers in a mailing list
listSubscribers 
    :: String
        -> String
        -> IO [ListSubscribersResponse]
listSubscribers apiKey listName = do
  let url = endPointUrl apiKey
  listid <- getListID apiKey listName
  let lUrl = url ++ "/lists/" ++ listid ++ "/members?fields=members.email_address,members.unique_email_id,members.email_type,members.list_id,members.status"
  response <- processGET lUrl apiKey
  let resBody = decode (responseBody response) :: Maybe Value 
  let vArray = resBody ^. key "members" :: Maybe Array
  let listSubResponse = getValues vArray
  return listSubResponse
  where getValues ls
          | ls /= (Just V.empty) = constructMLRes (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise = []
        constructMLRes elem = ListSubscribersResponse sName sEuid (Just listName) sEmailType sStatus
                              where sName      = elem ^. key "email_address" :: Maybe String
                                    sEuid      = elem ^. key "unique_email_id" :: Maybe String
                                    sEmailType = elem ^. key "email_type" :: Maybe String
                                    sStatus    = elem ^. key "status" :: Maybe String

getTemplates
    :: String
        -> IO [TemplateResponse]
getTemplates apiKey = do
  let url = endPointUrl apiKey
  let tUrl         = url ++ "/templates?fields=templates.id,templates.name"
  response         <- processGET tUrl apiKey
  let resBody      = decode (responseBody response) :: Maybe Value
  let galleryT     = resBody ^. key "templates"  :: Maybe Array
  let templateList = getValues galleryT
  return templateList
  where getValues ls
            | ls /= (Just V.empty) = constructTRes (fmap V.head ls) : getValues (fmap V.tail ls)
            | otherwise = []
        constructTRes elem = do let tName = elem ^. key "name" :: Maybe String
                                let tID   = elem ^. key "id" :: Maybe Int
                                TemplateResponse tName tID

getCampaigns :: String -> IO [(Maybe String, Maybe String)]
getCampaigns apiKey = do
  let url = endPointUrl apiKey
  let cUrl = url ++ "/campaigns?fields=campaigns.id,campaigns.settings"
  response <- processGET cUrl apiKey
  let resBody = decode (responseBody response) :: Maybe Value
  let rawcids = resBody ^. key "campaigns" :: Maybe Array
  let cids = getValues rawcids
  return cids
  where getValues ls
          | ls /= (Just V.empty) = constructCRes (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise = []
        constructCRes elem = do let cid = elem ^. key "id" :: Maybe String
                                let settings = elem ^. key "settings" :: Maybe Value
                                let name = settings ^. key "subject_line" :: Maybe String
                                (cid, name)

-- | Create a new campaign and save it 
createCampaign
    :: String
        -> String
        -> String
        -> String
        -> String
        -> String
        -> String
        -> IO (Maybe String)
createCampaign apiKey
               listName
               replyTo
               fromName
               cType
               title
               subject = do
  let url = endPointUrl apiKey
  listid <- getListID apiKey listName
  let campaign = Campaign { c_type       = cType
                          , c_settings    = Settings { s_subject   = subject
                                                     , s_title     = title
                                                     , s_from_name = fromName
                                                     , s_reply_to  = replyTo }
                          , c_receipients = (ListID listid) }
  let eUrl    = url ++ "/campaigns"
  response    <- processPOST eUrl campaign apiKey
  let resBody = decode (responseBody response) :: Maybe Value
  let campaignid = resBody ^. key "id" :: Maybe String
  return campaignid

-- | Send an email campaign
sendEmail
 :: String
    -> String
    -> IO (Either String SendMailResponse)  
sendEmail apiKey cid = do
  let url = endPointUrl apiKey
  let sUrl = url ++ "/campaigns/" ++ cid ++ "/actions/send"
  response <- processEmptyPOST sUrl apiKey
  let sendRes = eitherDecode (responseBody response) :: Either String SendMailResponse
  return sendRes

-- | Add a new member to a given list
addSubscriber
    :: String
        -> String
        -> String
        -> String
        -> String
        -> IO SubscriptionResponse
addSubscriber apiKey listName email emailType status = do
  let url = endPointUrl apiKey
  let subscription = Subscription { s_email      = email
                                  , s_email_type = emailType
                                  , s_status     = status }
  listid <- getListID apiKey listName
  let sUrl = url ++ "/lists/" ++ listid ++ "/members"
  response <- processPOST sUrl subscription apiKey
  let resBody = decode (responseBody response) :: Maybe Value
  let subscribers = constructSRes resBody
  return subscribers
  where constructSRes elem = do let email = elem ^. key "email_address"  :: Maybe String
                                let euid = elem ^. key "unique_email_id" :: Maybe String
                                let status = elem ^. key "status" :: Maybe String
                                SubscriptionResponse email euid status (Just listName)
        filterListID list = filter ((==(Just listName)) . l_name) list

-- Remove a member from a given list
removeSubscriber
    :: String 
        -> String 
        -> String 
        -> IO Bool
removeSubscriber apiKey email listName = do
  let url = endPointUrl apiKey
  listid <- getListID apiKey listName
  let subhash = createHash email
  let rUrl = url ++ "/lists/" ++ listid ++ "/members/" ++ subhash
  response <- processDELETE rUrl apiKey
  let resBody = statusCode $ responseStatus response
  case resBody of
    204 -> return True
    _ -> return False
  where createHash str = (B8.unpack (calculateHash (strToBS str)))
        calculateHash str = (B16.encode (hash str))
        strToBS str = B8.pack str
        
-- | Add a batch of subscribers
batchSubscribe 
    :: String
        -> String
        -> [(String, String)]
        -> IO BatchSubscriptionResponse
batchSubscribe apiKey listName subs = do
  let url = endPointUrl apiKey
  listid <- getListID apiKey listName
  let batchSubs = map constructSubs subs
  let batchOps = map (constructOps listid) batchSubs
  let batchSubscription = Batch { operations = batchOps }
  let bUrl = url ++ "/batches"
  response <- processPOST bUrl batchSubscription apiKey
  let resBody = decode (responseBody response) :: Maybe Value
  let batchResponse = BatchSubscriptionResponse (resBody ^. key "id" :: Maybe String) (resBody ^. key "status" :: Maybe String)
  return batchResponse
  where constructSubs (email, status) = Subscription { s_email      = email
                                                     , s_email_type = "html"
                                                     , s_status     = status }
        constructOps listid sub = Operation { o_method = "POST"
                                            , o_path = "/lists/" ++ listid ++ "/members"
                                            , o_params = Params { params = [] }
                                            , o_body = B8.unpack $ BL.toStrict $ encode sub }
   

-------------------------------------------------------------------------------------------------------------------------------------
-- Get the list_id when the listname is given
getListID :: String -> String -> IO String
getListID apiKey listName = do
  mailinglists <- listMailingLists apiKey
  let rawlistid = headMay $ filterListID mailinglists
  case rawlistid of
    Just mlist -> return $ fromJust $ l_id mlist
    Nothing -> do putStrLn $ "Error: Invalid list name, " ++ listName
                  return ""
  where filterListID list = filter ((==(Just listName)) . l_name) list

-- | Build the response from URL and JSON data
processGET :: String -> String -> IO (Response ByteString)
processGET url apiKey = do 
  let initReq = applyBasicAuth (B8.pack "anystring") (B8.pack apiKey) $ fromJust $ parseUrl url
  let req = initReq { method = methodGet }
  catch (newManager tlsManagerSettings >>= (httpLbs req))
    (\(StatusCodeException s h c) -> do let ex = (show s ++ "," ++ show h ++ "," ++ show c)
                                        getResponse s h c apiKey
                                        exitWith (ExitFailure 0))

processPOST :: ToJSON a => String -> a -> String -> IO (Response ByteString)
processPOST url json apiKey = do 
  let initReq = applyBasicAuth (B8.pack "anystring") (B8.pack apiKey) $ fromJust $ parseUrl url
  let req = initReq { requestBody = RequestBodyLBS $ encode json
                    , method      = methodPost }
  manager <- newManager tlsManagerSettings
  catch (httpLbs req manager)
      (\(StatusCodeException s h c) -> do let ex = (show s ++ "," ++ show h ++ "," ++ show c)
                                          getResponse s h c apiKey)

processEmptyPOST :: String -> String -> IO (Response ByteString)
processEmptyPOST url apiKey = do 
  let initReq = applyBasicAuth (B8.pack "anystring") (B8.pack apiKey) $ fromJust $ parseUrl url
  let req = initReq { method = methodPost }
  manager <- newManager tlsManagerSettings
  catch (httpLbs req manager)
      (\(StatusCodeException s h c) -> do let ex = (show s ++ "," ++ show h ++ "," ++ show c)
                                          getResponse s h c apiKey)

processDELETE :: String -> String -> IO (Response ByteString)
processDELETE url apiKey = do
  let initReq  = applyBasicAuth (B8.pack "anystring") (B8.pack apiKey) $ fromJust $ parseUrl url
  let req      = initReq { method = methodDelete }
  manager <- newManager tlsManagerSettings
  catch (httpLbs req manager)
      (\(StatusCodeException s h c) -> do let ex = (show s ++ "," ++ show h ++ "," ++ show c)
                                          getResponse s h c apiKey)

-- | Construct the erroneous HTTP responses when an exception occurs
getResponse :: Status -> ResponseHeaders -> CookieJar -> String -> IO (Response ByteString)
getResponse s h c apiKey = do
  let url      = endPointUrl apiKey
  let initReq = applyBasicAuth (B8.pack "anystring") (B8.pack apiKey) $ fromJust $ parseUrl url
  let req  = initReq { method = methodGet }
  manager <- newManager tlsManagerSettings
  response <- httpLbs req manager
  let errorRes = response { responseStatus      = s
                          , responseVersion     = http11
                          , responseBody        = ""
                          , responseHeaders     = h
                          , responseCookieJar   = c }
  return errorRes

-- | Construct the end-point URL
endPointUrl :: String -> String
endPointUrl apiKey = "https://" ++ (last (splitString '-' apiKey)) ++ ".api.mailchimp.com/3.0"

-- | Utility function to split strings  
splitString :: Char -> String -> [String]
splitString d [] = []
splitString d s  = x : splitString d (drop 1 y) where (x,y) = span (/= d) s