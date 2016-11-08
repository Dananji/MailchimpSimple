{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      MailchimpSimple
-- License:     BSD3
-- Maintainer:  Dananji Liyanage <dan9131@gmail.com>
-- Stability:   experimental
--
-- Types and functions for working with Mailchimp JSON API Version 3.0

module MailchimpSimple
(
 -- ** Working with Lists
 -- $lists
   listMailingLists
 , listSubscribers
 , listListActivity
 , addSubscriber
 , removeSubscriber
 -- ** Retrieve Template related data
 -- $templates
 , getTemplates
 -- ** Working with Campaigns
 -- $campaigns
 , getCampaigns
 , createCampaign
 , sendEmail
 -- ** Batch Requests
 -- $batches
 , batchSubscribe 
 -- ** Search Members
 -- $search
 , searchMembersInList ) where

import           Network.HTTP.Conduit
import           Network.HTTP.Types ( methodPost, methodGet, methodDelete, Method(..), Status(..), http11, ResponseHeaders, hContentType )
import           Control.Monad.IO.Class ( liftIO )

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
import           Data.Time.Calendar

import           Data.Maybe ( Maybe(..), fromJust )
import qualified Data.Vector as V
import           Crypto.Hash.MD5 as MD5


-- App modules
import           Utils.Types
import           Utils.Logger

-- | Takes an @apiKey@ of a Mailchimp account, and gives all the mailing-lists in the account.
-- 
-- This function lists the mailing lists in a particular account
listMailingLists 
    :: String
        -> IO [MailListResponse]
listMailingLists apiKey = do
  let url = endPointUrl apiKey
  let lUrl         = url ++ "/lists?fields=lists.id,lists.name"
  response         <- processEmptyRequest lUrl apiKey methodGet
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

-- | Takes an @apiKey@ of the Mailchimp account, and a @listName@.
-- Retrieves all the members in the given list.
-- 
-- Request URL specifies which data to be returned from the response. They are, 
-- @email_address, unique_email_id, email_type, list_id@, and @status@ for each
-- member in the reponse.
-- 
-- This function lists subscribers in a mailing list
listSubscribers 
    :: String
        -> String
        -> IO [ListSubscribersResponse]
listSubscribers apiKey listName = do
  let url  = endPointUrl apiKey
  listid   <- getListID apiKey listName
  let lUrl = url ++ "/lists/" ++ listid ++ "/members?fields=members.email_address,members.unique_email_id,members.email_type,members.list_id,members.status"
  response <- processEmptyRequest lUrl apiKey methodGet
  let resBody = decode (responseBody response) :: Maybe Value 
  let vArray = resBody ^. key "members" :: Maybe Array
  let listSubResponse = getValues vArray
  return listSubResponse
  where getValues ls
          | ls /= (Just V.empty) = constructLSRes (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise = []
        constructLSRes elem = ListSubscribersResponse sName sEuid (Just listName) sEmailType sStatus
                              where sName      = elem ^. key "email_address" :: Maybe String
                                    sEuid      = elem ^. key "unique_email_id" :: Maybe String
                                    sEmailType = elem ^. key "email_type" :: Maybe String
                                    sStatus    = elem ^. key "status" :: Maybe String

-- | Takes an @apiKey@ of the Mailchimp account, and a @listName@.
-- Retrieves the activity summary in the given list.
-- 
-- Request URL specifies which data to be returned from the response. They are, 
-- @day, emails_sent, unique_opens, recipient_clicks@, and @subs@ for the given list.
-- 
-- This function gives the summary of activities in a mailing list
listListActivity 
    :: String
        -> String
        -> IO [ListActivityResponse]
listListActivity apiKey listName = do
  let url  = endPointUrl apiKey
  listid   <- getListID apiKey listName
  let aUrl = url ++ "/lists/" ++ listid ++ "/activity?fields=activity.day,activity.emails_sent,activity.unique_opens,activity.recipient_clicks,activity.subs"
  response <- processEmptyRequest aUrl apiKey methodGet
  let resBody = decode (responseBody response) :: Maybe Value
  let vArray  = resBody ^. key "activity" :: Maybe Array
  let listActivityResponse = getValues vArray
  return listActivityResponse
  where getValues ls
          | ls /= (Just V.empty) = constructALRes (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise = []
        constructALRes elem = ListActivityResponse date mailSent uniqueOpen clicks subs
                              where date       = elem ^. key "day" :: Maybe Day
                                    mailSent   = elem ^. key "emails_sent" :: Maybe Int
                                    uniqueOpen = elem ^. key "unique_opens" :: Maybe Int
                                    clicks     = elem ^. key "recipient_clicks" :: Maybe Int
                                    subs       = elem ^. key "subs" :: Maybe Int

-- | Taking @apiKey,@ @listName,@ @emailAddress,@ @emailType,@ and @memberStatus@ as input
-- parameters in the given order, this function creates and add the member to the given list.
-- 
-- This function adds a new member to a given list
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
  listid   <- getListID apiKey listName
  let sUrl = url ++ "/lists/" ++ listid ++ "/members"
  response <- processRequest sUrl apiKey subscription methodPost
  let resBody = decode (responseBody response) :: Maybe Value
  let subscribers = constructSARes resBody
  return subscribers
  where constructSARes elem = do let email = elem ^. key "email_address"  :: Maybe String
                                 let euid = elem ^. key "unique_email_id" :: Maybe String
                                 let status = elem ^. key "status" :: Maybe String
                                 SubscriptionResponse email euid status (Just listName)
        filterListID list = filter ((==(Just listName)) . l_name) list

-- | Giving an @apiKey,@ @emailAddress,@ and @listName@ which the member belongs to, this 
-- function unsubscribe the member from the list. This function does not deletes the particular
-- user profile from the mailing-list.
-- 
-- This function removes a member from a given list
removeSubscriber
    :: String 
        -> String 
        -> String 
        -> IO Bool
removeSubscriber apiKey email listName = do
  let url = endPointUrl apiKey
  listid  <- getListID apiKey listName
  let subhash = createHash email
  let rUrl    = url ++ "/lists/" ++ listid ++ "/members/" ++ subhash
  response    <- processEmptyRequest rUrl apiKey methodDelete
  let status  = statusCode $ responseStatus response
  case status of
    204 -> return True
    _   -> return False
  where createHash str    = (B8.unpack (calculateHash (strToBS str)))
        calculateHash str = (B16.encode (hash str))
        strToBS str       = B8.pack str


-- | Input parameters for this function is the @apiKey@ of the Mailchimp account.
--
-- This function retrieves all the templates in the account.
getTemplates
    :: String
        -> IO [TemplateResponse]
getTemplates apiKey = do
  let url = endPointUrl apiKey
  let tUrl         = url ++ "/templates?fields=templates.id,templates.name"
  response         <- processEmptyRequest tUrl apiKey methodGet
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

-- | Taking the @apiKey@ of a Mailchimp account, this function returns all the
-- stored unsent Campaigns.
--
-- This function returns all the Campaigns in the account.
getCampaigns :: String -> IO [(Maybe String, Maybe String)]
getCampaigns apiKey = do
  let url = endPointUrl apiKey
  let cUrl = url ++ "/campaigns?fields=campaigns.id,campaigns.settings"
  response <- processEmptyRequest cUrl apiKey methodGet
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

-- | Usage of this function to create a new Campaign and save is as follows;
-- 
-- @createCampaign@ @apiKey listName replyTo fromName cType title subject -> campaignID@
-- 
-- This function creates a new campaign and save it 
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
  listid  <- getListID apiKey listName
  let campaign = Campaign { c_type       = cType
                          , c_settings    = Settings { s_subject   = subject
                                                     , s_title     = title
                                                     , s_from_name = fromName
                                                     , s_reply_to  = replyTo }
                          , c_receipients = (ListID listid) }
  let eUrl    = url ++ "/campaigns"
  response    <- processRequest eUrl apiKey campaign methodPost
  let resBody = decode (responseBody response) :: Maybe Value
  let campaignid = resBody ^. key "id" :: Maybe String
  return campaignid

-- | Input parameters for this function are @apiKey@ and the @campaignID@ of the
-- particular Campaign to be sent.
-- 
-- This function sends an email campaign
sendEmail
 :: String
    -> String
    -> IO (Either String SendMailResponse)  
sendEmail apiKey cid = do
  let url  = endPointUrl apiKey
  let sUrl = url ++ "/campaigns/" ++ cid ++ "/actions/send"
  response <- processEmptyRequest sUrl apiKey methodPost
  let sendRes = eitherDecode (responseBody response) :: Either String SendMailResponse
  return sendRes

-- | Efficiently processes a batch subscription requests for a given list
-- of @emailAddress@ and @subscriptionStatus@ combinations.
-- 
-- This function can be re-implemented to perform other batch requests by changing the
-- @body@ and @path@ properties of @Operation@ data structure.
-- 
-- This function adds a batch of subscribers
batchSubscribe 
    :: String
        -> String
        -> [(String, String)]
        -> IO BatchSubscriptionResponse
batchSubscribe apiKey listName subs = do
  let url = endPointUrl apiKey
  listid <- getListID apiKey listName
  let batchSubs = map constructSubs subs
  let batchOps  = map (constructBSRes listid) batchSubs
  let batchSubscription = Batch { operations = batchOps }
  let bUrl = url ++ "/batches"
  response <- processRequest bUrl apiKey batchSubscription methodPost
  let resBody = decode (responseBody response) :: Maybe Value
  let batchResponse = BatchSubscriptionResponse (resBody ^. key "id" :: Maybe String) (resBody ^. key "status" :: Maybe String)
  return batchResponse
  where constructSubs (email, status) = Subscription { s_email      = email
                                                     , s_email_type = "html"
                                                     , s_status     = status }
        constructBSRes listid sub = Operation { o_method = "POST"
                                              , o_path = "/lists/" ++ listid ++ "/members"
                                              , o_params = Params { params = [] }
                                              , o_body = B8.unpack $ BL.toStrict $ encode sub }

-- | Search a specific list for members that match specified query terms.
-- 
-- Invoking this function with an empty @listName@ , we can get all 
-- the memebers that match the specified query terms in the account.
searchMembersInList
    :: String
        -> String
        -> String
        -> IO SearchResultResponse
searchMembersInList apiKey listName query = do
  let url  = endPointUrl apiKey
  listid   <- getListID apiKey listName
  let sUrl = if null listid
                then url ++ "/search-members?query=" ++ query
                else url ++ "/search-members?query=" ++ query ++ "&list_id=" ++ listid
  response <- processEmptyRequest sUrl apiKey methodGet
  if (statusCode $ responseStatus response) < 400
    then do let resBody       = decode (responseBody response) :: Maybe Value
            let fullsearch    = resBody ^. key "full_search" :: Maybe Value
            let totalcount    = fullsearch ^. key "total_items" :: Maybe Int
            let members       = fullsearch ^. key "members" :: Maybe Array
            let parsedmembers = getValues members
            let searchresults = SearchResultResponse (Just parsedmembers) totalcount
            return searchresults
    else error $ "Error in response: " ++ show response  
  where getValues ls
          | ls /= (Just V.empty) = constructMembers (fmap V.head ls) : getValues (fmap V.tail ls)
          | otherwise = []
        constructMembers elem = ListSubscribersResponse sName sEuid sListID sEmailType sStatus
                              where sName      = elem ^. key "email_address" :: Maybe String
                                    sEuid      = elem ^. key "unique_email_id" :: Maybe String
                                    sListID    = elem ^. key "list_id" :: Maybe String
                                    sEmailType = elem ^. key "email_type" :: Maybe String
                                    sStatus    = elem ^. key "status" :: Maybe String       

-------------------------------------------------------------------------------------------------------------------------------------

-- | Get the list_id when the listname is given
getListID :: String -> String -> IO String
getListID apiKey listName
  | null listName = return ""
  | otherwise     = do mailinglists <- listMailingLists apiKey
                       let rawlistid = headMay $ filterListID mailinglists
                       case rawlistid of
                         Just mlist -> return $ fromJust $ l_id mlist
                         Nothing    -> do putStrLn $ "Error: Invalid list name, " ++ listName
                                          return ""
  where filterListID list = filter ((==(Just listName)) . l_name) list

-- | Build the response from URL and JSON data
processRequest :: ToJSON a => String -> String -> a -> Method -> IO (Response ByteString)
processRequest url apiKey json httpmethod = do
  let initReq = applyBasicAuth (B8.pack "anystring") (B8.pack apiKey) $ fromJust $ parseUrlThrow url
  let req = initReq { requestBody = RequestBodyLBS $ encode json
                    , method       = httpmethod }
  catch (newManager tlsManagerSettings >>= (httpLbs req))
    (\exception -> do writeLog ERROR "processRequest" url (show exception)
                      handleException exception)

-- | Build the response from URL and JSON data
processEmptyRequest :: String -> String -> Method -> IO (Response ByteString)
processEmptyRequest url apiKey httpmethod = do
  let initReq = applyBasicAuth (B8.pack "anystring") (B8.pack apiKey) $ fromJust $ parseUrlThrow url
  let req = initReq { method = httpmethod }
  catch (newManager tlsManagerSettings >>= (httpLbs req))
    (\exception -> do writeLog ERROR "processEmptyRequest" url (show exception)
                      handleException exception)

-- | Exception handling in making the HTTP requests
handleException :: HttpException -> IO (Response ByteString)
handleException (HttpExceptionRequest req exception) = do
  case exception of
    StatusCodeException res _ -> do let errorres = res { responseVersion = http11
                                                       , responseBody    = "" }
                                    return errorres
    ConnectionTimeout -> error "Connection timed out."
    ResponseTimeout   -> error "Response timed out."
    _                 -> error "Unidentified HTTP exception."

-- | Construct the end-point URL
endPointUrl :: String -> String
endPointUrl apiKey
  | null splitted= error "Empty apiKey input into the function"
  | otherwise    = "https://" ++ (last splitted) ++ ".api.mailchimp.com/3.0"
  where splitted = splitString '-' apiKey

-- | Utility function to split strings  
splitString :: Char -> String -> [String]
splitString d [] = []
splitString d s  = x : splitString d (drop 1 y) where (x,y) = span (/= d) s