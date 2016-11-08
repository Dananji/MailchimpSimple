{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- |
-- Module:      Utils.Types
-- License:     BSD3
-- Maintainer:  Dananji Liyanage <dan9131@gmail.com>
-- Stability:   experimental
--
-- JSON data structures to work with Mailchimp JSON API Version 3.0

module Utils.Types 
( 
 -- ** JSON Requests
 -- $requests
  Subscription(..)
 , EmailId(..)
 , Batch(..)
 , Operation(..)
 , Params(..)
 , Campaign(..)
 , Settings(..)
 , Receipient(..) 
 -- ** JSON Responses
 -- $responses
 , ListSubscribersResponse(..)
 , ListActivityResponse(..)
 , MailListResponse(..) 
 , TemplateResponse(..)
 , SendMailResponse(..)
 , SubscriptionResponse(..) 
 , BatchSubscriptionResponse(..)
 , SearchResultResponse(..) ) where

import           Data.Aeson
import           GHC.Generics hiding ( head )
import           Control.Monad ( mzero )
import           Data.Maybe ( catMaybes )
import           Data.Time.Calendar

-- | JSON structure containing a single subscription.
--
-- This structure includes the @email_address, email_type,@ and @status@ to be shown in
-- the member's profile.
-- @email_type@ accepts the two values; html / text
data Subscription =
  Subscription { s_email :: String
               , s_email_type :: String
               , s_status :: String
               } deriving (Show, Generic)

instance FromJSON Subscription where
instance ToJSON Subscription where
  toJSON (Subscription s_email s_email_type s_status) = object [ "email_address" .= s_email
                                                               , "email_type"    .= s_email_type
                                                               , "status"        .= s_status ]

-- | Constructor to build the EmailId.
data EmailId = Email String             -- ^ from email address of the member
             | EmailUniqueId String     -- ^ from unique ID provided by Mailchimp
             | ListEmailId String       -- ^ from emailID of the member in the particular list
  deriving (Show, Generic)

instance ToJSON EmailId where
  toJSON (Email t)         = object ["email" .= t]
  toJSON (EmailUniqueId t) = object ["euid" .= t]
  toJSON (ListEmailId t)   = object ["leid" .= t]

instance FromJSON EmailId where
  parseJSON (Object v) = do
    email <- fmap (fmap Email) $ v         .:? "email"
    euid  <- fmap (fmap EmailUniqueId) $ v .:? "euid"
    leid  <- fmap (fmap ListEmailId) $ v   .:? "leid"
    case catMaybes [email, euid, leid] of
      (x:_) -> return x
      _ -> mzero
  parseJSON _ = mzero

-- | JSON structure for Batch oprations.
data Batch = Batch { operations :: [Operation] } deriving (Show)
                   
instance ToJSON Batch where
  toJSON (Batch operations) = object [ "operations" .= operations ]

-- | JSON structure to contain the Batch operation information.
--
-- For GET requests, @o_param@ should be given the list of values.
-- For POST requests, @o_body@ should be given the String representation of the
-- encoded JSON data.
data Operation = Operation { o_method :: String
                           , o_path :: String
                           , o_params :: Params
                           , o_body :: String } deriving (Show)
                             
instance ToJSON Operation where
  toJSON (Operation method path params body) = object [ "method" .= method
                                                      , "path"   .= path
                                                      , "params" .= params
                                                      , "body"   .= body ]

-- | List of parameters to the URL in HTTP request
data Params = Params { params :: [String] } deriving (Show)

instance ToJSON Params where
  toJSON (Params params) = object [ "params" .= params ]

-- | JSON structure to construct a Campaign
-- 
-- This takes a @campaign_type@, which accepts these possible values;
-- @regular, plaintext, absplit, rss,@ and @variate@. 
data Campaign =
  Campaign { c_type :: String
           , c_settings :: Settings
           , c_receipients :: Receipient
           } deriving (Show)

instance ToJSON Campaign where
  toJSON (Campaign c_type c_settings c_receipients) = object [ "type"       .= c_type
                                                             , "settings"   .= c_settings 
                                                             , "recipients" .= c_receipients ]

-- | Settings for the Campaign creation
--
-- This includes the basic properties of the Campaign, like
-- subject_line, title, from_name, and reply_to address.
data Settings = 
  Settings { s_subject :: String
           , s_title :: String
           , s_from_name :: String
           , s_reply_to :: String
           } deriving (Show)

instance ToJSON Settings where
  toJSON (Settings s_subject s_title s_from_name s_reply_to) = object [ "subject_line" .= s_subject
                                                                      , "title"        .= s_title
                                                                      , "from_name"    .= s_from_name
                                                                      , "reply_to"     .= s_reply_to ]

-- | Structure to hold the list of recipients of a Campaign
data Receipient = ListID String deriving (Show)

instance ToJSON Receipient where
  toJSON (ListID r) = object ["list_id" .= r]

-- | Data structure to hold the HTTP response of the request
-- to list the subscribers in a mailing-list.
data ListSubscribersResponse =
  ListSubscribersResponse { ls_name :: Maybe String
                          , ls_euid :: Maybe String
                          , ls_listName :: Maybe String
                          , ls_emailType :: Maybe String
                          , ls_status :: Maybe String
                          } deriving (Show, Generic)

instance FromJSON ListSubscribersResponse where

-- | Data structure to hold the HTTP response of the request
-- to list the activity in a mailing-list.
data ListActivityResponse =
  ListActivityResponse { ac_date :: Maybe Day
                       , ac_sent :: Maybe Int
                       , ac_opens :: Maybe Int
                       , ac_clicks :: Maybe Int
                       , ac_subs :: Maybe Int
                       } deriving (Show, Generic)

instance FromJSON ListActivityResponse where

-- | Data structure to hold the HTTP response of the request to
-- list the mailing-lists in the account.
data MailListResponse =
  MailListResponse { l_name :: Maybe String
                   , l_id :: Maybe String 
                   } deriving (Show, Generic)
                   
instance FromJSON MailListResponse where

-- | Data structure to hold the Template information.
--
-- This contains the template name and ID.
data TemplateResponse =
  TemplateResponse { t_name :: Maybe String
                   , t_id :: Maybe Int 
                   } deriving (Show, Generic)

instance FromJSON TemplateResponse where

data SendMailResponse = 
  SendMailResponse { complete :: Bool } deriving (Show, Generic)

instance FromJSON SendMailResponse where

-- | Data structure to hold the HTTP response of the
-- subscription request. 
data SubscriptionResponse =
  SubscriptionResponse { email :: Maybe String
                       , euid :: Maybe String
                       , status :: Maybe String
                       , lname :: Maybe String 
                       } deriving (Show, Generic)
                       
instance FromJSON SubscriptionResponse where

-- | Data structure to hold the response of the Batch request 
data BatchSubscriptionResponse =
  BatchSubscriptionResponse { b_id :: Maybe String
                            , b_status :: Maybe String
                            } deriving (Show, Generic)
                            
instance FromJSON BatchSubscriptionResponse where

-- | Data structure to hold the response from search members request
data SearchResultResponse =
  SearchResultResponse { members :: Maybe [ListSubscribersResponse]
                       , total_items :: Maybe Int
                       } deriving (Show, Generic)

instance FromJSON SearchResultResponse where