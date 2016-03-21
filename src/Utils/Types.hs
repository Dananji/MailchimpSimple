{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Utils.Types ( Subscription(..)
                   , EmailId(..)
                   , Batch(..)
                   , Operation(..)
                   , Params(..)
                   , Campaign(..)
                   , Settings(..)
                   , Receipient(..) 
                   , ListSubscribersResponse(..)
                   , MailListResponse(..) 
                   , TemplateResponse(..)
                   , SendMailResponse(..)
                   , SubscriptionResponse(..) 
                   , BatchSubscriptionResponse(..) ) where

import           Data.Aeson
import           GHC.Generics hiding ( head )
import           Control.Monad ( mzero )
import           Data.Maybe ( catMaybes )

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

data EmailId = Email String
             | EmailUniqueId String
             | ListEmailId String
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

data Batch = Batch { operations :: [Operation] } deriving (Show)
                   
instance ToJSON Batch where
  toJSON (Batch operations) = object [ "operations" .= operations ]
                                                  
data Operation = Operation { o_method :: String
                           , o_path :: String
                           , o_params :: Params
                           , o_body :: String } deriving (Show)
                             
instance ToJSON Operation where
  toJSON (Operation method path params body) = object [ "method" .= method
                                                      , "path"   .= path
                                                      , "params" .= params
                                                      , "body"   .= body ]
                                                      
data Params = Params { params :: [String] } deriving (Show)

instance ToJSON Params where
  toJSON (Params params) = object [ "params" .= params ]

data Campaign =
  Campaign { c_type :: String
           , c_settings :: Settings
           , c_receipients :: Receipient
           } deriving (Show)

instance ToJSON Campaign where
  toJSON (Campaign c_type c_settings c_receipients) = object [ "type"       .= c_type
                                                             , "settings"   .= c_settings 
                                                             , "recipients" .= c_receipients ]

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

data Receipient = ListID String deriving (Show)

instance ToJSON Receipient where
  toJSON (ListID r) = object ["list_id" .= r]

---------------------------------------------------- Responses --------------------------------------------------------
data ListSubscribersResponse =
  ListSubscribersResponse { ls_name :: Maybe String
                          , ls_euid :: Maybe String
                          , ls_list_name :: Maybe String
                          , ls_emailType :: Maybe String
                          , ls_status :: Maybe String
                          } deriving (Show, Generic)

instance FromJSON ListSubscribersResponse where

data MailListResponse =
  MailListResponse { l_name :: Maybe String
                   , l_id :: Maybe String 
                   } deriving (Show, Generic)
                   
instance FromJSON MailListResponse where

data TemplateResponse =
  TemplateResponse { t_name :: Maybe String
                   , t_id :: Maybe Int 
                   } deriving (Show, Generic)

instance FromJSON TemplateResponse where

data SendMailResponse = 
  SendMailResponse { complete :: Bool } deriving (Show, Generic)

instance FromJSON SendMailResponse where

data SubscriptionResponse =
  SubscriptionResponse { email :: Maybe String
                       , euid :: Maybe String
                       , status :: Maybe String
                       , lname :: Maybe String 
                       } deriving (Show, Generic)
                       
instance FromJSON SubscriptionResponse where

data BatchSubscriptionResponse =
  BatchSubscriptionResponse { b_id :: Maybe String
                            , b_status :: Maybe String
                            } deriving (Show, Generic)
                            
instance FromJSON BatchSubscriptionResponse where