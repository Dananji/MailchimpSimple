{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Types ( Subscription(..)
             , EmailId(..)
             , Activity(..)
			 , MailList(..)
             , Filters(..)
			 , Subscribers(..)
             , BatchSubscription(..)
             , Batch(..)
             , Campaign(..)
             , SendMail(..) ) where

import           Data.Aeson
import           GHC.Generics hiding ( head )
import           Control.Monad ( mzero )
import           Data.Maybe ( catMaybes )

data Subscription =
  Subscription { s_apikey :: String
               , s_id :: String
               , s_email :: EmailId
               , s_email_type :: String
               , s_dou_opt :: Bool
               , s_up_ex :: Bool
               , s_rep_int :: Bool
               , s_send :: Bool			   
               } deriving (Show, Generic)
		   
instance FromJSON Subscription where
instance ToJSON Subscription where
  toJSON (Subscription s_apikey s_id s_email s_email_type s_dou_opt s_up_ex s_rep_int s_send) = object [ "apikey"            .= s_apikey
                                                                                                       , "id"                .= s_id
                                                                                                       , "email"             .= s_email
                                                                                                       , "email_type"        .= s_email_type
                                                                                                       , "double_optin"      .= s_dou_opt
                                                                                                       , "update_existing"   .= s_up_ex
                                                                                                       , "replace_interests" .= s_rep_int
                                                                                                       , "send_welcome"      .= s_send ]

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

data BatchSubscription =
  BatchSubscription { b_apikey :: String
                    , b_id :: String
					, b_batch :: [Batch]
                    , b_dou_opt :: Bool
                    , b_up_ex :: Bool
                    , b_rep_int :: Bool
					} deriving (Show, Generic)
					
instance FromJSON BatchSubscription where
instance ToJSON BatchSubscription where
  toJSON (BatchSubscription b_apikey b_id b_batch b_dou_opt b_up_ex b_rep_int) = object [ "apikey"            .= b_apikey
                                                                                        , "id"                .= b_id
                                                                                        , "batch"             .= b_batch
                                                                                        , "double_optin"      .= b_dou_opt
                                                                                        , "update_existing"   .= b_up_ex
                                                                                        , "replace_interests" .= b_rep_int ]

data Batch =
  Batch { b_email :: EmailId
        , b_email_type :: String
		} deriving (Show, Generic)
		
instance FromJSON Batch where
instance ToJSON Batch where
  toJSON (Batch b_email b_email_type) = object [ "email"      .= b_email
                                               , "email_type" .= b_email_type ]

data Activity = 
  Activity { a_apikey :: String
           , a_id :: String
		   } deriving (Show, Generic)

instance FromJSON Activity where
  parseJSON (Object v) = do
    aApikey <- v .: "apikey"
    aId     <- v .: "id"
    return $ Activity aApikey aId
  parseJSON _ = mzero
instance ToJSON Activity where
  toJSON (Activity a_apikey a_id) = object ["apikey" .= a_apikey
                                          , "id" .= a_id ]

data MailList =
  MailList { l_apikey :: String
           , l_filters :: Filters
	       , l_start :: Int 
	       , l_limit :: Int
	       , l_sort_field :: String
	       , l_sort_dir :: String
	       } deriving (Show, Generic)

instance FromJSON MailList where
instance ToJSON MailList where
  toJSON (MailList l_apikey l_filters l_start l_limit l_sort_field l_sort_dir) = object [ "apikey"     .= l_apikey
                                                                                        , "filters"  .= l_filters
                                                                                        , "start"      .= l_start
                                                                                        , "limit"      .= l_limit
                                                                                        , "sort_field" .= l_sort_field
                                                                                        , "sort_dir"   .= l_sort_dir ]
                                                                                      
data Filters =
  Filters { list_id :: String
          , list_name :: String
          } deriving (Show, Generic)
          
instance FromJSON Filters where
instance ToJSON Filters where

data Subscribers =
  Subscribers { su_apikey :: String
              , su_id :: String
              , su_status :: String
              } deriving (Show, Generic)
		   
instance FromJSON Subscribers where
instance ToJSON Subscribers where
  toJSON (Subscribers su_apikey su_id su_status) = object [ "apikey" .= su_apikey
                                                          , "id"     .= su_id
                                                          , "status" .= su_status]
                                                          
data Campaign =
  Campaign { cid :: String
           , title :: String
           } deriving (Show, Generic)
           
instance FromJSON Campaign where
instance ToJSON Campaign where
                                                                              
data SendMail =
  SendMail { m_apikey :: String
           , m_cid :: String
           , m_test_emails :: [String]
           , m_send_type :: String
           } deriving (Show, Generic)
           
instance FromJSON SendMail where
instance ToJSON SendMail where
  toJSON (SendMail m_apikey m_cid m_test_emails m_send_type) = object [ "apikey"      .= m_apikey
                                                                      , "cid"         .= m_cid
                                                                      , "test_emails" .= m_test_emails
                                                                      , "send_type"   .= m_send_type ]