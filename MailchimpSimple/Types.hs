{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module MailchimpSimple.Types ( Subscription(..)
                             , EmailId(..)
                             , Activity(..)
			                 , MailList(..)
                             , Filters(..)
			                 , Subscribers(..)
                             , BatchSubscription(..)
                             , Batch(..)
                             , Campaign(..)
                             , Options(..)
                             , Content(..)
                             , SendMail(..)
                             , Template(..)
                             , TemplateTypes(..)
                             , SubscriptionResponse(..) 
                             , MailListResponse(..) 
                             , ListSubscribersResponse(..) 
                             , BatchSubscriptionResponse(..) 
                             , SendMailResponse(..) 
                             , TemplateResponse(..) ) where

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
  parseJSON (Object v) = do
    lApikey <- v .: "apikey"
    lFilters <- v .: "filters"
    lStart <- v .: "start"
    lLimit <- v .: "limit"
    lSortField <- v .: "sort_field"
    lSortDir <- v .: "sort_dir"
    return $ MailList lApikey lFilters lStart lLimit lSortField lSortDir
  parseJSON _ = mzero
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
  Campaign { c_apikey :: String
           , c_type :: String
           , c_options :: Options 
           , c_content :: Content
           } deriving (Show, Generic)
           
instance FromJSON Campaign where
instance ToJSON Campaign where
  toJSON (Campaign c_apikey c_type c_options c_content) = object [ "apikey"  .= c_apikey
                                                                 , "type"    .= c_type
                                                                 , "options" .= c_options 
                                                                 , "content" .= c_content ]
                                                       
data Options =
  Options { o_list_id :: String
          , o_subject :: String
          , o_from_email :: String
          , o_from_name :: String
          , o_to_name :: String
          , o_template_id :: Int 
          } deriving (Show, Generic)
          
instance FromJSON Options where
instance ToJSON Options where
  toJSON (Options o_list_id o_subject o_from_email o_from_name o_to_name o_template_id) = object [ "list_id"     .= o_list_id
                                                                                                 , "subject"     .= o_subject
                                                                                                 , "from_email"  .= o_from_email
                                                                                                 , "from_name"   .= o_from_name
                                                                                                 , "to_name"     .= o_to_name 
                                                                                                 , "template_id" .= o_template_id ]
data Content = HTML String
             | Text String
             | URL String 
        deriving (Show)
             
instance ToJSON Content where
  toJSON (HTML t) = object ["html" .= t]
  toJSON (Text t) = object ["text" .= t]
  toJSON (URL t)  = object ["url"  .= t]

instance FromJSON Content where
  parseJSON (Object v) = do
    html <- fmap (fmap HTML) $ v .:? "html"
    text <- fmap (fmap Text) $ v .:? "text"
    url  <- fmap (fmap URL) $ v  .:? "url"
    case catMaybes [html, text, url] of
      (x:_) -> return x
      _ -> mzero
  parseJSON _ = mzero
  
data SendMail =
  SendMail { m_apikey :: String
           , m_cid :: String
           } deriving (Show, Generic)
           
instance FromJSON SendMail where
instance ToJSON SendMail where
  toJSON (SendMail m_apikey m_cid) = object [ "apikey"      .= m_apikey
                                            , "cid"         .= m_cid ]

data Template =
  Template { t_apikey :: String
           , t_types :: TemplateTypes
           } deriving (Show, Generic)

instance FromJSON Template where
instance ToJSON Template where
  toJSON (Template t_apikey t_types) = object [ "apikey" .= t_apikey
                                              , "types"  .= t_types ]

data TemplateTypes =
  TemplateTypes { user :: Bool
                , gallery :: Bool
                , base :: Bool 
                } deriving (Show, Generic)

instance FromJSON TemplateTypes where
instance ToJSON TemplateTypes where                
---------------------------------------------------- Responses --------------------------------------------------------

data SubscriptionResponse =
  SubscriptionResponse { email :: String
                       , euid :: String
                       , leid :: String 
                       } deriving (Show)
                       
instance FromJSON SubscriptionResponse where
  parseJSON (Object v) = do
    srEmail <- v .: "email"
    srEuid <- v .: "euid"
    srLeid <- v .: "leid"
    return $ SubscriptionResponse srEmail srEuid srLeid
  parseJSON _ = mzero
instance ToJSON SubscriptionResponse where
  toJSON (SubscriptionResponse email euid leid) = object [ "email" .= email
                                                         , "euid"  .= euid
                                                         , "leid"  .= leid ]
                                                                  
data MailListResponse =
  MailListResponse { l_name :: Maybe String
                   , l_id :: Maybe String 
                   } deriving (Show, Generic)
                   
instance FromJSON MailListResponse where
instance ToJSON MailListResponse where

data ListSubscribersResponse =
  ListSubscribersResponse { s_name :: Maybe String
                          , s_euid :: Maybe String
                          , s_list_name :: Maybe String
                          , s_emailType :: Maybe String
                          } deriving (Show, Generic)
                      
instance FromJSON ListSubscribersResponse where
instance ToJSON ListSubscribersResponse where

data BatchSubscriptionResponse =
  BatchSubscriptionResponse { add_count :: Maybe Int
                            , adds :: [Maybe SubscriptionResponse]
                            } deriving (Show, Generic)
                            
instance FromJSON BatchSubscriptionResponse where
instance ToJSON BatchSubscriptionResponse where

data SendMailResponse = 
  SendMailResponse { complete :: Bool } deriving (Show, Generic)
  
instance FromJSON SendMailResponse where
instance ToJSON SendMailResponse where

data TemplateResponse =
  TemplateResponse { t_name :: Maybe String
                   , t_id :: Maybe Int 
                   } deriving (Show, Generic)
                   
instance FromJSON TemplateResponse where
instance ToJSON TemplateResponse where