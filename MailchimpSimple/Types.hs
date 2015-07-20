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
                             , SendMail(..) 
                             , SubscriptionResponse(..) 
                             , MailListResponse(..) 
                             , ListSubscribersResponse(..) 
                             , BatchSubscriptionResponse(..) ) where

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
                                                                      
data MandrillMail =
  MandrillMail { key :: String
               , message :: Message
               , async :: Bool
               , ip_pool :: String
               , send_at :: String
               } deriving (Show, Generic)
               
instance FromJSON MandrillMail where
instance ToJSON MandrillMail where

data Message = 
  Message { html :: String
          , text :: String
          , subject :: String
          , from_email :: String
          , from_name :: String
          , to :: [Emails]
          , headers :: Headers
          , important :: Bool
          , track_opens :: Bool
          , track_clicks :: Bool
          , auto_text :: Bool
          , auto_html :: Bool
          , inline_css :: Bool
          , url_strip_qs :: Bool
          , preserve_recipients :: Bool
          , view_content_link :: Bool
          , bcc_address :: String
          , tracking_domain :: String
          , signing_domain :: String
          , return_path_domain :: String
          , merge :: Bool
          , merge_language :: String
          , global_merge_vars :: [GlobalMergeVars]
          , merge_vars :: [MergeVars]
          , tags :: [String]
          , subaccount :: String
          , google_analytics_domains :: [String]
          , google_analytics_campaign :: String
          , metadata :: [RecipientMetadata]
          , attachment :: [Attachement]
          , images :: [Image]
          } deriving (Show, Generic)
          
instance FromJSON Message where
instance ToJSON Message where

data Emails =
  Emails { e_email :: String
         , name :: String
         , email_type :: String
         } deriving (Show)

instance FromJSON Emails where
  parseJSON (Object v) = do
    eEmail <- v .: "email*"
    name <- v .: "name"
    email_type <- v .: "type"
    return $ Emails eEmail name email_type
  parseJSON _ = mzero
instance ToJSON Emails where
  toJSON (Emails eEmail name email_type) = object [ "email*" .= eEmail
                                                  , "name"   .= name
                                                  , "type"   .= email_type ]
                                                 
data Headers = Headers {} deriving (Show, Generic)
instance FromJSON Headers where
instance ToJSON Headers where

data GlobalMergeVars =
  GlobalMergeVars { g_name :: String
                  , g_content :: String
                  } deriving (Show)
                  
instance FromJSON GlobalMergeVars where
  parseJSON (Object v) = do
    g_name <- v .: "name"
    g_content <- v .: "content"
    return $ GlobalMergeVars g_name g_content
  parseJSON _ = mzero
instance ToJSON GlobalMergeVars where
  toJSON (GlobalMergeVars g_name g_content) = object [ "name"    .= g_name
                                                     , "content" .= g_content]

data MergeVars =
  MergeVars { m_rcpt :: String
            , m_vars :: [Vars]
            } deriving (Show, Generic)
                             
instance FromJSON MergeVars where
  parseJSON (Object v) = do
    mRcpt <- v .: "rcpt*"
    mVars <- v .: "vars"
    return $ MergeVars mRcpt mVars
instance ToJSON MergeVars where
  toJSON (MergeVars m_rcpt m_vars) = object [ "rcpt*" .= m_rcpt
                                            , "vars"  .= m_vars ]

data Vars =
  Vars { v_name :: String
       , v_content :: String
       } deriving (Show)

instance FromJSON Vars where
  parseJSON (Object v) = do
    v_name <- v .: "name"
    v_content <- v .: "content"
    return $ Vars v_name v_content
  parseJSON _ = mzero
instance ToJSON Vars where
  toJSON (Vars v_name v_content) = object [ "name"    .= v_name
                                          , "content" .= v_content]
                                          
data RecipientMetadata =
  RecipientMetadata { rcpt :: String
                    , values :: [Values]
                    } deriving (Show, Generic)
                    
instance FromJSON RecipientMetadata where
instance ToJSON RecipientMetadata where

data Values = Values {} deriving (Show, Generic)
instance FromJSON Values where
instance ToJSON Values where

data Attachement =
  Attachement { a_type :: String
              , a_name :: String
              , a_content :: String
              } deriving (Show, Generic)
              
instance FromJSON Attachement where
  parseJSON (Object v) = do
    aType <- v .: "type"
    aName <- v .: "name"
    aContent <- v .: "content"
    return $ Attachement aType aName aContent
  parseJSON _ = mzero
instance ToJSON Attachement where
  toJSON (Attachement a_type a_name a_content) = object [ "type" .= a_type
                                                        , "name" .= a_name
                                                        , "content" .= a_content ]
                                                        
data Image =
  Image { i_type :: String
        , i_name :: String
        , i_content :: String
        } deriving (Show, Generic)
              
instance FromJSON Image where
  parseJSON (Object v) = do
    iType <- v .: "type"
    iName <- v .: "name"
    iContent <- v .: "content"
    return $ Image iType iName iContent
  parseJSON _ = mzero
instance ToJSON Image where
  toJSON (Image i_type i_name i_content) = object [ "type" .= i_type
                                                  , "name" .= i_name
                                                  , "content" .= i_content ]
                                          
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
  MailListResponse { l_name :: String
                   , l_id :: String 
                   } deriving (Show, Generic)
                   
instance FromJSON MailListResponse where
instance ToJSON MailListResponse where

data ListSubscribersResponse =
  ListSubscribersResponse { s_name :: String
                          , s_euid :: String
                          , s_list_name :: String
                          , s_emailType :: String
                          } deriving (Show, Generic)
                      
instance FromJSON ListSubscribersResponse where
instance ToJSON ListSubscribersResponse where

data BatchSubscriptionResponse =
  BatchSubscriptionResponse { add_count :: Int
                            , adds :: [SubscriptionResponse]
                            } deriving (Show, Generic)
                            
instance FromJSON BatchSubscriptionResponse where
instance ToJSON BatchSubscriptionResponse where