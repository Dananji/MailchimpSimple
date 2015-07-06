{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Logger ( writeLog ) where

import           Data.Time.Clock
import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS ( unpack )

-- | Constructor for the Log levels 
data LogLevels = ERROR | DEBUG | INFO deriving (Show, Eq, Generic)

-- { 
-- To encode and decode using Data.Aeson package define 'FromJSON' and 'ToJSON' 
-- instances for 'LogLevels' data type 
-- }
instance FromJSON LogLevels where
instance ToJSON LogLevels where

-- | Constructor for the Log entry 
data LogEntry =
  LogEntry { logEntryTime :: UTCTime
		   , logLevel :: LogLevels
		   , loggingMethod :: String
		   , logData :: String
		   , logMessage :: String
		   } deriving (Show, Eq, Generic)

-- { 
-- To encode and decode using Data.Aeson package define 'FromJSON' and 'ToJSON' 
-- instances for 'LogEntry' data type 
-- }
instance FromJSON LogEntry where
instance ToJSON LogEntry where		   

-- | Build the Log entry
constructLog :: LogLevels -> String -> String -> String -> IO LogEntry
constructLog lLevel lMethod lData lMessage = do
  myTime <- getCurrentTime
  let log =LogEntry { logEntryTime = myTime
					, logLevel = lLevel
					, loggingMethod = lMethod
					, logData = lData
					, logMessage = lMessage }
  return log  
  
-- {
-- Append the log entry to an external log files
-- Error logs to 'error.log' file
-- Other logs to 'access.log' file
-- }
writeLog :: LogLevels -> String -> String -> String -> IO ()
writeLog lLevel lMethod lData lMessage = do
  logEntry <- constructLog lLevel lMethod lData lMessage
  let logEntryStr = BS.unpack $ encode logEntry
  if lLevel == ERROR 
    then appendFile "error.log" logEntryStr
	else appendFile "access.log" logEntryStr