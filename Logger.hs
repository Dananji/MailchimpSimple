{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Logger ( LogLevels(..), writeLog ) where

import           Data.Time.Clock
import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS ( unpack )
import           Data.List as L ( intercalate )

-- | Constructor for the Log levels 
data LogLevels = ERROR | DEBUG | INFO deriving (Show, Eq, Generic)

-- { 
-- To encode and decode using Data.Aeson package define 'FromJSON' and 'ToJSON' 
-- instances for 'LogLevels' data type 
-- }
instance FromJSON LogLevels where
instance ToJSON LogLevels where

-- {
-- Constructor for the Log entry 
-- logEntryTime -> System time which the log is written
-- logLevel     -> Logging purpose (ERROR/INFO/DEBUG)
-- loggingMethod-> Module of the program which writes the log
-- logInputData -> Input data at the point of logging
-- logMessage   -> Output message of the method
-- } 
data LogEntry =
  LogEntry { logEntryTime  :: UTCTime
		   , logLevel      :: LogLevels
		   , loggingMethod :: String
		   , logInputData  :: String
		   , logMessage    :: String
		   } deriving (Show, Eq, Generic)

-- { 
-- To encode and decode using Data.Aeson package define 'FromJSON' and 'ToJSON' 
-- instances for 'LogEntry' data type 
-- }
instance FromJSON LogEntry where
instance ToJSON LogEntry where		   

-- | Build the Log entry
constructLog :: LogLevels -> String -> String -> String -> IO LogEntry
constructLog lLevel lMethod lInputData lMessage = do
  utcTime <- getCurrentTime
  let myTime = addUTCTime 19800 utcTime
  let log =LogEntry { logEntryTime  = myTime
					, logLevel      = lLevel
					, loggingMethod = lMethod
					, logInputData  = lInputData
					, logMessage    = lMessage }
  return log  
  
-- {
-- Append the log entry to an external log files
-- Error logs to 'error.log' file
-- Other logs to 'access.log' file
-- }
writeLog :: LogLevels -> String -> String -> String -> IO ()
writeLog lLevel lMethod lInputData lMessage = do
  logEntry <- constructLog lLevel lMethod lInputData lMessage
  let logEntryStr = BS.unpack $ encode logEntry
  let logEntryProcessed = (removeChars "{}" logEntryStr) ++ "\n"
  if lLevel == ERROR 
    then appendFile "error.log" logEntryProcessed
	else appendFile "access.log" logEntryProcessed
	
-- | Remove unnecessary characters from the string
removeChars :: String -> String -> String
removeChars = filter . flip notElem