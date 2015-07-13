{-# LANGUAGE OverloadedStrings #-}

module Logger ( LogLevels(..), writeLog ) where

import           Data.Time

-- | Constructor for the Log levels 
data LogLevels = ERROR | DEBUG | INFO deriving (Show, Eq)

-- {
-- Constructor for the Log entry = (LogEntry LogLevels logging logInputData logMessage)
-- logLevel     -> Logging purpose (ERROR/INFO/DEBUG)
-- loggingMethod-> Module of the program which writes the log
-- logInputData -> Input data at the point of logging
-- logMessage   -> Output message of the method
-- } 
data Logger = LogEntry LogLevels String String String deriving (Show, Eq)

toString :: Logger -> IO String 
toString (LogEntry lLevel lMethod lInputData lMessage) = do 
  utcTime <- getCurrentTime
  let myTime = addUTCTime 19800 utcTime
  return $ (show myTime) ++ ", [" ++ show lLevel ++ "], [" ++ lMethod ++ "], Input: " ++ lInputData ++ ", " ++ lMessage
  
-- {
-- Append the log entry to an external log file
-- Error logs to 'error.log' file
-- Other logs to 'access.log' file
-- }
writeLog :: LogLevels -> String -> String -> String -> IO ()
writeLog lLevel lMethod lInputData lMessage = do
  logEntry <- toString (LogEntry lLevel lMethod lInputData lMessage)
  let logEntryProcessed = logEntry ++ "\n"
  if lLevel == ERROR 
    then appendFile "error.log" logEntryProcessed
	else appendFile "access.log" logEntryProcessed