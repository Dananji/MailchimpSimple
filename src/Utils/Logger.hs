{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Utils.Logger
-- License:     BSD3
-- Maintainer:  Dananji Liyanage <dan9131@gmail.com>
-- Stability:   experimental
--
-- Types and functions for Logging the activities

module Utils.Logger ( LogLevels(..)
                    , writeLog ) where

import           Data.Time
import           System.FilePath.Posix
import           System.Directory ( doesDirectoryExist , createDirectory )

-- | Constructor for the Log levels. 
data LogLevels = ERROR | DEBUG | INFO deriving (Show, Eq)

-- | Constructor for the Log entry = (LogEntry LogLevels logging logInputData logMessage)
--
-- logLevel     -> Logging purpose (ERROR/INFO/DEBUG)
-- loggingMethod-> Module of the program which writes the log
-- logInputData -> Input data at the point of logging
-- logMessage   -> Output message of the method
data Logger = LogEntry LogLevels String String String deriving (Show, Eq)

toString :: Logger -> IO String 
toString (LogEntry lLevel lMethod lInputData lMessage) = do 
  utcTime <- getCurrentTime
  let myTime = addUTCTime 19800 utcTime
  return $ show myTime ++ ", [" ++ show lLevel ++ "], [" ++ lMethod ++ "], Input: " ++ lInputData ++ ", " ++ lMessage
  
-- | This function appends a log entry to an external log file.
--
-- Error logs to 'error.log' file.
-- Other logs to 'access.log' file.
writeLog :: LogLevels -> String -> String -> String -> IO ()
writeLog lLevel lMethod lInputData lMessage = do
  logEntry <- toString (LogEntry lLevel lMethod lInputData lMessage)
  let logEntryProcessed = logEntry ++ "\n"
  exists <- doesDirectoryExist "log"
  if exists then return () else (createDirectory "log")
  if lLevel == ERROR 
    then appendFile ("log" ++ [pathSeparator] ++ "error.log") logEntryProcessed
    else appendFile ("log" ++ [pathSeparator] ++ "access.log") logEntryProcessed