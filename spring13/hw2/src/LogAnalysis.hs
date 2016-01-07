{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1
-- Parses an individual line from the log file. For example,
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage message = parseMessage' $ words message

parseMessage' :: [String] -> LogMessage
parseMessage' ("E":errornum:timestamp:xs) = LogMessage (Error (read errornum :: Int)) (read timestamp :: Int) (unwords xs)
parseMessage' ("I":timestamp:xs) = LogMessage Info (read timestamp :: Int) (unwords xs)
parseMessage' ("W":timestamp:xs) = LogMessage Warning (read timestamp :: Int) (unwords xs)
parseMessage' _ = Unknown "This is not in the right format"

-- Assume testParse function opens the file to read - Don't have to deal with IO Monad
parse :: String -> [LogMessage]
parse s =  map parseMessage (lines s)
