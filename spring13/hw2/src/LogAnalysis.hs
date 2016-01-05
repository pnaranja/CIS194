{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

--Need to run "stack install split"
import Data.List.Split

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

parse :: String -> [LogMessage]
parse filename =  do
                    contents <- readFile filename
                    ret <- map parseMessage  (Data.List.Split.splitOn "\n" contents)
                    return ret
