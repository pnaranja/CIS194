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
-- The function "lines" splits the string from endline characters
parse :: String -> [LogMessage]
parse s =  map parseMessage (lines s)

-- Exercise 2
-- Insert LogMessage into (an assumed sorted) MessageTree
-- Don't insert Unknown LogMessages
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ timestamp1 _) tree@(Node mtree1 tmessage@(LogMessage _ timestamp2 _) mtree2)
    |  timestamp1>timestamp2     =   Node mtree1 tmessage (insert message mtree2) 
    |  timestamp1<timestamp2     =   Node (insert message mtree1) tmessage mtree2 
    |  otherwise                 =   tree

-- Exercise 3
-- Build a Message Tree from a list of LogMessages
build :: [LogMessage] -> MessageTree
build mlist = foldr insert Leaf mlist
