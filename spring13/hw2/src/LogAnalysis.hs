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
insert _ (Node mtree1 (Unknown s) mtree2) =  Node mtree1 (Unknown s) mtree2 -- This should never happen since MessageTrees don't have Unknowns

-- Exercise 3
-- Build a Message Tree from a list of LogMessages
-- Using foldr since we're applying the first argument of insert from the mlist
build :: [LogMessage] -> MessageTree
build mlist = foldr insert Leaf mlist

-- Exercise 4
-- Produce a sorted list of LogMessages from a SORTED MessageTree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node mtree1 message mtree2) = inOrder mtree1 ++ message : inOrder mtree2

-- Exercise 5
-- Extract errors with a severity of >= 50
-- Take an UNSORTED list of Log Message and return a list of Strings
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong message = map getMessage (filter filterSevere message)

filterSevere :: LogMessage -> Bool
filterSevere (LogMessage (Error num) _ _)
    |   num >= 50                           = True
    |   otherwise                           = False
filterSevere _                              = False

getMessage :: LogMessage -> String
getMessage (Unknown s) = s
getMessage (LogMessage _ _ s) = s

