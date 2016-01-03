{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1
-- Parses an individual line from the log file. For example,
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage [] = LogMessage Unknown unknownMessage
parseMessage message
    | ["E"] <- take 1 $ words message                 = LogMessage errorMessageType timeStamp (unwords xs)
    | (type:thetime:xs) <- words message            = LogMessage messageType thetime (unwords xs)
    | otherwise                                     = LogMessage Unknown unknownMessage
        where messageType = getMessageType type
              errorMessageType = getErrorMessageType (take 2 $ words message)

unknownMessage :: String
unknownMessage = "This is not in the right format"

getErrorMessageType:: [String] -> MessageType
getErrorMessageType s = case s of
                ("E":num) -> Error num 
                _         -> Error -1

getMessageType :: String -> MessageType
getMessageType m = case m of
                     "I" -> Info
                     "W" -> Warning
                     _   -> Error -1
