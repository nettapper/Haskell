{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage s
  | first == "I" = LogMessage Info (read second::Int) (third ++ rest)
  | first == "W" = LogMessage Warning (read second::Int) (third ++ rest)
  | first == "E" = LogMessage (Error (read second::Int)) (read third::Int) rest
  | otherwise    = Unknown s
    where
        ss     = words s -- TODO trim whitespace
        first  = head ss
        second = ss !! 1
        third  = ss !! 2
        rest   = unwords $ drop 3 ss

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm@(LogMessage _ time _) mt = case mt of
                                          Leaf -> Node Leaf lm Leaf
                                          (Node l nodelm@(LogMessage _ nodeTime _) r) -> if time > nodeTime
                                                                                     then Node l nodelm (insert lm r)
                                                                                     else Node (insert lm l) nodelm r
                                          n@(Node _ (Unknown _) _) -> n
