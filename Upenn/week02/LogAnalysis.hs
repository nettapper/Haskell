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

build :: [LogMessage] -> MessageTree
build lms = build' lms Leaf

build' :: [LogMessage] -> MessageTree -> MessageTree
build' ls mt = foldl (flip insert) mt ls
-- build' [] mt = mt
-- build' (l:ls) mt = build' ls $ insert l mt

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = inOrder l ++ [lm] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lm = map getString $ filter test $ inOrder $ build lm
 where
     test (LogMessage (Error i) _ _) = i >= 50
     test _ = False
     getString (LogMessage _ _ s) = s
     getString (Unknown s) = s
