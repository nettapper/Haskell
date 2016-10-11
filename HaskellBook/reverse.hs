module Reverse where

-- this should only work for a string with 3 words and should be written with take and drop only
rvrs :: String -> String
-- rvrs s = last ++ middle ++ first
rvrs s = concat [last, " ", middle, " ", first]
  where
      ws = words s
      last   = head $ take 1 $ drop 2 ws
      middle = head $ take 1 $ drop 1 ws
      first  = head $ take 1 ws

