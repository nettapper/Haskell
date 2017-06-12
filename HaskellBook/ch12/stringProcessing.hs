-- example GHCi session above the functions
--
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe s = if s == "the"
              then Nothing
              else Just s

replaceThe :: String -> String
replaceThe s = unwords $ map nothingToA $ map notThe $ words s
  where nothingToA (Just s)  = s
        nothingToA Nothing = "a"
