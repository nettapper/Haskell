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

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
-- countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = sum $ map f zs
    where ws = words s
          zs = zip (map notThe ws) (tail ws)
          f (a,b) = case a of
                         Nothing -> if startsWithVowel b then 1 else 0
                         otherwise -> 0

startsWithVowel :: String -> Bool
startsWithVowel (s:_) = case s of
                             'a' -> True
                             'e' -> True
                             'i' -> True
                             'o' -> True
                             'u' -> True
                             otherwise -> False

