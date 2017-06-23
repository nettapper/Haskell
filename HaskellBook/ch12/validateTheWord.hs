import Data.List (intersect)

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

-- If the # vowels > # consonants then return Nothing
mkWord :: String -> Maybe Word'
mkWord s = if v > c then Nothing else Just (Word' s)
  where v = numVowels s
        c = numConsonants s

numVowels :: String -> Integer
numVowels = toInteger . length . flip myIntersect vowels

-- I'm not sure if it's cheating to use intersect from Data.List
numVowels' :: String -> Integer
numVowels' = toInteger . length . flip intersect vowels

numConsonants :: String -> Integer
numConsonants s = toInteger total - numVowels s
  where total = length s

myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect xs ys = [x | x <- xs, x `isIn` ys]

isIn :: Eq a => a -> [a] -> Bool
isIn _ [] = False
isIn x (y:ys) = x == y || isIn x ys

-- Needed to make GhcMod work!
main :: IO()
main = return ()
