module ExploringQuickcheck where

import Test.QuickCheck

trivialInt :: Gen Int
trivialInt = return 1

trivialSample :: IO [Int]
trivialSample = sample' trivialInt

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

trivialOneThree :: IO [Int]
trivialOneThree = sample' oneThroughThree

oneThroughThreeNonUniformProb :: Gen Int
oneThroughThreeNonUniformProb = elements [1,2,2,2,2,2,2,2,3]  -- 2 is a litte bit more likely than the other val

trivialOneThreeNonUniformProb :: IO [Int]
trivialOneThreeNonUniformProb = sample' oneThroughThreeNonUniformProb

genBool :: Gen Bool
       -- choose :: System.Random.Random a => (a, a) -> Gen a
genBool = choose (True, False)

genBool' :: Gen Bool
        -- elements :: [a] -> Gen a
genBool' = elements [True, False]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a,b,c)

sampleGenTuple :: IO ()
sampleGenTuple = sample (genTuple :: Gen (Int, Char))

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Right a, Left b]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Just a, Nothing]

genMaybe' :: (Arbitrary a) => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))
            ]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_additionGreaterUntrueProperty :: Int -> Bool
prop_additionGreaterUntrueProperty x = x + x > x  -- 0 + 0 /> 0

runQuickCheckWithOutHspec :: IO ()
runQuickCheckWithOutHspec = do
  quickCheck prop_additionGreater
  putStrLn "This next test should fail:"
  quickCheck prop_additionGreaterUntrueProperty

