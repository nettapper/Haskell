{-
1 Problem 1
(*) Find the last element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
-}
{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

myLast [x] = x
myLast (x:xs) = myLast xs

-- here we specify that 'myLast' should return exactly the same result
-- as 'last' for any given 'xs'
prop_myLast xs = length xs > 0 ==> myLast xs == last xs


return [] -- need this for GHC 7.8
-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
