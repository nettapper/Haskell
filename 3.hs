-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143?

-- todo
-- Is Prime is very inefficient

-- Integral because isPrime should only deal with whole numbers
isPrime :: Integral a => a -> Bool
isPrime a = if a <= 1 then False else  -- Take care of case less or equal to 1
                if (length divEvenList) /= 0
                then False
                else True
                    where divEvenList = [x | x <- 2:[3,5..(a-1)], (mod a x) == 0]
                                        
-- Recursive isPrime
-- isPrimeV2 a = if a <= 1
              -- then False
              -- else ......

largestPrime a = maximum [x | x <- [1..a], isPrime x]
