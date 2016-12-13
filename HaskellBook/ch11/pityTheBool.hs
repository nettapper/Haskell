import Data.Int

data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)

-- the cardinality is Big Bool + Small Bool
-- Bool = 1 + 1 = 2
-- Big = 1 + 1 = 2 and
-- Small = 1 + 1 = 2 therefore
-- BigSmall = 2 + 2 = 4




data NumberOrBool =
   Numba Int8
 | BoolyBool Bool
 deriving (Eq, Show)

-- the cardinality of NumberOrBool is Numba + BoolyBool
-- Numba = Int8 = 2^8 = 256
-- BoolyBool = 2 therefore
-- NumberOrBool = 258


-- n = Numba (-128)  -- complaints :(
-- can :set -XNegativeLiterals

-- or create it like this in ghci
-- let myNum = -128
-- let myNumba = Numba myNum

