module Listy where

import Data.Monoid

newtype Listy a =
  Listy [a]
  deriving (Eq, Show)

-- instance Monoid (Listy a) where
--   mempty = Listy []
--   mappend (Listy l) (Listy l') = Listy $ mappend l l'
--
-- if this is in this file we will get the following error
--
-- Listy.hs:9:10: error:
-- Duplicate instance declarations:
--   instance [safe] Monoid (Listy a) -- Defined at Listy.hs:9:10
--   instance Monoid (Listy a) -- Defined at ListyInstances.hs:4:10
