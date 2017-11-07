import Data.Monoid
import Listy

instance Monoid (Listy a) where
  mempty = []
  mappend (Listy l) (Listy l') = Listy $ mappend l l'
