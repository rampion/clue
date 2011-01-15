module Clue.Cards.Tests where
import Test.Framework

import Clue.Cards

import Data.List (transpose)

data Undealt = Undealt Int String
  deriving Show
instance Arbitrary Undealt where
  arbitrary = do
    deck <- arbitrary
    n    <- choose (1, 2 * length deck)
    return $ Undealt n deck

prop_creates_n_lists :: Undealt -> Property
prop_creates_n_lists (Undealt n as) = property $ n == length (deal n as)

prop_distributes_cards :: Undealt -> Property
prop_distributes_cards (Undealt n as) = property $ as == concat (transpose $ deal n as)
