module Main where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test
import Clue.Cards

import Data.List (transpose)

data Undealt = Undealt Int String
  deriving Show
instance Arbitrary Undealt where
  arbitrary = do
    deck <- arbitrary
    n    <- choose (1, 2 * length deck)
    return $ Undealt n deck

creates_n_lists :: Undealt -> Property
creates_n_lists (Undealt n as) = property $ n == length (deal n as)

distributes_cards :: Undealt -> Property
distributes_cards (Undealt n as) = property $ as == concat (transpose $ deal n as)

main :: IO ()
main = do
  quickCheck creates_n_lists
  quickCheck distributes_cards
