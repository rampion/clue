module Main where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test
import Clue.Cards
import Control.Monad (liftM)

creates_n_lists :: Int -> [Int] -> Property
creates_n_lists n as = 
  n >= 0 ==> n == length (deal n as)

main :: IO ()
main = quickCheck creates_n_lists
