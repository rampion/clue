module Clue.Cards.Tests where
import Test.Framework

import Clue.Cards

import Data.List (transpose, sort)
import System.Random (mkStdGen)
import Control.Monad.State (runStateT)
import Control.Monad.Identity (runIdentity)

runShuffle :: Int -> [a] -> [a]
runShuffle i as = fst $ runIdentity $ shuffle as `runStateT` mkStdGen i
    
prop_shuffle_returns_permutation :: Int -> String -> Property
prop_shuffle_returns_permutation i as =
  property $ sort as == sort (runShuffle i as)

prop_shuffle_depends_on_gen :: Int -> String-> Property
prop_shuffle_depends_on_gen g s = 
  property $ any id [ runShuffle g' as /= as' | g' <- [0..] ]
    where as = [ 0 .. length s + 1 ]
          as' = runShuffle g as

forAllDeals :: [a] -> (Int -> Property) -> Property
forAllDeals s f = forAll (choose (1, 2 * length s)) f

prop_deal_creates_n_lists :: String -> Property
prop_deal_creates_n_lists s = forAllDeals s $ \n -> 
  property $ n == length (deal n s)

prop_deal_distributes_cards :: String -> Property
prop_deal_distributes_cards s = forAllDeals s $ \n -> 
  property $ s == concat (transpose $ deal n s)
