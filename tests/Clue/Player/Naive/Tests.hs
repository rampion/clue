{-# OPTIONS_GHC -fno-warn-orphans #-}
module Clue.Player.Naive.Tests where
import Test.Framework
import Test.HUnit.Base ((@?=))

import Control.Monad.Identity
import Control.Monad.State
import Data.List (nub, intersect)

import Clue.Player
import Clue.Player.Naive
import Clue.Cards hiding (elements)

instance Arbitrary Suspect where arbitrary = elements suspects
instance Arbitrary Room where arbitrary = elements rooms
instance Arbitrary Weapon where arbitrary = elements weapons
instance Arbitrary Card where arbitrary = elements cards

instance Arbitrary Naive where
  arbitrary = liftM3 Naive set set set
    where set :: (Arbitrary a, Eq a) => Gen [a]
          set = liftM nub arbitrary

prop_eliminate_removes_options :: Card -> Naive -> Property
prop_eliminate_removes_options c n@(Naive ss rs ws) =
  property $ ss' `isSubsetOf` ss && rs' `isSubsetOf` rs && ws' `isSubsetOf` ws
  where isSubsetOf as bs = (as `intersect` bs) == as
        (Naive ss' rs' ws') = eliminate c n

prop_eliminate_removes_at_most_1 :: Card -> Naive -> Property
prop_eliminate_removes_at_most_1 c n@(Naive ss rs ws) = 
  property $ 0 <= diffsize && diffsize <= 1
  where (Naive ss' rs' ws') = eliminate c n
        diffsize = length ss + length rs + length ws - length ss' - length rs' - length ws'

prop_eliminate_is_idemponent :: Card -> Naive -> Property
prop_eliminate_is_idemponent c n =
  property $ (eliminate c n) == (eliminate c $ eliminate c n)

runNaive :: StateT Naive Identity a -> Naive -> (a, Naive)
runNaive st n = runIdentity $ runStateT st n

test_dealIn_nothing :: Assertion
test_dealIn_nothing = 
  ((),Naive suspects rooms weapons) @?= runNaive (dealIn __ __ []) __
  where __ :: a
        __ = undefined
