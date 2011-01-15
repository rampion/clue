{-# OPTIONS_GHC -fno-warn-orphans #-}
module Clue.Player.Naive.Tests where
import Test.Framework
import Control.Monad (liftM3, liftM)
import Data.List (nub)

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

prop_eliminate_decreases_options :: Card -> Naive -> Property
prop_eliminate_decreases_options c n =
  property $ possibilites n >= possibilites (eliminate c n)
  where possibilites (Naive ss rs ws) = length ss + length rs + length ws

prop_eliminate_is_idemponent :: Card -> Naive -> Property
prop_eliminate_is_idemponent c n =
  property $ (eliminate c n) == (eliminate c $ eliminate c n)

