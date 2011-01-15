module Main where
import Clue.Cards.Tests
import Test.QuickCheck.Test

main :: IO ()
main = do
  mapM_ quickCheck clue_card_props
