module Main where
import Test.Framework
import System.Environment (getArgs)

import Clue.Cards.Tests

main :: IO ()
main = do
  args <- getArgs
  runTestWithArgs args Clue.Cards.Tests.allHTFTests
