module Main where
import Test.Framework
import System.Environment (getArgs)

import Clue.Cards.Tests
import Clue.Player.Naive.Tests

tests :: TestSuite
tests = makeAnonTestSuite $ map testSuiteAsTest 
          [ Clue.Cards.Tests.allHTFTests
          , Clue.Player.Naive.Tests.allHTFTests
          ]

main :: IO ()
main = getArgs >>= flip runTestWithArgs tests
