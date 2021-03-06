module Main where

import Clue.Game
import Clue.Player
import Clue.Player.Naive
import Control.Monad.State
import Random

stooges :: Monad m => [WpPlayer m]
stooges = replicate 3 (WpPlayer (undefined :: Naive))

main :: IO ()
main = do
  gen <- newStdGen
  (((mi,ls),_),_) <- play `runStateT` stooges `runStateT` gen
  print mi
  mapM_ print ls

  
