module Main where

import Cards
import Game
import Naive
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

  
