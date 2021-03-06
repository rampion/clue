{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}
module Clue.Player.Naive where

import Clue.Cards
import Clue.Player
import Control.Monad.State
import Data.List ((\\))

data Naive = Naive { whos :: [Suspect], wheres :: [Room], hows :: [Weapon] }
  deriving (Show, Eq)

eliminate :: Card -> Naive -> Naive
eliminate (SuspectCard s) n = n { whos = whos n \\ [s] }
eliminate (WeaponCard w) n = n { hows = hows n \\ [w] }
eliminate (RoomCard r) n = n { wheres = wheres n \\ [r] }

instance (Monad m) => Player Naive m where
  dealIn _ _ = put . foldr eliminate (Naive suspects rooms weapons)
  notify (RevealCard _ c) = modify $ eliminate c
  notify _ = return ()
  suggest = do
    r:_ <- gets wheres
    s:_ <- gets whos
    w:_ <- gets hows
    return $ Just $ Scenario r s w
  accuse = do
    r:rs <- gets wheres
    s:ss <- gets whos
    w:ws <- gets hows
    return $ if null rs && null ss && null ws then Just $ Scenario r s w else Nothing
  reveal (_, Scenario r s w) = do
    showWhere <- gets (elem r . wheres)
    showWho <- gets (elem s . whos)
    return $  if showWhere        then RoomCard r
                else if showWho   then SuspectCard s
                                  else WeaponCard w
