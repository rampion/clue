{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
module Clue.Player where
import Control.Monad.State
import Clue.Cards

type TotalPlayers = Int
type PlayerPosition = Int

-- events are broadcast whenever a player does something
-- that other players should know about
data Event  = Suggestion PlayerPosition Scenario        -- someone makes a suggestion
            | RevealSomething PlayerPosition            -- someone reveals an unknown card to the suggestor
            | RevealCard PlayerPosition Card            -- someone reveals a card to you
            | WinningAccusation PlayerPosition Scenario -- someone makes a winning accusation
            | LosingAccusation PlayerPosition Scenario  -- someone makes a losing accusation
  deriving (Show, Eq)

class Player p m where
  -- deal them in to a particular game
  dealIn :: TotalPlayers -> PlayerPosition -> [Card] -> StateT p m ()

  -- let them know what another player does
  notify :: Event -> StateT p m ()

  -- ask them to make a suggestion
  suggest :: StateT p m (Maybe Scenario)

  -- ask them to make an accusation
  accuse :: StateT p m (Maybe Scenario)

  -- ask them to reveal a card to invalidate a suggestion
  reveal :: (PlayerPosition, Scenario) -> StateT p m Card

-- wrapper for storing a player within a given monad
data WpPlayer m = forall p. Player p m => WpPlayer p
