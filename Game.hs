{-# LANGUAGE ExistentialQuantification #-}
module Clue.Game where

elements :: Enum a => [a]
elements = enumFrom $ toEnum 0

data Room = Kitchen | Ballroom | Conservatory | 
            DiningRoom | BilliardRoom | Library | 
            Lounge | Hall | Study
  deriving (Enum, Eq, Show, Read)
rooms :: [Room]
rooms = elements

data Suspect =  ColMustard | MissScarlet | MrGreen | 
                MrsPeacock | MrsWhite | ProfPlum
  deriving (Enum, Eq, Show, Read)
suspects :: [Suspect]
suspects = elements

data Weapon = Candlestick | Knife | LeadPipe | 
              Revolver | Rope | Wrench
  deriving (Enum, Eq, Show, Read)
weapons :: [Weapon]
weapons = elements

data Card = Room Room | Suspect Suspect | Weapon Weapon
  deriving (Eq, Show, Read)

cards :: [Card]
cards = map Room rooms ++ map Suspect suspects ++ map Weapon weapons

data Scenario = Scenario { getWhere :: Room, getWho :: Suspect, getHow :: Weapon }

type TotalPlayers = Int
type PlayerPosition = Int


{-
 - return
 -  State Monad :: for StdGen (random number generator)
 -  Writer Monad :: for game log
 -  Maybe PlayerPosition :: 
game players =
  -- choose the true scenario and remove those cards
  -- deal out the remaining cards to each player
  -- allow each player to go until one player wins (or all lose)
    -- on a player's turn, 
      -- a player may make a suggestion
        -- other players each get a chance to rebut, if they can
      -- a player may make an accusation
        -- if true, they win.
        -- if false, they may no longer take turns 
  -- return a log of gameplay
turn =~ /(suggest pass* reveal?)? accuse?/
if you try to reveal an impossible card (you don't carry it, it's not relevent),
you're marked as a cheat
  - cheats can no longer take turns
  - cheats don't get to choose which card to reveal
-}

data Player = forall p. IOPlayer p => Player p
data Event  = Proposition PlayerPosition Scenario
            | WinningAccusation PlayerPosition Scenario
            | LosingAccusation PlayerPosition Scenario
            | Reveal PlayerPosition

-- deal this player in
type MkPlayer = TotalPlayers -> PlayerPosition -> [Card] -> IO Player

class IOPlayer a where
  -- let them know what another player does
  update :: a -> Event -> IO a
  update = (return .). const

  -- ask them to make a suggestion
  suggest :: a -> IO (a, Maybe Scenario)

  -- ask them to make an accusation
  accuse :: a -> IO (a, Maybe Scenario)

  -- ask them to reveal a card to invalidate a suggestion
  reveal :: a -> (PlayerPosition, Scenario) -> IO (a, Card)
