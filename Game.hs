{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Clue.Game where
{-
 - return
 -  State Monad :: for StdGen (random number generator)
 -  Writer Monad :: for game log
 -  Maybe PlayerPosition :: winner (if any)
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
import Control.Monad (liftM, unless)
import Control.Monad.State
import System.Random (randomRIO)
import Data.List ((\\), find, intersect)

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
  deriving (Eq, Show, Read)

type TotalPlayers = Int
type PlayerPosition = Int



data Event  = Suggestion PlayerPosition Scenario
            | Reveal PlayerPosition
            | WinningAccusation PlayerPosition Scenario
            | LosingAccusation PlayerPosition Scenario

class Player p where
  -- let them know what another player does
  update :: Event -> StateT p IO ()

  -- ask them to make a suggestion
  suggest :: StateT p IO (Maybe Scenario)

  -- ask them to make an accusation
  accuse :: StateT p IO (Maybe Scenario)

  -- ask them to reveal a card to invalidate a suggestion
  reveal :: (PlayerPosition, Scenario) -> StateT p IO Card


data Agent = forall p. Player p => Agent p
type MkAgent = TotalPlayers -> PlayerPosition -> [Card] -> IO Agent
data PlayerInfo = PlayerInfo  { agent :: Agent
                              , position :: PlayerPosition
                              , hand :: [Card]
                              , lost :: Bool
                              , cheated :: Bool }
data Action b = Action (forall p. Player p => StateT p IO b)
wrap :: Action b -> StateT PlayerInfo IO b
wrap (Action a) = StateT $ \(pi@PlayerInfo { agent = Agent p }) -> do
  (b, p) <- runStateT a p
  return $ (b, pi { agent = Agent p })

instance Player PlayerInfo where
  update ev = wrap $ Action $ update ev
  suggest   = wrap $ Action $ suggest
  accuse    = wrap $ Action $ accuse
  reveal sg = wrap $ Action $ reveal sg
 
-- draw a random member of a list
draw :: [a] -> IO (a, [a])
draw as = do
  i <- randomRIO (0, length as - 1)
  let (before, a : after) = splitAt i as
  return (a, before ++ after)

-- shuffle the elements of a list into random order
-- using http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle as = do
  (a, as) <- draw as
  as <- shuffle as
  return (a : as)

deal :: TotalPlayers -> [a] -> [[a]]
deal n [] = replicate n []
deal n as = zipWith (:) cs $ deal n as'
  where (cs, as') = splitAt n as

create :: MkAgent -> TotalPlayers -> PlayerPosition -> [Card] -> IO PlayerInfo
create m n i cs = m n i cs >>= \a -> return $ PlayerInfo a i cs False False

{-
playturn :: TotalPlayers -> [PlayerInfo] -> IO (Maybe PlayerPosition)
playturn n (p:ps) = do
  -- see if they have a suggestion
  (p, suggestion) <- suggest p
  (p:ps) <- case suggestion of
    Nothing -> return (p:ps)
    Just scenario -> do
      -- notify the other players
      ps <- sequence $ map (`update` (Suggestion (position p) scenario)) ps
      let cs = map ($scenario) [ Suspect . getWho, Room . getWhere, Weapon . getHow ] 
      case find (not . null . intersect cs . hand) ps of _ -> return (p:ps) 
  return Nothing
-}

playgame :: [MkAgent] -> IO (Maybe PlayerPosition)
playgame ms = do
  let n = length ms
  if length cards `mod` n /= 3
    then return Nothing
    else do
      (killer,_)  <- draw suspects
      (weapon,_)  <- draw weapons
      (room,_)    <- draw rooms
      deck        <- shuffle $ cards \\ [ Suspect killer, Weapon weapon, Room room ]
      ps          <- sequence $ zipWith3 (flip create n) ms [0..] (deal n deck)
      return Nothing

