{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, Rank2Types, FlexibleInstances, FlexibleContexts, RankNTypes #-}
module Clue.Game where
{- This module simulates a simplified game of Clue (or Cluedo)
 - using the playgame function to mediate a game between
 - supplied players.
-}
import Control.Monad (liftM, unless)
import Control.Monad.State
import Control.Monad.Trans (MonadIO)
import Data.List ((\\), find, intersect)
import Data.Maybe (isNothing)
import System.Random (Random, RandomGen, randomR)

-- list all the elements in a particular enum
elements :: Enum a => [a]
elements = enumFrom $ toEnum 0

-- possible murder locations
data Room = Kitchen | Ballroom | Conservatory | 
            DiningRoom | BilliardRoom | Library | 
            Lounge | Hall | Study
  deriving (Enum, Eq, Show, Read)
rooms :: [Room]
rooms = elements

-- possible murderers
data Suspect =  ColMustard | MissScarlet | MrGreen | 
                MrsPeacock | MrsWhite | ProfPlum
  deriving (Enum, Eq, Show, Read)
suspects :: [Suspect]
suspects = elements

-- possible murder weapons
data Weapon = Candlestick | Knife | LeadPipe | 
              Revolver | Rope | Wrench
  deriving (Enum, Eq, Show, Read)
weapons :: [Weapon]
weapons = elements

-- cards are the clues that players use to learn
-- what really happened
data Card = Room Room | Suspect Suspect | Weapon Weapon
  deriving (Eq, Show, Read)
cards :: [Card]
cards = map Room rooms ++ map Suspect suspects ++ map Weapon weapons

-- a possible murder scenario
data Scenario = Scenario { getWhere :: Room, getWho :: Suspect, getHow :: Weapon }
  deriving (Eq, Show, Read)

type TotalPlayers = Int
type PlayerPosition = Int

-- events are broadcast whenever a player does something
-- that other players should know about
data Event  = Suggestion PlayerPosition Scenario        -- someone makes a suggestion
            | RevealSomething PlayerPosition            -- someone reveals an unknown card to the suggestor
            | RevealCard PlayerPosition Card            -- someone reveals a card to you
            | WinningAccusation PlayerPosition Scenario -- someone makes a winning accusation
            | LosingAccusation PlayerPosition Scenario  -- someone makes a losing accusation

class Player p m where
  -- let them know what another player does
  notify :: Event -> p -> m p

  -- ask them to make a suggestion
  suggest :: p -> m (Maybe Scenario, p)

  -- ask them to make an accusation
  accuse :: p -> m (Maybe Scenario, p)

  -- ask them to reveal a card to invalidate a suggestion
  reveal :: (PlayerPosition, Scenario) -> p -> m (Card, p)

-- wrapper for generating a player within a given monad
data MkPlayer m = forall p. Player p m => MkPlayer (TotalPlayers -> PlayerPosition -> [Card] -> m p)

-- wrapper for storing a player within a given monad
data WpPlayer m = forall p. Player p m => WpPlayer p

-- state about a player
data PlayerInfo m = PlayerInfo  { agent :: WpPlayer m
                                , position :: PlayerPosition
                                , hand :: [Card]
                                , lost :: Bool
                                , cheated :: Bool }

-- make PlayerInfo into an instance of Player
data Action m b = Action (forall p. Player p m => p -> m (b, p))
wrap :: (Monad m) => Action m b -> (PlayerInfo m) -> m (b, PlayerInfo m)
wrap (Action a) (pi@PlayerInfo { agent = WpPlayer p }) = do
  (b, p) <- a p
  return $ (b, pi { agent = WpPlayer p })

instance Monad m => Player (PlayerInfo m) m where
  notify ev (pi@PlayerInfo { agent = WpPlayer p }) = do
    p <- notify ev p
    return $ pi { agent = WpPlayer p }
  suggest   = wrap $ Action $ suggest
  accuse    = wrap $ Action $ accuse
  reveal sg = wrap $ Action $ reveal sg

type Game m = [PlayerInfo m]

gen :: (Random a, RandomGen g, Monad m) => (a,a) -> StateT g m a
gen (lo,hi) = do
  g <- get
  let (i, g') = randomR (lo, hi) g
  put g'
  return i

-- draw a random member of a list
draw :: (RandomGen g, Monad m) => [a] -> StateT g m [a]
draw as = do
  i <- gen (0, length as - 1)
  let (before, a : after) = splitAt i as
  return (a:before ++ after)

-- shuffle the elements of a list into random order
-- using http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
shuffle :: (RandomGen g, Monad m) => [a] -> StateT g m [a]
shuffle [] = return []
shuffle as = do
  (a:as) <- draw as
  liftM (a:) $ shuffle as

-- deal out the given cards to n players
deal :: TotalPlayers -> [a] -> [[a]]
deal n [] = replicate n []
deal n as = zipWith (:) cs $ deal n as'
  where (cs, as') = splitAt n as

-- use the callback to initialize player state
create :: (Monad m) => TotalPlayers -> PlayerPosition -> [Card] -> MkPlayer m -> m (PlayerInfo m)
create n i cs (MkPlayer f) = f n i cs >>= \p -> return $ PlayerInfo (WpPlayer p) i cs False False

createAll :: (Monad m) => TotalPlayers -> [[Card]] -> [MkPlayer m] -> m [PlayerInfo m]
createAll n = (sequence .) . zipWith3 (create n) [0..]

putHead :: (Monad m) => PlayerInfo m -> StateT (Game m) m ()
putHead p = modify $ (p:) . tail

putTail :: (Monad m) => [PlayerInfo m] -> StateT (Game m) m ()
putTail ps = modify $ (:ps) . head

-- play a random game, using the given methods to generate players
setup :: (RandomGen g, Monad m) => [MkPlayer (StateT g m)] -> StateT g m (Maybe (Scenario, Game (StateT g m)))
setup ms = do
  let n = length ms
  if length cards `mod` n /= 3 `mod` n
    then return Nothing
    else do
      (killer:_)  <- draw suspects
      (weapon:_)  <- draw weapons
      (room:_)    <- draw rooms
      let deck    = cards \\ [ Suspect killer, Weapon weapon, Room room ]
      ps          <- shuffle deck >>= flip (createAll n) ms . deal n 
      return $ Just (Scenario room killer weapon, ps)
