{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Clue.Game where
{- This module simulates a simplified game of Clue (or Cluedo)
 - using the playgame function to mediate a game between
 - supplied players.
-}
import Control.Monad (liftM, unless)
import Control.Monad.State
import Control.Monad.Trans (MonadIO)
import System.Random (randomRIO)
import Data.List ((\\), find, intersect)
import Data.Maybe (isNothing)

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

class Player p where
  -- let them know what another player does
  update :: (MonadIO io) => Event -> StateT p io ()

  -- ask them to make a suggestion
  suggest :: (MonadIO io) => StateT p io (Maybe Scenario)

  -- ask them to make an accusation
  accuse :: (MonadIO io) => StateT p io (Maybe Scenario)

  -- ask them to reveal a card to invalidate a suggestion
  reveal :: (MonadIO io) => (PlayerPosition, Scenario) -> StateT p io Card

-- wrapper for generating a player
data MkPlayer = forall p. Player p => MkPlayer (TotalPlayers -> PlayerPosition -> [Card] -> IO p)

-- state about a player
data PlayerInfo = PlayerInfo  { agent :: WpPlayer
                              , position :: PlayerPosition
                              , hand :: [Card]
                              , lost :: Bool
                              , cheated :: Bool }
data WpPlayer = forall p. Player p => WpPlayer p

-- make PlayerInfo into an instance of Player
wrap :: (MonadIO io) => Action io b -> StateT PlayerInfo io b
wrap (Action a) = StateT $ \(pi@PlayerInfo { agent = WpPlayer p }) -> do
  (b, p) <- runStateT a p
  return $ (b, pi { agent = WpPlayer p })
data Action io b = Action (forall p. Player p => StateT p io b)

instance Player PlayerInfo where
  update ev = wrap $ Action $ update ev
  suggest   = wrap $ Action $ suggest
  accuse    = wrap $ Action $ accuse
  reveal sg = wrap $ Action $ reveal sg

-- tell a given player about some event
notify :: Event -> PlayerInfo -> StateT Game IO PlayerInfo
notify e = liftM snd . runStateT (update e)

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

-- deal out the given cards to n players
deal :: TotalPlayers -> [a] -> [[a]]
deal n [] = replicate n []
deal n as = zipWith (:) cs $ deal n as'
  where (cs, as') = splitAt n as

-- use the callback to inialize player state
create :: MkPlayer -> TotalPlayers -> PlayerPosition -> [Card] -> IO PlayerInfo
create (MkPlayer f) n i cs = f n i cs >>= \p -> return $ PlayerInfo (WpPlayer p) i cs False False

type Game = [PlayerInfo]

putHead :: PlayerInfo -> StateT Game IO ()
putHead p = modify $ (p:) . tail

putTail :: [PlayerInfo] -> StateT Game IO ()
putTail ps = modify $ (:ps) . head

-- let the next player take a turn
turn :: Scenario -> StateT Game IO (Maybe PlayerPosition)
turn secret = do
  -- see if they have a suggestion
  (suggestion, p) <- liftM head get >>= runStateT suggest
  putHead p

  unless (isNothing suggestion) $ do
    let (Just scenario) = suggestion

    -- broadcast the suggestion
    get >>= mapM (notify $ Suggestion (position p) scenario) >>= put
    
    -- find the first one who can refute the scenario
    let cs = map ($scenario) [ Suspect . getWho, Room . getWhere, Weapon . getHow ] 
    (qs,rss) <- get >>= return . break (not . null . intersect cs . hand) . tail

    unless (null rss) $ do
      let refuter:ss = rss
      let valid = intersect cs $ hand refuter

      (shown, refuter) <- if cheated refuter 
                            -- if the refuter is a cheater, just choose for him
                            then return (head valid, refuter)
                            else do 
                              (shown, refuter) <- runStateT (reveal (position p, scenario)) refuter
                              -- make sure they don't cheat
                              return $ if shown `elem` valid
                                          then (shown, refuter)
                                          else (head valid, refuter { cheated = True, lost = True })

      notify (RevealCard (position refuter) shown) p >>= putHead
      mapM (notify $ RevealSomething (position refuter)) (qs ++ refuter:ss) >>= putTail

  -- see if they have an accusation
  (accusation, p) <- liftM head get >>= runStateT accuse
  putHead p

  case accusation of
    Just scenario -> do
      -- see if they won or lost
      let won = scenario == secret
      putHead $ p { lost = not won }
      
      -- tell everyone whether they won or lost
      let ev  = (if won then LosingAccusation else WinningAccusation) (position p) scenario
      get >>= mapM (notify ev) >>= put
      
      if won
        then return $ Just $ position p
        else do
          p:ps <- get
          let (qs,rss) = break (not . lost) ps
          put (rss ++ p:qs)
          if null rss
            -- quit if everyone else has failed
            then return Nothing
            -- otherwise let the next non-loser play
            else turn secret
    Nothing -> do
      p:ps <- get
      -- go around the table and choose the next player who hasn't lost
      let (qs,rss) = break (not . lost) ps
      put (rss ++ p:qs)
      turn secret

-- play a random game, using the given methods to generate players
playgame :: [MkPlayer] -> IO (Maybe PlayerPosition)
playgame ms = do
  let n = length ms
  if length cards `mod` n /= 3 `mod` n
    then return Nothing
    else do
      (killer,_)  <- draw suspects
      (weapon,_)  <- draw weapons
      (room,_)    <- draw rooms
      deck        <- shuffle $ cards \\ [ Suspect killer, Weapon weapon, Room room ]
      ps          <- sequence $ zipWith3 (flip create n) ms [0..] (deal n deck)
      liftM fst $ runStateT (turn $ Scenario room killer weapon) ps

