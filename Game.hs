{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, Rank2Types, FlexibleInstances, FlexibleContexts, RankNTypes #-}
module Game where
{- This module simulates a simplified game of Clue (or Cluedo)
 - using the playgame function to mediate a game between
 - supplied players.
-}
import Control.Monad (liftM, unless)
import Control.Monad.State
import Control.Monad.Trans (MonadIO)
import Data.List ((\\), find, intersect)
import Data.Maybe (isNothing)
import System.Random (RandomGen)

import Cards

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
  notify :: Event -> StateT p m ()

  -- ask them to make a suggestion
  suggest :: StateT p m (Maybe Scenario)

  -- ask them to make an accusation
  accuse :: StateT p m (Maybe Scenario)

  -- ask them to reveal a card to invalidate a suggestion
  reveal :: (PlayerPosition, Scenario) -> StateT p m Card

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
data Action m b = Action (forall p. Player p m => StateT p m b)
wrap :: (Monad m) => Action m b -> StateT (PlayerInfo m) m b
wrap (Action a) = StateT $ \(pi@PlayerInfo { agent = WpPlayer p }) -> do
  (b, p) <- runStateT a p
  return (b, pi { agent = WpPlayer p })

instance Monad m => Player (PlayerInfo m) m where
  notify ev = wrap $ Action $ notify ev
  suggest   = wrap $ Action $ suggest
  accuse    = wrap $ Action $ accuse
  reveal sg = wrap $ Action $ reveal sg

type Game m = [PlayerInfo m]

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

-- use the callback to initialize player state
create :: (Monad m) => TotalPlayers -> PlayerPosition -> [Card] -> MkPlayer m -> m (PlayerInfo m)
create n i cs (MkPlayer f) = f n i cs >>= \p -> return $ PlayerInfo (WpPlayer p) i cs False False

createAll :: (Monad m) => TotalPlayers -> [[Card]] -> [MkPlayer m] -> m [PlayerInfo m]
createAll n = (sequence .) . zipWith3 (create n) [0..]

onHead :: Monad m => (PlayerInfo m -> StateT (Game m) m (a, PlayerInfo m)) -> StateT (Game m) m a
onHead f = do
  (a, pi) <- get >>= f . head 
  modify $ (pi:) . tail
  return a

onTail :: Monad m => (PlayerInfo m -> StateT (Game m) m (a, PlayerInfo m)) -> StateT (Game m) m [a]
onTail f = do
  (as, pis) <- get >>= liftM unzip . mapM f
  modify $ (:pis) . head
  return as


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
      let deck    = cards \\ [ SuspectCard killer, WeaponCard weapon, RoomCard room ]
      ps          <- shuffle deck >>= flip (createAll n) ms . deal n 
      return $ Just (Scenario room killer weapon, ps)

{-
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
-}
