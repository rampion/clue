{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, Rank2Types, FlexibleInstances, FlexibleContexts, RankNTypes, TupleSections #-}
module Game where
{- This module simulates a simplified game of Clue (or Cluedo)
 - using the playgame function to mediate a game between
 - supplied players.
-}
import Prelude hiding (log)
import Control.Monad (liftM, unless)
import Control.Monad.State
import Control.Monad.Writer
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
  dealIn t i cs     = wrap $ Action $ dealIn t i cs
  notify ev         = wrap $ Action $ notify ev
  suggest           = wrap $ Action $ suggest
  accuse            = wrap $ Action $ accuse
  reveal sg         = wrap $ Action $ reveal sg

data GameState m = GameState  { players :: [ PlayerInfo m ]
                              , winner  :: Maybe PlayerPosition
                              }
type Log = [ Event ]

data GameT m a = GameT { runGameT :: (Scenario, GameState m) -> m (a, GameState m, Log) }

instance MonadTrans GameT where
  lift m = GameT $ \(_,state) -> m >>= return . (,state,[])

instance Monad m => Monad (GameT m) where
  m >>= f = GameT $ \(secret,state) -> do
    (a, state, l) <- runGameT m (secret,state)
    (b, state, l') <- runGameT (f a) (secret,state)
    return (b, state, l ++ l')
  return = lift . return

secret :: Monad m => GameT m Scenario
secret = GameT $ \(secret,state) -> return (secret, state, [])

log :: Monad m => Event -> GameT m ()
log ev = GameT $ \(_,state) -> return ((), state, [ev])

getWinner :: Monad m => GameT m (Maybe PlayerPosition)
getWinner = GameT $ \(_,state) -> return (winner state, state, [])

setWinner :: Monad m => PlayerPosition -> GameT m ()
setWinner i = GameT $ \(_,state) -> return ((), state { winner = Just i }, [])

onPositions :: Monad m => StateT (PlayerInfo m) m a -> (Int, Int) -> GameT m [a]
onPositions st (lo,hi) = GameT $ \(_, state) -> do
  let ps = players state
  (as, ps) <- if lo < hi
                then do
                  let (xs, yzs) = splitAt lo ps
                  let (ys, zs) = splitAt (hi - lo) yzs
                  (as, ys') <- unzip `liftM` mapM (runStateT st) ps
                  return (as, xs ++ ys' ++ zs)
                else do
                  let (wxs, ys) = splitAt lo ps
                  let (ws, xs) = splitAt hi wxs
                  (as, ys') <- unzip `liftM` mapM (runStateT st) ys
                  (bs, ws') <- unzip `liftM` mapM (runStateT st) ws
                  return (as ++ bs, ws' ++ xs ++ ys')
  return (as, state { players = ps }, [])

onPosition :: Monad m => StateT (PlayerInfo m) m a -> Int -> GameT m a
onPosition st i = head `liftM` (st `onPositions` (i,i+1))

recordCheat :: Monad m => StateT (PlayerInfo m) m ()
recordCheat = StateT $ \p -> return ( (), p { lost=True, cheated=True } )

ifThenElseM :: Monad m => Bool -> a -> m a -> m a
ifThenElseM b a ma = if b then return a else ma

(>?=) :: (Monad m) => m (Maybe a) -> (a -> m b) -> m ()
m >?= f = m >>= f'
  where f' Nothing = return ()
        f' (Just a) = f a >> return ()

makeSuggestion :: (Monad m) => PlayerPosition -> GameT m ()
makeSuggestion i = do
  -- ask the current player for a suggestion
  suggest `onPosition` i >?= \scenario -> do
  
  -- broadcast the suggestion to the other players
  -- and record it in the log
  let suggestion = Suggestion i scenario
  notify suggestion `onPositions` (i+1,i)
  log suggestion

  -- find the first one who can refute the scenario
  let cs = map ($scenario) [ SuspectCard . getWho, RoomCard . getWhere, WeaponCard . getHow ] 
  let relevant = intersect cs . hand
  let findRefuter = liftM position . (find $ not . null . relevant)
  findRefuter `liftM` (get `onPositions` (i+1,i)) >?= \j -> do

  -- make sure they reveal a valid card
  valid@(defaultReveal:_) <- gets relevant `onPosition` j
  hasCheated <- gets cheated `onPosition` j
  shown <- ifThenElseM hasCheated defaultReveal $ do
              shown <- reveal (i,scenario) `onPosition` j
              ifThenElseM (shown `elem` valid) shown $ do
                  recordCheat `onPosition` j
                  return defaultReveal

  -- broadcast the reveal appropriately
  let revelation = RevealSomething j
  notify revelation `onPositions` (j+1,i)
  (notify $ RevealCard j shown) `onPosition` i
  notify revelation `onPositions` (i+1,j)
  log revelation

makeAccusation :: (Monad m) => PlayerPosition -> GameT m ()
makeAccusation i = do
  -- ask the current player for an accusation
  accuse `onPosition` i >?= \scenario -> do
  won <- (scenario ==) `liftM` secret

  let result = (if won then WinningAccusation else LosingAccusation) i scenario

  notify result `onPositions` (i,i)
  log result

  when won $ setWinner i

mkPlayerInfo :: Monad m => TotalPlayers -> PlayerPosition -> [Card] -> WpPlayer m -> m (PlayerInfo m)
mkPlayerInfo t i cs w = snd `liftM` (dealIn t i cs `runStateT` PlayerInfo w i cs False False)

-- play a random game, using the given methods to generate players
setup :: (RandomGen g, Monad m) => [WpPlayer m] -> StateT g m (Maybe (Scenario, [PlayerInfo m]))
setup ws = do
  let n = length ws 
  if n == 0 || length cards `mod` n /= 3 `mod` n
    then return Nothing
    else do
      (killer:_)  <- draw suspects
      (weapon:_)  <- draw weapons
      (room:_)    <- draw rooms
      let deck    = cards \\ [ SuspectCard killer, WeaponCard weapon, RoomCard room ]
      hands       <- deal n `liftM` shuffle deck
      ps          <- lift $ sequence $ zipWith3 (mkPlayerInfo n) [0..(n-1)] hands ws
      return $ Just (Scenario room killer weapon, ps)

