{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, Rank2Types, FlexibleInstances, FlexibleContexts, RankNTypes, TupleSections #-}
module Clue.Game where
{- This module simulates a simplified game of Clue (or Cluedo)
 - using the playgame function to mediate a game between
 - supplied players.
-}
import Prelude hiding (log)
import Control.Monad (liftM, unless)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans (MonadIO)
import Data.List ((\\), find, findIndex, intersect)
import Data.Maybe (isNothing)
import System.Random (RandomGen)

import Clue.Cards
import Clue.Player

-- state about a player
data PlayerInfo m = PlayerInfo  { agent :: WpPlayer m
                                , position :: PlayerPosition
                                , hand :: [Card]
                                , lost :: Bool
                                , cheated :: Bool }
instance Show (PlayerInfo m) where
  show pi = show (position pi, hand pi, lost pi, cheated pi)


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

type GameState m = [ PlayerInfo m ]
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

onPositions :: Monad m => StateT (PlayerInfo m) m a -> (PlayerPosition, PlayerPosition) -> GameT m [a]
onPositions st (lo,hi) = GameT $ \(_, state) -> do
  let ps = state
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
  return (as, ps, [])

onPosition :: Monad m => StateT (PlayerInfo m) m a -> PlayerPosition -> GameT m a
onPosition st i = head `liftM` (st `onPositions` (i,i+1))

recordCheat :: Monad m => StateT (PlayerInfo m) m ()
recordCheat = StateT $ \p -> return ( (), p { lost=True, cheated=True } )

ifThenElseM :: Monad m => Bool -> a -> m a -> m a
ifThenElseM b a ma = if b then return a else ma

makeSuggestion :: (Monad m) => PlayerPosition -> GameT m ()
makeSuggestion i = do
  -- ask the current player for a suggestion
  maybeScenario <- suggest `onPosition` i 
  case maybeScenario of
    Nothing       -> return ()
    Just scenario -> do

      -- broadcast the suggestion to the other players
      -- and record it in the log
      let suggestion = Suggestion i scenario
      notify suggestion `onPositions` (i+1,i)
      log suggestion

      -- find the first one who can refute the scenario
      let cs = map ($scenario) [ SuspectCard . getWho, RoomCard . getWhere, WeaponCard . getHow ] 
      let relevant = intersect cs . hand
     
      n <- GameT $ \(_,ps) -> return (length ps, ps, [])
      let findRefuter = liftM (\j -> (j + i + 1) `mod` n) . (findIndex $ not . null . relevant)

      maybeRefuter <- findRefuter `liftM` (get `onPositions` (i+1,i))
      case maybeRefuter of
        Nothing -> return ()
        Just j  -> do
      
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

makeAccusation :: (Monad m) => PlayerPosition -> GameT m Bool
makeAccusation i = do
  -- ask the current player for an accusation
  maybeScenario <- accuse `onPosition` i
  case maybeScenario of
    Nothing       -> return False
    Just scenario -> do
      won <- (scenario ==) `liftM` secret

      let result = (if won then WinningAccusation else LosingAccusation) i scenario

      notify result `onPositions` (i,i)
      log result

      return won

turnLoop :: Monad m => PlayerPosition -> GameT m (Maybe PlayerPosition)
turnLoop i = do
  makeSuggestion i
  won <- makeAccusation i
  ifThenElseM won (Just i) $ do
      let findNext = liftM position . (find $ not . lost)
      maybeNext <- findNext `liftM` (get `onPositions` (i+1,i+1))
      case maybeNext of 
        Nothing -> return Nothing
        Just i' -> turnLoop i'

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

play :: (RandomGen g, Monad m) => StateT [WpPlayer m] (StateT g m) (Maybe PlayerPosition, Log)
play = do
  pregame <- get >>= lift . setup
  case pregame of
    Nothing   -> return (Nothing, [])
    Just init -> do
      (winner, ps', ls) <- lift $ lift $ runGameT (turnLoop 0) init
      put $ map agent ps'
      return (winner, ls)
  

