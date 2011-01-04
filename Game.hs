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
import Control.Monad.Trans (MonadIO)
import System.Random (randomRIO)
import Data.List ((\\), find, intersect)
import Data.Maybe (isNothing)

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
            | RevealCard PlayerPosition Card
            | WinningAccusation PlayerPosition Scenario
            | LosingAccusation PlayerPosition Scenario

class Player p where
  -- let them know what another player does
  update :: (MonadIO io) => Event -> StateT p io ()

  -- ask them to make a suggestion
  suggest :: (MonadIO io) => StateT p io (Maybe Scenario)

  -- ask them to make an accusation
  accuse :: (MonadIO io) => StateT p io (Maybe Scenario)

  -- ask them to reveal a card to invalidate a suggestion
  reveal :: (MonadIO io) => (PlayerPosition, Scenario) -> StateT p io Card
  
data MkPlayer = forall p. Player p => MkPlayer (TotalPlayers -> PlayerPosition -> [Card] -> IO p)

data WpPlayer = forall p. Player p => WpPlayer p
data PlayerInfo = PlayerInfo  { agent :: WpPlayer
                              , position :: PlayerPosition
                              , hand :: [Card]
                              , lost :: Bool
                              , cheated :: Bool }
data Action io b = Action (forall p. Player p => StateT p io b)
wrap :: (MonadIO io) => Action io b -> StateT PlayerInfo io b
wrap (Action a) = StateT $ \(pi@PlayerInfo { agent = WpPlayer p }) -> do
  (b, p) <- runStateT a p
  return $ (b, pi { agent = WpPlayer p })

instance Player PlayerInfo where
  update ev = wrap $ Action $ update ev
  suggest   = wrap $ Action $ suggest
  accuse    = wrap $ Action $ accuse
  reveal sg = wrap $ Action $ reveal sg
 
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

deal :: TotalPlayers -> [a] -> [[a]]
deal n [] = replicate n []
deal n as = zipWith (:) cs $ deal n as'
  where (cs, as') = splitAt n as

create :: MkPlayer -> TotalPlayers -> PlayerPosition -> [Card] -> IO PlayerInfo
create (MkPlayer f) n i cs = f n i cs >>= \p -> return $ PlayerInfo (WpPlayer p) i cs False False

type Game = [PlayerInfo]

putHead :: PlayerInfo -> StateT Game IO ()
putHead p = modify $ (p:) . tail

putTail :: [PlayerInfo] -> StateT Game IO ()
putTail ps = modify $ (:ps) . head

turn :: StateT Game IO (Maybe PlayerPosition)
turn = do
  -- see if they have a suggestion
  (suggestion, p) <- liftM head get >>= runStateT suggest
  putHead p

  unless (isNothing suggestion) $ do
    let (Just scenario) = suggestion

    -- notify the other players
    ps <- liftM tail get >>= mapM (notify $ Suggestion (position p) scenario)
    putTail ps
    
    -- find the first one who can refute the scenario
    let cs = map ($scenario) [ Suspect . getWho, Room . getWhere, Weapon . getHow ] 
    let (qs,rss) = break (not . null . intersect cs . hand) ps 
    unless (null rss) $ do
      let refuter:ss= rss
      let valid = intersect cs $ hand refuter

      (shown, refuter) <- if cheated refuter 
                            -- if the refuter is a cheater, just choose for him
                            then return (head valid, refuter)
                            else do 
                              (shown, refuter) <- runStateT (reveal (position p, scenario)) refuter
                              -- make sure they don't cheat
                              return $ if shown `elem` valid
                                          then (shown, refuter)
                                          else (head valid, refuter { cheated = True })

      let ev = Reveal (position refuter)
      ss <- mapM (notify ev) ss
      p <- notify (RevealCard (position refuter) shown) p
      qs <- mapM (notify ev) qs

      put $ (p:qs ++ refuter:ss)
    return ()
  return Nothing



playgame :: [MkPlayer] -> IO (Maybe PlayerPosition)
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

