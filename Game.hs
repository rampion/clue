{-# LANGUAGE ExistentialQuantification #-}
module Clue.Game where
import Control.Monad (liftM, unless)
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

data Agent = forall p. Player p => Agent p

-- deal this player in
type MkAgent = TotalPlayers -> PlayerPosition -> [Card] -> IO Agent
data PlayerInfo = PlayerInfo  { agent :: Agent
                              , position :: PlayerPosition
                              , hand :: [Card]
                              , lost :: Bool
                              , cheated :: Bool }

data Event  = Suggestion PlayerPosition Scenario
            | Reveal PlayerPosition
            | WinningAccusation PlayerPosition Scenario
            | LosingAccusation PlayerPosition Scenario

class Player a where
  -- let them know what another player does
  update :: a -> Event -> IO a

  -- ask them to make a suggestion
  suggest :: a -> IO (a, Maybe Scenario)

  -- ask them to make an accusation
  accuse :: a -> IO (a, Maybe Scenario)

  -- ask them to reveal a card to invalidate a suggestion
  reveal :: a -> (PlayerPosition, Scenario) -> IO (a, Card)


wrap :: Player a => PlayerInfo -> IO (a, b) -> IO (PlayerInfo, b)
wrap p m = m >>= \(a,b) -> return (p { agent = Agent a }, b)

instance Player PlayerInfo where
  update  (p@PlayerInfo { agent=(Agent a) }) = wrap p . update a
    where wrap p m = m >>= \a -> return $ p { agent = Agent a }
  suggest (p@PlayerInfo { agent=(Agent a) }) = wrap p $ suggest a
  accuse  (p@PlayerInfo { agent=(Agent a) }) = wrap p $ accuse a
  reveal  (p@PlayerInfo { agent=(Agent a) }) = wrap p . reveal a
  
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

apply :: MkAgent -> TotalPlayers -> PlayerPosition -> [Card] -> IO PlayerInfo
apply m n i cs = m n i cs >>= \a -> return $ PlayerInfo a i cs False False

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
      ps          <- sequence $ zipWith3 (flip apply n) ms [0..] (deal n deck)
      return Nothing
