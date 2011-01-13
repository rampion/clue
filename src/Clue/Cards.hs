module Clue.Cards where
import System.Random (Random, RandomGen, randomR)
import Control.Monad.State

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

-- a possible murder scenario
data Scenario = Scenario { getWhere :: Room, getWho :: Suspect, getHow :: Weapon }
  deriving (Eq, Show, Read)

-- cards are the clues that players use to learn
-- what really happened
data Card = RoomCard Room | SuspectCard Suspect | WeaponCard Weapon
  deriving (Eq, Show, Read)
cards :: [Card]
cards = map RoomCard rooms ++ map SuspectCard suspects ++ map WeaponCard weapons

-- draw a random member of a list
draw :: (RandomGen g, Monad m) => [a] -> StateT g m (a,[a])
draw as = do
  i <- StateT $ return . randomR (0, length as - 1)
  let (before, a : after) = splitAt i as
  return (a, before ++ after)

-- shuffle the elements of a list into random order
-- using http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
shuffle :: (RandomGen g, Monad m) => [a] -> StateT g m [a]
shuffle [] = return []
shuffle as = do
  (a,as) <- draw as
  liftM (a:) $ shuffle as

-- deal out the given cards to n players
deal :: Int -> [a] -> [[a]]
deal n [] = replicate n []
deal n as = zipWith (:) cs $ deal n as'
  where (cs, as') = splitAt n as
