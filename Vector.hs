module Vector where
-- import System.Random

-- Data Declarations in Haskell book

data Player = Player1 | Player2 | Tie deriving Show

data RPS = Rock | Paper | Scissors deriving (Show, Eq)

instance Ord RPS where
    (<=) Rock Paper = True
    (<=) Paper Scissors = True
    (<=) Scissors Rock = True
    (<=) Paper Rock = False
    (<=) Scissors Paper = False
    (<=) Rock Scissors = False

bout :: RPS -> RPS -> Player
bout p1 p2 | p1 == p2 = Tie
           | p1 < p2 = Player2
           | otherwise = Player1

data Coin = Heads | Tails deriving (Show, Eq)