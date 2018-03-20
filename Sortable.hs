module Sortable where

import Listable
import System.Random

--qsort
qs = [5,3,7,1,4,4]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (n:ns) = (qsort [y | y <- ns, y <= n]) ++ [n] ++ (qsort [x | x <- ns, x > n])


keys :: [Integer]
keys = randomRs (0, 10^6) $ mkStdGen 42

qshuffle :: Ord a => [a] -> [a]
qshuffle ns = snd.unzip.qsort.zip keys $ ns

class Sortable s where
    sort :: s -> s
    shuffle :: s -> s

instance Ord s => Sortable [s] where
    sort = qsort
    shuffle = qshuffle

-- instance Sortable Integer where
--     sort n = sortL (headL n) (tailL n)
--         where 