module Listable where

class Eq v => Listable v where
    takeL, dropL :: Integer -> v -> v
    headL :: v -> v
    tailL :: v -> v
    unit :: v
    lenL :: v -> Integer
    catL :: v -> v -> v
    cons :: v -> v -> v
    revL :: v -> v

    headL n = takeL 1 n
    tailL n = dropL 1 n
    catL n1 n2 = f n1 (revL n2)
        where
            f n m | m == unit = n
                  | otherwise = f (cons n (headL m)) (tailL m)
    lenL n | n == unit = 0
           | otherwise = 1 + lenL (dropL 1 n)
    revL n = ff n unit
        where
            ff ns es | ns == unit = es
                     | otherwise = ff (tailL ns) (cons es (headL ns))

instance Listable Integer where
    unit = 0
    takeL d n = mod n (10^d)
    dropL d n = div n (10^d)
    cons ns n = ns * 10 + n


instance Eq a => Listable [a] where
    unit = []
    takeL = take.fromInteger
    dropL = drop.fromInteger
    cons [] ms = ms
    cons ns [] = ns
    cons ms [ns] = ns : ms

