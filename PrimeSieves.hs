module PrimeSieves where

-- naive approach with arrays.
eratosthenes = 2 : sieveP [1..] [2]
  where
    sieveP (n:ns) xs | gg (2 * n + 1) xs = sieveP ns xs
                     | otherwise = (2 * n + 1) : sieveP ns ((2 * n + 1):xs)
    gg _ [] = False
    gg n (x:xs) | mod n x == 0 = True
                | otherwise = gg n xs

-- relying on multiplication and gcd.
-- a cubic fuck-ton faster.
eratosthenes2 = 2 : sieveP [1..] 2
  where
    sieveP (n:ns) xs | gg (2 * n + 1) xs = sieveP ns xs
                     | otherwise = (2 * n + 1) : sieveP ns ((2 * n + 1)*xs)
    gg n m = gcd n m > 1

-- a seriously slow version of the sundaram algorithm.
sundaram :: Integer -> [Integer]
sundaram n = 2 : [2 * x + 1 | x<-[1..2*n+2], not.any (== x) $ f 1 n]
  where
    f i 1 = [4]
    f i j | i > j = f 1 (j-1)
          | otherwise = (i + j + 2 * i * j) : f (i+1) j


