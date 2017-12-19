module NumberTheory where

-- Number tools (primes, fibs, triangles, etc)

factors :: Integer -> [Integer]
factors n = [x | x <- [1..n], mod n x == 0]

isprime :: Integer -> Bool
isprime n = length(factors n) == 2

-- really slow fib generator
-- fib :: Integer -> Integer
-- fib 0 = 0
-- fib 1 = 1
-- fib 2 = 1
-- fib n = (fib (n - 1) + fib (n - 2))

fibs :: [Integer]
fibs = f 0 1
    where
        f n m = (n + m) : f m (n + m)

tris :: [Integer]
tris = [(n^2 + n) `div` 2 | n <- [1..]]


-- alec's stupid triangle generator
-- triangles :: [Integer]
-- triangles = t 1 1
--     where
--       t n m = n : t (n + (m + 1)) (m + 1)


-- scoping is local (within scope of fibs, we've defined function f)
-- other functions can be defined under same 'where'

---------------
-- Prime Sieves

-- relying on multiplication and gcd.

eratosthenes2 :: [Integer]
eratosthenes2 = 2 : sieveP [1..] 2
  where
    sieveP (n:ns) xs | gg (2 * n + 1) xs = sieveP ns xs
                     | otherwise = (2 * n + 1) : sieveP ns ((2 * n + 1)*xs)
    gg n m = gcd n m > 1


