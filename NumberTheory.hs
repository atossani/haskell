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

-- scoping is local (within scope of fibs, we've defined function f)
-- other functions can be defined under same 'where'


primes :: [Integer] -> [Integer] -> [Integer]
primes (n:ns) xs | any (gg n) xs = primes ns xs
                 | otherwise = primes ns (n:xs)
    where
        gg n x = mod n x == 0


triangles :: [Integer]
triangles = t 1 1
    where
      t n m = n : t (n + (m + 1)) (m + 1)