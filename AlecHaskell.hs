module AlecHaskell where

{--
meta methods:

quit => :q
type => :t
reload => :r
browse standard library => :browse Prelude

[1..10]!!2
ff ++ [1,2,3]

currying is right associative:
a -> a -> a == a -> (a -> a)


map :: (a -> b) -> ([a] -> [b])
map (+ 2) ff
--}

ff :: [Integer]
ff = [1,2,3,4,5,6]

gg :: [Integer]
gg = [1..20]

dropten :: [Integer] -> [Integer]
dropten xs = (take 9 xs) ++ (drop 10 xs)

dropgen :: [a] -> [a]
dropgen xs = (take 9 xs) ++ (drop 10 xs)

dropbear :: Int -> Int -> [Integer] -> [Integer]
dropbear n1 n2 xs = (take n1 xs) ++ (drop n2 xs)


------------
-- Recursion

fake :: Integer -> Integer
fake 0 = 1
fake n = 7

-- Length of a list
len :: [a] -> Integer
len [] = 0
len [x] = 1
len (x:xs) = 1 + (len xs)

{--
[1,2,3,4,5]
1:[2,3,4,5]
1 + len [2,3,4,5]
1 + len 2:[3,4,5]
1 + 1 + 
--}

hh :: [a] -> a
hh (x:xs) = x

tt :: [a] -> [a]
tt (x:xs) = xs


------------
-- Numbers are Lists

listify :: Integer -> [Integer]
listify 0 = []
listify n = (listify (div n 10)) ++ [mod n 10]

numbify :: [Integer] -> Integer
numbify [] = 0
numbify (x:xs) = x * 10^(len xs) + numbify xs

catN :: Integer -> Integer -> Integer
catN n1 n2 = numbify ((listify n1) ++ (listify n2))

lenN :: Integer -> Integer
lenN n = len (listify n) 


------------
-- Number functions without lists
-- max, min

--take
takeN :: Integer -> Integer -> Integer
takeN n d = div (mod n (10^d)) (10^(d-1))

--length
len2 :: Integer -> Integer
len2 0 = 0
len2 n = 1 + len2 (div n 10)

--concatenate
fastcat :: Integer -> Integer -> Integer
fastcat n1 n2 = (n1 * 10^(len2 n2)) + n2

--reverse
revN :: Integer -> Integer
revN 0 = 0
revN n = fastcat (mod n 10) (revN (div n 10))

{--
mod 12345 10 ++ (revN (div 12345 10))
5 ++ (revN 1234)
5 ++ (mod 1234 10 ++ (revN (div 1234 10)))
5 ++ 4 ++ (revN 123)
5 ++ 4 ++ (mod 123 10 ++ (revN (div 123 10)))
5 ++ 4 ++ 3 ++ (revN 12)
5 ++ 4 ++ 3 ++ (mod 12 10 ++ (revN (div 12 10)))
5 ++ 4 ++ 3 ++ 2 ++ (revN 1)
5 ++ 4 ++ 3 ++ 2 ++ (mod 1 10 ++ (revN (div 1 10)))

50000 + 4000 + 300 + 20 + 1 + 0

54321 * 10^(len2 0) + n2
54321 * 10^0 + 0
--}

--sum
sumN :: Integer -> Integer
sumN 0 = 0
sumN n = (mod n 10) + sumN (div n 10)

--product
prodN :: Integer -> Integer
prodN 0 = 1
prodN n = (mod n 10) * prodN (div n 10)

--general operation
type Binary = Integer -> Integer -> Integer

opN :: Binary -> Integer -> Integer -> Integer
opN bin base 0 = base
opN bin base n = bin (mod n 10) (opN bin base (div n 10))


------------
-- foldr

{--
foldr => separates list into individual components with binary operator
		 iterates on result with operator

foldr (*) 1 [1,2,3,4]
[1,2,3,4]
1:2:3:4:[1]
1*2*3*4*1

see below, foldr over set of functions
--}

f, g, h :: Int -> Int
f = (\x -> x + 2)
g = (\x -> x * x)
h = (\x -> x + 2 * x)

fgh :: Int -> Int
fgh = (foldr (.) id [f,g,h])


------------
-- Functional Equivalence

fe :: Integral a => [a]
fe = [x | x <- [1..100], mod x 5 == 0]

ge :: Integral a => [a]
ge = filter (\n -> mod n 5 == 0) [1..100]

gge :: Integral a => [a] -> [a]
gge = filter (\n -> mod n 5 == 0)


------------
-- Project Euler

{-- If we list all the natural numbers below 10 that are multiples of
3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000. --}

euler1 :: Integer
euler1 = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]


{--
Each new term in the Fibonacci sequence is generated by adding the
previous two terms. By starting with 1 and 2, the first 10 terms will
be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not
exceed four million, find the sum of the even-valued terms.
--}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = (fib (n-1) + fib (n-2))

------------
-- Scoping

fibs :: [Integer]
fibs = f 0 1
    where
        f n m = (n + m) : f m (n + m)

-- scoping is local (within scope of fibs, we've defined function f)

euler2 :: [Integer]
euler2 = [x | x <- fibs, mod x 2 == 0]


{--
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
--}


