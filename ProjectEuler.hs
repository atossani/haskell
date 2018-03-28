module ProjectEuler where

import NumberTheory

import AlecHaskell

import Vector

type Prime = Integer

-- Project Euler solutions

------------------------

{-- If we list all the natural numbers below 10 that are multiples of
3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000. --}

euler1 :: Integer
euler1 = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]



------------------------

{--
Each new term in the Fibonacci sequence is generated by adding the
previous two terms. By starting with 1 and 2, the first 10 terms will
be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not
exceed four million, find the sum of the even-valued terms.
--}

euler2 :: Integer -> Integer
euler2 n = sum $ takeWhile (<= n) [x | x <- fibs, mod x 2 == 0]



------------------------

{--
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
--}

{--
The solution below is too slow for a large number, was just an exercise to think about the problem.

-- primes :: [Integer]
-- primes = [x | x <- [1..], isprime x]

-- primesN :: Integer -> [Integer]
-- primesN n = [x | x <- factors n, isprime x]

-- euler3 :: Integer -> Integer
-- euler3 = maximum.primesN
--}

euler3 :: Integer -> Integer
euler3 n = hackN n 2    
    where
        hackN n k | n == k = n
                  | mod n k == 0 = hackN (div n k) k
                  | otherwise = hackN n (k + 1)



------------------------

{-- A palindromic number reads the same both ways. The largest
palindrome made from the product of two 2-digit numbers is 9009 = 91 ×
99.

Find the largest palindrome made from the product of two 3-digit
numbers. --}

-- Alec's solution
-- Sort of Jon's solution. Alec gave up on solution without a list and stole part of Jon's idea.

euler4 :: [Integer] -> [Integer] -> Integer
euler4 ns ms = maximum [(n * m) | n <- reverse ns, m <- reverse ms, ispal (n * m)]
    where
        ispal x = x == revN x

-- Solution found on Project Euler forum to ask Jon about
-- [m | a <- [9], b <- [0..9], c
-- <- [0..9], m <- [100001* a + 10010 * b + 1100 * c], [x | x <-
-- [100..999], m `mod` x == 0 && m `div` x < 1000] /= []]



------------------------

{-- 2520 is the smallest number that can be divided by each of the
numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all
of the numbers from 1 to 20? --}

pfact :: Integer -> Integer -> Integer
pfact m n = fact m n 1
    where fact m n k | n^k > m = n^(k - 1)
                     | otherwise = fact m n (k + 1)

euler5 :: Integer -> Integer
euler5 m = product $ map (pfact m) (takeWhile (< m) eratosthenes2)



------------------------

{-- The sum of the squares of the first ten natural numbers is,

1^2 + 2^2 + ... + 10^22 = 385

The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square
of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one
hundred natural numbers and the square of the sum. --}

euler6 :: [Integer] -> Integer
euler6 ns = ((sum ns)^2) - (sum [(x^2) | x <- ns])



------------------------

{-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we
can see that the 6th prime is 13.

What is the 10 001st prime number? --}

euler7 :: Integer -> Integer
euler7 n = head $ drop (fromIntegral (n - 1)) eratosthenes2



------------------------

{--A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.--}


euler9 :: Integer -> Integer
euler9 n = pyth (abcs n)
    where
        abcs n = [(a,b,c) | a <- [2..(n - 2)], b <- [3..(n - 2)], c <- [4..(n - 2)], (a < b), (b < c), (a + b + c == n)]
        pyth ((a, b, c):abcs) | (a^2) + (b^2) == (c^2) = a * b * c
                              | otherwise = pyth (abcs)


------------------------

{--The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.--}

euler10 :: Integer -> Integer
euler10 n = sum $ takeWhile (< n) eratosthenes2



------------------------

{-- The sequence of triangle numbers is generated by adding the
natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5
+ 6 + 7 = 28. The first ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

 1: 1  3: 1,3  6: 1,2,3,6 10: 1,2,5,10 15: 1,3,5,15 21: 1,3,7,21 28:
1,2,4,7,14,28 We can see that 28 is the first triangle number to have
over five divisors.

What is the value of the first triangle number to have over five
hundred divisors?--}

fsinnum :: Integer -> Integer
fsinnum n = ff n eratosthenes2 0
    where
        ff n (k:ks) j | n < k = j + 1
                      | mod n k == 0 = ff (div n k) (k:ks) (j + 1)
                      | j == 0 = ff n ks 0
                      | otherwise = (j + 1) * ff n ks 0


euler12 :: Integer -> [Integer] -> Integer
euler12 n (x:xs) | fsinnum x > n = x
                 | otherwise = euler12 n xs



------------------------

{--2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?--}

euler16 :: Integer -> Integer
euler16 n = sum (listify (2^n))



------------------------

{--n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!--}

euler20 :: Integer -> Integer
euler20 n = sum (listify (product [1..n]))



------------------------

{--Let d(n) be defined as the sum of proper divisors of n (numbers
less than n which divide evenly into n). If d(a) = b and d(b) = a,
where a ≠ b, then a and b are an amicable pair and each of a and b are
called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20,
22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284
are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.--}

euler21 :: Integer -> Integer
euler21 n | n > 10^4 = 0
          | amicable n = n + euler21 (n+1)
          | otherwise = euler21 (n+1)

amicable :: Integer -> Bool
amicable n | plusfacts n == n = False
           | otherwise = (plusfacts.plusfacts) n == n

plusfacts :: Integer -> Integer
plusfacts 1 = 1
plusfacts n = ff 0 1 n
  where
    ff accum x n | n == x = accum
                 | mod n x == 0 = ff (accum + x) (x + 1) n
                 | otherwise = ff accum (x + 1) n



------------------------

{--The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144
The 12th term, F12, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000 digits?--}

euler25 :: Integer -> Integer
euler25 n = ff n fibs 2
  where
    ff n (f:fs) accum | len2 f == n = accum
                      | otherwise = ff n fs (accum + 1)



------------------------

{--A unit fraction contains 1 in the numerator. The decimal
representation of the unit fractions with denominators 2 to 10 are
given:

1/2 =   0.5 1/3 =   0.(3) 1/4 =   0.25 1/5 =   0.2 1/6 =   0.1(6) 1/7
=   0.(142857) 1/8 =   0.125 1/9 =   0.(1) 1/10  =   0.1 Where 0.1(6)
means 0.166666..., and has a 1-digit recurring cycle. It can be seen
that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest
recurring cycle in its decimal fraction part.--}

--euler26


------------------------

{--Starting with the number 1 and moving to the right in a clockwise
direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is
101.

What is the sum of the numbers on the diagonals in a 1001 by 1001
spiral formed in the same way?--}

euler28 :: Integer
euler28 = foldr ((+).spiral) 0 [2,4,6,8] - 3

spiral :: Integer -> Integer
spiral n = ff n 1 500
  where
    ff n a 0 = 1
    ff n a c = a + n + ff (n + 8) (a + n) (c - 1)


------------------------

{-- Surprisingly there are only three numbers that can be written as
the sum of fourth powers of their digits:

1634 = 14 + 64 + 34 + 44 8208 = 84 + 24 + 04 + 84 9474 = 94 + 44 + 74
+ 44 As 1 = 14 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of
fifth powers of their digits.--}

euler30 :: [Integer] -> [Integer]
euler30 [] = []
euler30 (x:xs) | x == (sum (map (^5) (f x))) = x : euler30 xs
               | otherwise = euler30 xs
                  where
                    f n = listify n

-- Need to sum(euler30 [1111..999999]) for final answer


------------------------

{-- In England the currency is made up of pound, £, and pence, p, and
there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p). It is possible to
make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p How many different ways can
£2 be made using any number of coins?--}

tw = [(a,b) | a <- (map (\w -> 200-w) [0..200]),
  b <- (map (*2) [0..100]), (a + b == 200)]
fi = [(a,b,c) | a <- (map (\w -> 200-w) [0..200]),
  b <- (map (*2) [0..100]), c <- (map (*5) [0..40]), (a + b + c == 200)]
twh = [0,200]
onh = [(a,b) | a <- [0,100,200], b <- twh, (a + b == 200)]