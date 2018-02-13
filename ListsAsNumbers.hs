module ListsAsNumbers where

listify :: Integer -> [Integer]
listify 0 = []
listify n = (listify (div n 10)) ++ [mod n 10]

data NumList = N [Integer] deriving (Show, Eq)

nl = N [1,2,3]
ml = N [4,5,6]
kl = N [4,5,6,7,8]
jl = N [-1,2,-3,0]

-- ‘*’, ‘abs’, ‘signum’, ‘fromInteger’, and (either ‘negate’ or ‘-’)

binOp :: (a -> a -> a) -> [a] -> [a] -> [a]
binOp bin [] ys = ys
binOp bin xs [] = xs
binOp bin (x:xs) (y:ys) = (bin x y) : binOp bin xs ys

instance Num NumList where
    (+) (N xs) (N ys) = N $ binOp (+) xs ys
    (-) (N xs) (N ys) = N $ binOp (-) xs ys
    (*) (N xs) (N ys) = N $ binOp (*) xs ys
    abs (N xs) = N $ map abs xs
    signum (N xs) = N $ map signum xs
    fromInteger x = N $ listify x