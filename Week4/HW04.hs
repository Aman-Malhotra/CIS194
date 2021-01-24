{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List (dropWhileEnd)
newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a)              => Eq (Poly a) where
    -- This will not be effecient way of doing it I think,
    -- because its taking extra storage to save the new list 
    -- without trailing zeros and then does the matching
    P a == P b                      = withoutTrailingZerosA == withoutTrailingZerosB
        where 
            withoutTrailingZerosA   = dropWhileEnd (== 0) a 
            withoutTrailingZerosB   =  dropWhileEnd (== 0) b
        

-- Exercise 3 -----------------------------------------

-- e == exponent 
-- c == count of the polynomial term, is it first, second, third and so on
showHelper :: (Num a, Show a, Eq a) => Poly a -> Int -> Int -> String
showHelper (P (p:ps)) e c
    | p == 0                = showHelper (P ps) (e-1) (c+1)
    | e == 0                = leadingPlus ++ show p 
    -- I was trying to get nested guards here instead of using [if-then-else] or [case] but wasnt able to 
    | e == 1                = (case p of 
        0                   -> ""
        1                   -> leadingPlus ++ "x"
        anyP                -> leadingPlus  ++ show anyP ++ "x" )  ++ showHelper (P ps) (e-1) (c+1)
    | otherwise             = leadingPlus ++ show p ++ "x^" ++ show e ++ showHelper (P ps) (e-1) (c+1)
    where leadingPlus       = case c of 
                0           -> "" 
                _           -> " + "
showHelper (P []) _ _       = ""

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = 
        showHelper (P $ reverse withoutTrailingZeros ) (length withoutTrailingZeros - 1) 0
            where 
                withoutTrailingZeros = dropWhileEnd (==0) p

-- Exercise 4 -----------------------------------------

plus :: Num a               => Poly a -> Poly a -> Poly a
plus (P polyA) (P polyB)    = P $ zipWith (+) (polyA ++ replicate leadingZerosToA 0) (polyB ++ replicate leadingZerosToB 0)
    where 
        lA                  = length polyA 
        lB                  = length polyB
        leadingZerosToA 
            | lA < lB       = lB - lA
            | otherwise     = 0
        leadingZerosToB 
            | lB < lA       = lA - lB
            | otherwise     = 0

-- Exercise 5 -----------------------------------------

-- c is the coeff being multiplied from the first polynomial
-- e is the exponentioal term being used for multiplication from first polynomial
timesHelper ::  (Num a)     => Poly a -> a -> Int -> Poly a
timesHelper (P p) c e = P $ replicate e 0 ++ map (*c) p

addAllTimes :: (Num a)      => [Poly a] -> Poly a
addAllTimes []              = P []
addAllTimes [p1]            = p1
addAllTimes [p1, p2]        = plus p1 p2
addAllTimes (p1:p2:ps)      = plus p1 p2 + addAllTimes ps

getAllTimes ::  (Num a)     => Int -> Poly a -> Poly a -> [Poly a]
getAllTimes _ (P []) _      = []
getAllTimes n (P [p]) q     = [timesHelper q p n]
getAllTimes n (P (p:ps)) q  = timesHelper q p n :  getAllTimes (n+1) (P ps) q

times :: (Num a)            => Poly a -> Poly a -> Poly a
times p q                   = addAllTimes $ getAllTimes 0 p q

-- in progress
-- times (P a) (P b) = result 
--     where 
--         allTimes :: [Poly a]
--         allTimes = undefined 
--         result   = addAllTimes allTimes
        

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined
