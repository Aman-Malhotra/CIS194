{-# OPTIONS_GHC -Wall #-}

-- https://www.cis.upenn.edu/~cis194/spring15/hw/04-typeclasses.pdf

module Week4.HW04 where

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
    P a == P b                      = dropWhileEnd (== 0) a == dropWhileEnd (== 0) b

-- Exercise 3 -----------------------------------------

-- e == exponent 
-- c == count of the polynomial term, is it first, second, third and so on
showHelper :: (Num a, Show a, Eq a, Ord a) => Poly a -> Int -> Int -> String
showHelper (P (p:ps)) e c
    | p == 0                = showHelper (P ps) (e-1) (c+1)
    | e == 0                = leadingPlus ++ show p 
    | e == 1                = leadingPlus ++ leadingNum ++ "x"  ++ showHelper (P ps) (e-1) (c+1)
    | otherwise             = leadingPlus ++ leadingNum ++ "x^" ++ show e ++ showHelper (P ps) (e-1) (c+1)
    where 
        leadingPlus         
            | p < 0         = " - "
            | c == 0        = "" 
            | p > 0         = " + "
            | otherwise     = " + "
        leadingNum          
            | p == 1        = "" 
            | p == -1       = ""
            | p < 0         = show $ negate p
            | otherwise     = show p
showHelper (P []) _ _       = ""

instance (Num a, Eq a, Show a, Ord a) => Show (Poly a) where
    show (P p) 
        | null p            = "0"
        | otherwise         = 
            showHelper (P withoutTrailingZeros ) (length withoutTrailingZeros - 1) 0
                where 
                    withoutTrailingZeros = reverse $ dropWhileEnd (==0) p

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

-- timesHelper (P [1, 4, 2]) 1 0 == P [1, 4, 2]
-- timesHelper (P [1, 4, 2]) 1 1 == P [0, 1, 4, 2]
-- timesHelper (P [1, 4, 2]) 1 2 == P [0, 0, 1, 4, 2]
-- timesHelper (P [1, 4, 2]) 2 2 == P [0, 0, 2, 8, 4]

-- 1. add leading zeros list of size == e 
-- and multiply rest with c
timesHelper ::  (Num a)     => Poly a -> a -> Int -> Poly a
timesHelper (P p) c e       = P $ replicate e 0 ++ map (*c) p

-- getAllTimes 0 (P [1, 1, 2]) (P [1, 4, 2]) == [P [1, 4, 2], P [0, 1, 4, 2], P [0, 0, 2, 8, 4]]
getAllTimes ::  (Num a)     => Int -> Poly a -> Poly a -> [Poly a]
getAllTimes _ (P []) _      = []
getAllTimes n (P (p:ps)) q  = timesHelper q p n :  getAllTimes (n+1) (P ps) q

-- addAllTimes [P [1, 4, 2], P [0, 1, 4, 2], P [0, 0, 2, 8, 4]] == P [1, 5, 8, 10, 4]
-- This is how it works with the help of [timesHelper] function
--      [1, 4, 2, 0, 0]
--   +  [0, 1, 4, 2, 0]
--   +  [0, 0, 2, 8, 4]
--      ---------------
--      [1, 5, 8, 10, 4]
--      ---------------
addAllTimes :: (Num a)      => [Poly a] -> Poly a
addAllTimes                 = foldr (+) (P [])

times :: (Num a)            => Poly a -> Poly a -> Poly a
times p q                   = addAllTimes (getAllTimes 0 p q)

-- Exercise 6 -----------------------------------------

instance Num a             => Num (Poly a) where
    (+)                    = plus
    (*)                    = times
    -- negate                 = (* P [-1])
    -- OR
    negate  (P a)           = P $ map negate a
    fromInteger i          = P [fromInteger i]

    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a            => Poly a -> a -> a
applyP (P p) valX          = sum $ zipWith (\a b -> a * (valX ^ b)) p intList
    where 
        intList :: [Int]
        intList            = [0..]

-- Exercise 8 -----------------------------------------

-- I wasnt able to understand what is nderiv here so this is the only exc left in this weeks assignment 
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n 
        | n <= 0            = id
        | otherwise         = deriv . nderiv (n-1) 

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a, Eq a) => Differentiable (Poly a) where
    deriv (P [])            = P [0]
    deriv (P [_])           = P [0]
    deriv (P (_:ps))        = P $ zipWith (*) ps intList
            where 
                intList     = [1..]
    
    
