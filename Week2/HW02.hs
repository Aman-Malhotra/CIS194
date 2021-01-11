{-# OPTIONS_GHC -Wall #-}

module Week2.HW02 where 

-- Mastermind -----------------------------------------
-- https://www.cis.upenn.edu/~cis194/spring15/hw/02-lists.pdf

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess

-- Example: exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red] == 0
-- Example: exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange] == 2
exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0 
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (c1:xs) (c2:ys) = (if c1 == c2 then 1 else 0) + exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map (\c -> length (filter (== c) xs)) colors

-- helper function for matches
getMatchCount :: [Int] -> [Int] -> Int
getMatchCount [] [] = 0
getMatchCount _ [] = 0
getMatchCount [] _ = 0
getMatchCount (x:xs) (y:ys) = (if x>0 && y>0 then min x y else 0) + getMatchCount xs ys

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches [] [] = 0
matches [] _ = 0
matches _ [] = 0
matches (c1:c1x) (c2:c2x) = getMatchCount (countColors (c1:c1x)) (countColors (c2:c2x))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the secret
getMove :: Code -> Code -> Move
getMove sec move = Move move exMatch (matches sec move - exMatch)
    where   exMatch = exactMatches sec move

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent mv@(Move guess _ _) code = mv == getMove code guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent 

-- Exercise 6 -----------------------------------------

-- I wasnt able to solve this one on my own 

allCodesHelper :: Code -> [Code]
allCodesHelper code = map (:code) colors

allCodes :: (Code -> [Code]) -> Int -> [Code]
allCodes _          0 = [[]]
allCodes addColorFn n
  | n > 0     = concatMap addColorFn nxtCodes
  | otherwise = [[]]
  where
    nxtCodes  = allCodes addColorFn $ n-1  
                  

-- Exercise 7 -----------------------------------------
solveHelper :: Code -> [Code] -> [Move]
solveHelper _                    [] = []
solveHelper secret (guess:remCodes) = move: nextAllowedCodes
  where
    move               = getMove secret guess
    getFilteredCodes   = filterCodes move remCodes
    nextAllowedCodes   = solveHelper secret getFilteredCodes


solve :: Code -> [Move]
solve secretCode      = solveHelper secretCode getAllCodes
  where getAllCodes   = allCodes allCodesHelper $ length secretCode
