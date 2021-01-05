module Week1.HW01 where

-- Homework Url https://www.cis.upenn.edu/~cis194/spring15/hw/01-intro.pdf

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit num = num `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit num = num `div` 10 

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits num 
    | num > 0           = lastDigit num : (toRevDigits . dropLastDigit) num
    | otherwise         = []

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y:xs) = x: 2*y : doubleEveryOther xs
doubleEveryOther xs = xs

-- Exercise 4 -----------------------------------------

getSumOfDigits :: Integer -> Integer
getSumOfDigits n
    | n>10              = (n `mod` 10) + getSumOfDigits (n `div` 10)
    | otherwise         = n

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . getSumOfDigits) 0

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn number = (sumDigits . doubleEveryOther . digs) number `mod` 10 == 0

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n > 0 = hanoi (n-1) a c b ++ [(a, c)] ++ hanoi (n-1) b a c
    | otherwise = []
