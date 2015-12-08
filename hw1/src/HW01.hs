{-# OPTIONS_GHC -Wall #-}
module HW01 where
import Data.Char

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x                     = mod x 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x                 = div x 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0                   = []
toRevDigits x                   = (lastDigit x) : toRevDigits (dropLastDigit x)

--Create toDigits from scratch
firstDigit :: Integer -> Integer
firstDigit x
    | (n:_) <- (show x)         = toInteger (digitToInt n)
    | otherwise                 = 0

dropFirstDigit :: Integer -> Integer
dropFirstDigit x
    | x < 10                    = (-1)
    | (_:ns) <- (show x)        = read ns :: Integer
    | otherwise                 = (-1)

toDigits :: Integer -> [Integer]
toDigits (-1)                   = []
toDigits x                      = (firstDigit x) : toDigits (dropFirstDigit x)


-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []             = []
doubleEveryOther (x:[])         = [x]
doubleEveryOther (x:y:xs)       = x: (y*2) : doubleEveryOther xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
-- foldl anonymous function will add all digits in the array from toDigits
sumDigits :: [Integer] -> Integer
sumDigits []                    = 0
sumDigits (x:xs)                = ((\a -> foldl (+) 0 a) (toDigits x)) + sumDigits xs 

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = mod (sumDigits (doubleEveryOther (toRevDigits x))) 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
