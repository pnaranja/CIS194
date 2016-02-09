{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List (sort, group)

-- Given a list, output a list of lists
-- Given list length n of the input, output list length n, n-1, n-2, ... lists
-- Every nth list in the output should contain every nth element from the input list
skips :: [a] -> [[a]]
skips [] = []
skips (x:xs) = (x:xs) : skips xs

-- Given a list of Integers, find the localMaxima
-- The localMaxima is defined as an element in the list that is greater than both elements
-- before and after it.
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:xs)
    | y>x && y>z    = y : localMaxima (z:xs)
    | otherwise     = localMaxima (y:z:xs)


-- Show histogram of how many of each number in the list
-- Example:
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
--     *
--     *
--     * *
--  ******  *
-- ==========
-- 0123456789
histogram :: [Integer] -> String
histogram x = (concat $ reverse (revhistogram x)) ++ "==========\n0123456789\n"

revhistogram :: [Integer] -> [String]
revhistogram [] = []
revhistogram x = histogramLine x : revhistogram (concat $ map reduceListByOne $ group $ sort x)

-- Reduce contents of list by 1
reduceListByOne :: [Integer] -> [Integer]
reduceListByOne []     = []
reduceListByOne [_]    = []
reduceListByOne (_:xs) = xs

-- Create histogram line
histogramLine :: [Integer] -> String
histogramLine lst = foldr1 mergeStars (map putStarPos lst) ++ "\n"

-- Puts the star in the correct position number
putStarPos :: Integer -> String
putStarPos n = replicate (fromIntegral n) ' ' ++ "*" ++ replicate (fromIntegral (9-n)) ' '

-- Combine star strings
mergeStars :: String -> String -> String
mergeStars x []                     = x
mergeStars [] y                     = y
mergeStars (x:xs) (y:ys)
    | (x=='*') || (y=='*')          = '*' : mergeStars xs ys
    | otherwise                     = ' ' : mergeStars xs ys
