{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List (group, sort)

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
histogram = undefined

-- Provide count of each Integer in the list
-- Example:
-- countIntElems [1,4,5,4,6,6,3,4,2,4,9]
-- [(1,1),(2,1),(3,1),(4,4),(5,1),(6,2),(9,1)]
countIntElems :: [Integer] -> [(Integer,Int)]
countIntElems lst = map (\n->(head n, length n)) $ group $ sort lst

-- Puts the star in the correct position number
putStarPos :: Int -> String
putStarPos n = replicate n ' ' ++ "*" ++ replicate (9-n) ' '
