{-# OPTIONS_GHC -Wall #-}
module Golf where

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
localMaxima = undefined
