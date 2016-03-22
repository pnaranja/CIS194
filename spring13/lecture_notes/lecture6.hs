module Lecture6 where

-- Strict evaluation makes it easier to predict when and what order things will happen
-- Strict evaluation makes sense when side effects are allowed
-- Side effect: Anything that causes evaluation of an expression to interact something outside itself
--
-- A "thunk" - an unevaluated expression
f :: Num a => a -> t -> a
f x y = x + 5
throwAwayThunk = f 5 (29^345656) -- The value 29^345656 is thunk that is not used


-- Pattern matching drives evaluation
-- f1 does not need to evaluate m.  It just duplicates it and puts it into an array
-- f2 needs to evaluate the Maybe to determine the output
f1 :: Maybe a -> [Maybe a]
f1 m = [m,m]

f2 :: Maybe a -> [a]
f2 Nothing = []
f2 (Just x) = [x]

takeRepeat = take 3 . repeat 

-- Consequences of Lazy Evaluation
-- Forces you to be Pure
-- Understanding space usage
t1 = foldl (+) 0 [1,2,3]
-- Acc is not demanded until recursing the entire list --> ( ( (0+1) + 2) + 3)
-- It's a problem when the list is long
-- Use strict evaluation using foldl'
t2 = foldl' (+) 0 [1,2,3]
    where
        foldl' :: (a -> b -> a) -> a -> [b] -> a
        foldl' _ z [] = z
        foldl' f' z (x:xs) = let z' = f' z x in z' `seq` foldl' f' z' xs

-- Shortcircuiting operators
-- || and && can be shortcircuited in Strict evaluation
-- You can do this Haskell too!
myAndOp :: Bool -> Bool -> Bool
myAndOp True x   =  x
myAndOp False _  =  False

-- User defined control structures
if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

-- Infinite data structures
a1 = take 2 $ repeat 5
