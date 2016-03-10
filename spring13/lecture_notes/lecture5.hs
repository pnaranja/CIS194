{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Lecture5 where

main :: IO ()
main = undefined

-- Only 1 kind of implementation allowed?
a0 :: a -> a -> a
a0 x _ = x 
a0 _ y = y 


-- The parametricity game!
a1 :: a -> a
a1 x = x

-- a2 :: a -> b
-- Can't do it!

a3 :: a -> b -> a
a3 x _ = x

-- Other alternatives
a4 :: [a] -> [a]
a4 = take 1

a5 :: (b->c) -> (a->b) -> (a->c)
a5 f g = f . g

-- Other alternatives
a6 :: (a->a) -> a -> a
a6 f x = x



-- Type Classes
--
-- Type class with single parameter a.  Any type a which wants to be an instance of Eq2 must define the functions (==) and (/=)
class Eq2 a where
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    x /== y = not (x === y) -- Default implementation so instance does not NEED to define this

data MyDataType = F Int | G Char

-- NOTE: These Type Class instances can be defined even in a different file than where the Type Class was defined
instance Eq2 MyDataType where
    (F i1) === (F i2) = i1 == i2
    (G c1) === (G c2) = c1 == c2
    _ === _ = False -- Comparing Int to Char is automatically False


-- Type Classes can be multi-parametered -> multiple dispatch
class Berg a b where
    berg :: a -> b -> Bool


class Listable a where
    toList :: a -> [Int]

instance Listable Int where
    toList x = [x]

instance Listable Bool where
    toList True = [1]
    toList False = [0]

instance Listable [Int] where
    toList = id

-- Flatten a binary tree to an [Int]
data Tree a = Empty | Node a (Tree a) (Tree a)
instance Listable (Tree Int) where
    toList Empty = []
    toList (Node x l r) = toList l ++ [x] ++ toList r

-- Use Listable as a type constraint
sumL :: Listable a => a -> Int
sumL = sum . toList

foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y

-- If types a and b are Listable then it's pair (a,b) is an instance of Listable
instance (Listable a, Listable b) => Listable (a,b) where
    toList (x,y) = toList x ++ toList y
