module HW4 where

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 []      = 1
fun1 (x:xs)
 | even x    = (x-2) * fun1 xs
 | otherwise = fun1 xs

fun1' = product . map (\x->x-2) . filter even

fun2 :: Integer -> Integer
fun2 1       = 0
fun2 n
 | even n    = n + fun2 (div n 2)
 | otherwise = fun2 (3*n+1)

-- if 1 then 0 else n
-- if even sum with itself and fun2 (div n 2) else fun2 (3n+1)
-- Use iterate to create the array to apply function depending on even or odd
-- Stop when 1 is found
-- Given the algorithm, only accepts even numbers!  Filter out odd numbers in the array
-- Sum all values in the array
fun2' = sum . filter even . takeWhile (/=1) . iterate (\x-> if odd x then 3*x+1 else div x 2)


-- Exercise 2
-- Height of btree is length of path from root to deepest node (non-Leaf)
-- Balanced btree if left and right subtrees differ by <= 1
-- NOTE: Does NOT NEED to be in order!  Just balanced!

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show,Eq)

-- Balance a btree using foldr
-- Example:
-- foldTree "ABCDEFGHIJ" ==
-- Node 3
-- (Node 2
-- (Node 0 Leaf ’F’ Leaf)
-- ’I’
-- (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
-- ’J’
-- (Node 2
-- (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
-- ’H’
-- (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))

foldTree :: Ord a => [a] -> Tree a
foldTree                                       =  foldr insertTree Leaf

heightTree :: Tree a -> Integer
heightTree Leaf                                =  0
heightTree (Node _ Leaf _ Leaf)                =  0
heightTree (Node _ branch1 _ branch2)          =  1 + max (heightTree branch1) (heightTree branch2)

balancedTree :: Tree a -> Bool
balancedTree Leaf                              =  True
balancedTree (Node _ branch1 _ branch2)        =  (abs ((heightTree branch1) - (heightTree branch2)) <= 1) && balancedTree branch1 && balancedTree branch2

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x Leaf                              =  Node 0 Leaf x Leaf

insertTree x (Node nodenum Leaf n Leaf)        =  Node newHeight Leaf n (insertTree x Leaf)
    where newHeight                            =  heightTree (Node nodenum Leaf n (insertTree x Leaf))

insertTree x (Node nodenum Leaf n branch2)     =  Node nodenum (insertTree x Leaf) n branch2

insertTree x (Node nodenum branch1 n branch2)
    | (hbranch1 >= hbranch2) && balancedTrees  =  Node newHeight branch1 n (insertTree x branch2)
    | otherwise                                =  Node nodenum (insertTree x branch1) n branch2
        where
            hbranch1                           =  heightTree branch1
            hbranch2                           =  heightTree branch2
            balancedTrees                      =  balancedTree branch1 && balancedTree branch2
            newHeight                          =  heightTree (Node nodenum branch1 n (insertTree x branch2))


-- Exercise 3
-- xor - Return True if odd # of True values
-- Need to use fold
xor :: [Bool] -> Bool
--xor = odd . length . filter (==True) 
xor = odd . foldl (\x y -> x + ((\y'->if y' then 1 else 0) y)) 0

-- implement map using foldr
map' :: (a -> b) -> [a] -> [b]
map' = undefined
