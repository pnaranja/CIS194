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
-- Height of btree is length of path from root to deepest node
-- Balanced btree if left and right subtrees differ by <= 1

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

foldTree :: [a] -> Tree a
foldTree = undefined

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x Leaf                       = Node 0 Leaf x Leaf
insertTree x (Node nodenum branch1 n branch2)
    | x>n                               = Node (nodenum+1) branch1 n (insertTree x branch2)
    | otherwise                         = Node (nodenum+1) (insertTree x branch1) n branch2
