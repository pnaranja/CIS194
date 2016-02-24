module HW4 where

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
fun2' n = sum $ filter even $ takeWhile (/=1) $ iterate (\x-> if odd x then 3*x+1 else div x 2) n
