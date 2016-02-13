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
-- takeWhile n\=1 ???
fun2' = undefined
