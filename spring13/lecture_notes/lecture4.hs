--Lambdas (Anonymous Functions)
greaterThan100 :: [Integer] -> [Integer]
greaterThan100 []     = []
greaterThan100 (x:xs)
 | x>100              = x : greaterThan100 xs
 | otherwise          = greaterThan100 xs

greaterThan100' :: [Integer] -> [Integer]
greaterThan100' = filter (>100) -- Parial apply an "operator section"

--Functional Composition
myfoo :: (b->c) -> (a->b) -> (a->c)
myfoo f g = \x -> f (g x)

--myfoo is actually represents function composition!
myfoo' :: (b->c) -> (a->b) -> a -> c
myfoo' f g = f . g


intLengthEvenGreaterThan100 :: [Integer] -> Bool
intLengthEvenGreaterThan100 = even . length . greaterThan100'
