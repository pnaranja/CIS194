strLength :: String -> Int
strLength []     = 0
strLength (_:xs) = let len_rest = strLength xs in
                       len_rest + 1

frob :: String -> Char
frob []         = 'a'
frob str
    | len > 5   = 'x'
    | len > 3   = 'y'
    | otherwise = 'z'
        where
            len = strLength str

--Accumulators
sumTo20 :: [Int] -> Int
sumTo20 nums  = go 0 nums 
  where go :: Int -> [Int] -> Int 
        go acc [] = acc   -- Empty list, return acc sum 
        go acc (x:xs) 
         | acc >= 20 = acc 
         | otherwise = go (acc + x) xs

-- Parametric polymorphism
notEmpty :: [a] -> Bool
notEmpty (_:_) = True
notEmpty _         = False

limited :: a->a -- Limited to only 1 possible function definition because of the type definition
limited a = a

-- Map, Filter and Folds
exampleList = [-1,2,6]
result = map (+1) exampleList

keepOnlyPositive :: [Integer] -> [Integer]
keepOnlyPositive x = filter (\x->x>=0) x

sum' :: [Integer] -> Integer
sum' x = foldl (\x y -> x+y) 0 x

product' :: [Integer] -> Integer
product' x = foldl (\x y -> x*y) 1 x

length' :: [Integer] -> Integer
length' x = foldl (\s _ -> 1+s) 0 x

--Functional Programming
add1Mul4 :: Integer -> Integer
add1Mul4 = ((*4) . (+1))

add1Mul4' :: Integer -> Integer
add1Mul4' x = (*4) $ (+1) x

sumAdd1Mul4_list :: [Integer] -> Integer
sumAdd1Mul4_list = (add1Mul4 . sum')

dup = map (\x -> x*2)
dup' x = map (\x -> x*2) x

--Currying and Partial Application
--Currying: Represent multi-argument functionas as 1-argument functions returning functions
mul2PlusY :: Integer -> Integer -> Integer
mul2PlusY x y = 2*x+y

mul2PlusY' :: Integer -> (Integer -> Integer)
mul2PlusY' x y = 2*x+y

fourPlusY :: Integer -> Integer
fourPlusY = mul2PlusY' 2

mul2XPlusYInPair :: (Integer,Integer) -> Integer
mul2XPlusYInPair (x,y) = 2*x+y

mul2XPlusYInPair' :: Integer -> Integer -> Integer
mul2XPlusYInPair' x y = curry mul2XPlusYInPair x y

mul2XPlusYInPair'' :: (Integer,Integer) -> Integer
mul2XPlusYInPair'' x = uncurry mul2XPlusYInPair' x

addPair :: (Integer,Integer) -> Integer
addPair = uncurry (+)

--Wholemeal Programming

--Only map 7x+2 if x>3 in the list and then add all digits in the list
foobar :: [Integer] -> Integer
foobar x = (sum . map ((+2) . (*7)) . (filter (\n->n>3))) x
