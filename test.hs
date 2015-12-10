doubleMe x = x+x
doubleUs x y = doubleMe x + doubleMe y
doubleNumber x = if x>100 then x else x*2

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
    | mod n 2 == 0  = div n 2
    | otherwise     = 3*n + 1     

isEven :: Integer -> Bool
isEven x = mod x 2 == 0

sumPair :: (Int,Int) -> Int
sumPair (x,y) = x+y

addAll :: Int -> Int -> Int -> Int
addAll a b c = a+b+c

list1, list2, list3 :: [Integer]
list1 = [1,2,3,5675]
list2 = [1..15]
list3 = [2,4..20]
list4 = 1:2:3:4:[]

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength []        = 0
intListLength (x:xs)    = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []          = []
sumEveryTwo [x]         = [x]
sumEveryTwo (x:y:zs)    = (x+y) : sumEveryTwo zs 


bmiTell :: RealFloat x => x -> x -> String
bmiTell weight height
    | bmi <= skinny = "Underweight"
    | bmi <= normal = "Normal"
    | bmi <= fat = "Overweight"
    | bmi > fat = "Obese"
    | otherwise = "No Idea"
    where bmi =weight / (height ^ 2) 
          (skinny,normal,fat) = (18.5,25.0,30.0)

maxnum :: Ord x => x -> x -> x
maxnum x y
    | x>y = x
    | y>x = y

initials :: String -> String -> String
initials first last = [f] ++ "." ++ [l] ++ "."
    where (f:_) = first
          (l:_) = last
