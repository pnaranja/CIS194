doubleMe x = x+x
doubleUs x y = doubleMe x + doubleMe y
doubleNumber x = if x>100 then x else x*2

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

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
