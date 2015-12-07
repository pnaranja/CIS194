--Exercise 1
lastDigit :: Integer -> Integer
lastDigit x         = mod x 10

dropLastDigit :: Integer -> Integer
dropLastDigit x     = div x 10

--Exercise 2
toRevDigits :: Integer -> [Integer]
toRevDigits 0       = []
toRevDigits x       = (lastDigit x) : toRevDigits (dropLastDigit x)
