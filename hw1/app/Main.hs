module Main where

import HW01

verify :: Bool -> String
verify x
    | x == True  = "Valid Credit Card"
    | x == False = "Invalid Credit Card"

main :: IO ()
main = do {
    putStrLn ("Enter a credit card number: ");
    ccnumberstr <- getLine;

    putStrLn (verify (luhn (read ccnumberstr :: Integer)));

    }
