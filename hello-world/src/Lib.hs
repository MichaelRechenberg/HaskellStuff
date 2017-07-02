module Lib
    ( someFunc, fizzBuzz, doubleUs
    ) where

import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

doubleUs :: Int -> Int -> Int
doubleUs x y = 2*x + x*y

-- Bad first attempt at FizzBuzz in Haskell
fizzBuzz xs = [if (x `mod` 3 == 0 && x `mod` 5 == 0) then "FizzBuzz" 
              else if x `mod` 3 == 0 then "Fizz"
              else if x `mod` 15 == 0 then "Buzz"
              else "Invalid"
              | x <- xs]



