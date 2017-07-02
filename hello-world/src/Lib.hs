module Lib
    ( someFunc, fizzBuzz, doubleUs
    ) where

import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

doubleUs :: Int -> Int -> Int
doubleUs x y = 2*x + x*y

--Second attempt and FizzBuzz, defining types explicitly
--TODO: Use map to apply a function to each element of the list,
--  and have that function do the conversion logic
fizzBuzz :: [Int] -> [[Char]]
fizzBuzz xs = [if (x `mod` 3 == 0 && x `mod` 5 == 0) then "FizzBuzz" 
              else if x `mod` 3 == 0 then "Fizz"
              else if x `mod` 15 == 0 then "Buzz"
              else show x 
              | x <- xs]



