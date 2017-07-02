module Lib
    ( someFunc, fizzBuzz, doubleUs, playerReview
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

--Silly function that returns a "review" of the player
-- (Player name, team, age)
--Meant to concretize patterns
playerReview :: (String, String, Int) -> String
playerReview ("Mike", _, _) = "He's a superstar"
playerReview (name, "Packers", _) =  name ++ " is garbage"
playerReview (name, team, 20) = name ++ " is bit young, but perhaps " 
                                ++ team ++ " needs some new blood"
playerReview ("Uncle Bob", "Cubs", _) = "I didn't know he played"
playerReview x = "Can't say anything definitively"



