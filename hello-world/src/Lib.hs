module Lib
    ( someFunc, fizzBuzz, doubleUs, playerReview
    ) where

import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

doubleUs :: Int -> Int -> Int
doubleUs x y = 2*x + x*y

--Third attempt at FizzBuzz, defining types explicity,
--  using guards and where bindings, map function
fizzBuzz :: [Int] -> [[Char]]
fizzBuzz xs = map fizzBuzzHelper xs

--Helper function that returns the appropriate string
--  according to FizzBuzz rules
fizzBuzzHelper :: Int -> [Char]
fizzBuzzHelper x
  | mod3 && mod5 = "FizzBuzz"
  | mod3         = "Fizz"
  | mod5         = "Buzz"
  | otherwise    = show x
  where
    mod3 = x `mod` 3 == 0
    mod5 = x `mod` 5 == 0


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

