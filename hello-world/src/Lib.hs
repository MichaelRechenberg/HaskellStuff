module Lib
    ( someFunc, 
      fizzBuzz, 
      doubleUs, 
      playerReview, 
      storyLengthr, 
      storyLengthl
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


--Note how the difference in lambda expressions in the 2 functions
--  below (reflective of the type definitions for foldl and foldr)

--Calculates the 'total' length of a list of strings (a story)
--  by adding up the length of each string to the overall total
--  using foldr
storyLengthr :: [[Char]] -> Int
storyLengthr [] = 0
storyLengthr xs = foldr (\phrase accum -> length phrase + accum) 0 xs


--Calculates the 'total' length of a list of strings (a story)
--  by adding up the length of each string to the overall total
--  using foldl
storyLengthl :: [[Char]] -> Int
storyLengthl [] = 0
storyLengthl xs = foldl (\accum phrase -> length phrase + accum) 0 xs



