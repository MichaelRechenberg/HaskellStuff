module Lib
    ( someFunc, 
      fizzBuzz, 
      doubleUs, 
      playerReview, 
      storyLengthr, 
      storyLengthl,
      myCompress,
      makeClosure, 
      pack,
      encode,
      --The (..) means to allow all ctors of Person 
      --  to be exported
      Person(..),
      describePerson,
      safeSqrt,
      printValidity
    ) where

import Data.Char
import Data.List
import Text.Printf

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


--Haskell 99?'s #7, solution taken from 99 questions solution
--Look at concat function, concatMap
--concat: concat all elements in a container of lists
--concatMap: Map a function over all elements in a container and concatenate the results
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x


--Haskell 99?'s #8
--Remove neighboring duplicate entries from a list
--myCompress "aaabccaaddeee" == "abcade"
myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress x = foldr (\z accum -> if z == (head accum) then accum else z:accum) [last x] x



--Haskell 99?'s #9 (Solution modified from cheat sheet)
--Pack consecutive uplicates of list elements into sublists
-- pack['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa", "b", "cc", "aa", "d", "eeee"]

--span will take a list and a predicate and return a list of 2 lists,
--  the first of which is the longest prefix for which the prefix held
--  and the second is the remainder of the input list

--Note: The Haskell library function Data.List.group does exactly this
pack :: (Eq a) => [a] -> [[a]]
pack (x:xs) = 
          let (first, rest) = span (==x) xs
          in 
            if null rest then
              [(x:first)]
            else
              (x:first) : pack rest
pack []     = [[]]



--Haskell 99?'s #10 
--Length-encode a list
--encode "aaaabccaadeeee"
--[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs = 
            let genTuple = (\x -> ((length x), (head x)))
            in
              map genTuple $ pack xs


        


--Closure example
--x will be bound to the closure when makeClosure is called
--y is a free variable  
--
--bind 7 to x
--add7 = makeClosure 7
--
--bind 32 to y
--add7 32
--39
--bind 21 to y, x is still bound to 7 for the closure
--add7 21
--28
makeClosure :: (Num a) => a -> a -> a
makeClosure x = (\y -> (x + y))



--Chapter 8 Practice: Making ADT

data Person = Person { firstName :: String,
                       lastName  :: String,
                       age       :: Int,
                       single    :: Bool}

describePerson :: Person -> String
describePerson (Person {firstName=fn, lastName=ln, age=age, single=_}) =
  printf "%s %s is %d years old...and I won't say this person's relationship status" fn ln age


--Getting used to the Either type
--Finds the sqrt of a Float:
--  Right x if x >= 0
--  Left errorMsg if x < 0
safeSqrt :: Float -> Either String Float
safeSqrt x
  | x < 0 = Left "ERROR: Negative Number"
  | otherwise = Right $ sqrt x

--Return a string displaying the value or an error message
printValidity :: (Show a) => Either String a -> String
printValidity (Right x) = printf "The valid value was %s" (show x)
printValidity (Left errMsg) = printf "Error Message: %s" errMsg


