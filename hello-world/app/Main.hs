module Main where

import Lib
import Sorts


main :: IO ()
main = do
  print(zip [1..100] (fizzBuzz [1..100]))
  -- Should print "He's a superstar"
  print(playerReview ("Mike", "Cubs", 20) )
  -- Should print "Cody is garbage"
  print(playerReview ("Cody", "Packers", 20) )
  -- Should print "Cody is a bit young, but perhaps Bears needs
  --    new blood"
  print(playerReview ("Cody", "Bears", 20) )
  -- Should print "Can't say anything definitively"
  print(playerReview ("Uncle Bob", "Bears", 55) )
  -- Should print "I didn't know he played"
  print(playerReview ("Uncle Bob", "Cubs", 55) )
  print(playerReview ("ASDF", "Invalid", (-1)) )
  let asdf = "a quick brown fox jumped over the lazy dog"
    in print(quickSort asdf)
  let asdf = "a quick brown fox jumped over the lazy dog"
    in print(mergeSort asdf)
  let bob = Person {firstName="bob", lastName="brown", age=27, single=True}
    in print(describePerson bob)
  let input = [4, 0, -1, -44, 2, 9] in
    print(map printValidity $ map safeSqrt input)




