module Main where

import Lib

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


