module Main where

import Lib

main :: IO ()
main = do
  print(zip [1..100] (fizzBuzz [1..100]))


