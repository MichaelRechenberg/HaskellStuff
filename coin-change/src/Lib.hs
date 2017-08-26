module Lib
    ( someFunc,
      simpleAdd
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--Example of multiple arg functions
simpleAdd :: Integer -> Integer -> Integer
simpleAdd x y = x + y
