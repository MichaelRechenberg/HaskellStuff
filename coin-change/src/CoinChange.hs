module CoinChange
    (
     coinChange, coinChangeR
    )
    where


import Data.List


--Used for `trace` in debugging
import Debug.Trace

--TODO make memoized version

--coinChange initialAmt coins
--  initialAmt: The starting initial amount, must be a positive number > 0 or -1 is returned
--  coins: List of all the 'coins' (denominations of coins), all coins are unique values
--
--Returns the amount of possible combinations as a positive int (or -1 if an error occurred)
coinChange :: Int -> [Int] -> Int
coinChange initialAmt coins 
  | initialAmt <= 0 || coins == [] = -1
  | otherwise  = coinChangeR initialAmt coins



--coinChangeR initialAmt Coins
coinChangeR :: Int -> [Int] -> Int
coinChangeR initialAmt coins
  -- Invalid arguemnts
  | initialAmt < 0 || coins == [] = 0
  -- Found a solution!
  | initialAmt == 0 = 1
  -- Search subproblems
  | otherwise = coinUsed + coinNotUsed
  where 
    coinUsed = coinChangeR (initialAmt - head coins) coins
    coinNotUsed = coinChangeR initialAmt (tail coins)
