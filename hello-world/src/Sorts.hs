module Sorts
    ( quickSort, mergeSort)
    where

-- a sorted list is a list that has all the values smaller than 
--  (or equal to) the head of the
--  list in front (and those values are sorted), 
--  then comes the head of the list 
--  in the middle and then come all the values that are bigger 
--  than the head (they’re also sorted).
-- To get head versus tail, we use the (x:xs) pattern
-- https://stackoverflow.com/questions/10167910/how-does-quicksort-in-haskell-work


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
--Our choice of the "pivot" is the head
quickSort (x:xs) = 
    -- anti-PLOP: we don't care about array indices
    --    or doing any swaps or elements, we just say
    --    ("run quickSort on a list of elements smaller
    --    than x")
    let smallerSorted = quickSort [a | a <- xs, a <= x]
        largerSorted = quickSort [a | a <- xs, a > x]
    -- Here we just say:
    --  * put the elements smaller than x to the left
    --      of x (and have those elems be sorted)
    --  * put the elements larger than x to the right
    --      of x (and have those elems be sorted)
    in smallerSorted ++ [x] ++ largerSorted

-- 5-liner quicksort that uses filter 
-- http://www.kovach.me/posts/2012-04-03-sorting.html


-- mergeSort from https://smthngsmwhr.wordpress.com/2012/11/09/sorting-algorithms-in-haskell/

mergeSort'merge :: (Ord a) => [a] -> [a] -> [a]
mergeSort'merge [] xs = xs
mergeSort'merge xs [] = xs
mergeSort'merge (x:xs) (y:ys)
    | (x < y) = x:mergeSort'merge xs (y:ys)
    | otherwise = y:mergeSort'merge (x:xs) ys
 
mergeSort'splitinhalf :: [a] -> ([a], [a])
mergeSort'splitinhalf xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2 
 
mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs 
    | (length xs) > 1 = mergeSort'merge (mergeSort ls) (mergeSort rs)
    | otherwise = xs
    where (ls, rs) = mergeSort'splitinhalf xs





