{-# Language ScopedTypeVariables #-}

module Main where

import Data.Char

import Prelude hiding ( filter
                      , takeWhile
                      , dropWhile
                      , nub
                      )

-- 2.1 Short Exercises

-- filter func [] = []
-- filter func (x : xs) = if func x then (x : filter func xs) else filter func xs
filter :: (a -> Bool) -> [a] -> [a]
filter func list = foldl applyFunc [] list
                   -- where applyFunc :: [a] -> a -> [a] Q: it would complain
                      where applyFunc acc item = if func item then (item : acc) else acc

-- Q: \_ -> False what does this func mean?
takeWhile :: forall a. (a -> Bool) -> [a] -> [a] -- Q: not sure if this is the best way
takeWhile func list = res
                      where (ifContinue, res) = foldl applyFunc (True, []) list
                                                -- where applyFunc :: (Bool, [a]) -> [a] -> [a]
                                                where applyFunc (ifContinue, curr_list) item = if ifContinue && func item then (True, curr_list ++ [item]) else (False, curr_list)

-- takeWhile func [] = []
-- takeWhile func (x : xs) = if func x then (x : takeWhile func xs) else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile func list = res
                      where (ifContinue, res) = foldl applyFunc (True, []) list
                                                where applyFunc (ifContinue, curr_list) item = if ifContinue && func item then (True, curr_list) else (False, curr_list ++ [item])
-- dropWhile func [] = []
-- dropWhile func (x : xs) = if func x then dropWhile func xs else (x : xs)


(\\) :: Eq a => [a] -> [a] -> [a] -- Q: what does Eq mean?
(\\) = diff
diff listA listB = foldr applyFunc [] listA
                   where applyFunc item acc = if ifExist item listB then acc else (item : acc)
                                              where ifExist item li = foldl applyFunc2 False li
                                                                      where applyFunc2 acc list_item = if acc || item == list_item then True else False

-- diff [] list = []
-- diff (x1 : xs1) list = if ifExist x1 list == False then (x1 : diff xs1 list) else diff xs1 list
                       -- where ifExist item li = foldl applyFunc False list -- Q: Can you pause foldl?
                                               -- where applyFunc acc list_item = if acc || item == list_item then True else False

ifExists item list = foldl applyFunc False list
                     where applyFunc acc list_item = if acc || item == list_item then True else False

nub :: Eq a => [a] -> [a]
nub list = foldl applyFunc [] list
           where applyFunc acc item = if ifExists item acc then acc else (acc ++ [item])

-- nub' [] list = list
-- nub' (x : xs) acc = if (ifExists x acc) then (nub' xs acc) else nub' xs (acc ++ [x])
-- nub list = nub' list []

-- 2.2 Do it whatever you want
partition :: [Integer] -> 
              Integer ->
             ([Integer], Integer, [Integer])
partition list pivot = foldl sort ([], pivot, []) list
                       where sort :: ([Integer], Integer, [Integer]) -> Integer -> ([Integer], Integer, [Integer])
                             sort (leftList, p, rightList) item = if p > item then (leftList ++ [item], p, rightList) else if p == item then (leftList, p, rightList) else  (leftList, p, rightList ++ [item])

quicksort :: [Integer] -> [Integer]
quicksort [] = []
quicksort (x : xs) = quicksort left ++ [p] ++ quicksort right
                      where (left, p, right) = partition xs x

main :: IO ()
main = do
  putStrLn "hello world"
