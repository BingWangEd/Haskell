-- module Main where
import Prelude hiding (reverse
                      , length
                      , elem
                      , map
                      , foldl
                      , foldr
                      )
import Data.Char

jb :: Integer
jb = 5

addOne :: Integer -> Integer
addOne n = n + 1

addThenDouble :: Integer -> Integer -> Integer
addThenDouble n m = (n + m) * 2
-- addThenDouble = \n m -> (n + m) * 2

justAdd :: Integer -> Integer -> Integer
justAdd n m = addThenDouble n m `div` 2

double :: Integer -> Integer
double n = (*) n 2
-- double = \n -> n * 2

res :: Integer
res = 1 `addThenDouble` 2
-- same as res = addThenDouble 1 2

niceTuple :: (Bool, [Char])
niceTuple = (False, "turtle")

makeUpperCase :: [Char] -> [Char]
makeUpperCase a = map toUpper a

five :: Integer
five = let n = 2
       in n + 3

extractTuple :: (((String, Bool), Int), Int) -> String
extractTuple bigTuple = let firstTup = fst bigTuple
                            secTup = fst firstTup
                        in fst secTup
                    
extractTuple2 :: (((String, Bool), Int), Int) -> String
extractTuple2 bigTuple = fst secTup
                         where firstTup = fst bigTuple
                               secTup = fst firstTup

condiValue :: String
condiValue = if True == True then "true == true" else "true!= true"

descendingSum :: Int -> Int
descendingSum 0 = 0
descendingSum n = n + descendingSum (n - 1)

realList :: [Int]
realList = (2: (1 : (0 : [])))

append :: [Int] -> [Int] -> [Int]
-- append lst1 [] = lst1
append [] lst2 = lst2
append (hd : rmFstList) secList = hd : (append rmFstList secList)
-- append firstList [] = firstList
-- append firstList secList = append ((head secList) : firstList) (tail secList)
-- [a] ++ [a]; a <> a

reverseList :: [Int] -> [Int] -> [Int]
-- reverseList [] = []
-- reverseList (hd : rm) = append (reverseList rm) [hd]

reverseList [] acc = acc
reverseList (hd : rm) acc = reverseList rm (hd : acc) -- why is it not `acc ++ [hd]`
reverseList2 list = let reverseList [] acc = acc
                        reverseList (hd : rm) acc = reverseList rm (hd : acc)
                    in reverseList list []
                      
mapToAddOne :: [Int] -> [Int]
mapToAddOne list = map (+1) list

mapToUpper :: [Char] -> [Char]
mapToUpper str = map toUpper str

isOdd :: Int -> Bool
isOdd n = (mod n 2) /= 0 

filterEven :: [Int] -> [Int]
filterEven li = filter isOdd li

-- fold (two-place operation) (a starting value) (a list)
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op acc [] = acc
--foldr op acc (hd : rm) = foldr op (op hd acc) rm -- wrong
-- take the value from the rightmost side of the array to start
foldr op acc (hd : rm) = op hd (foldr op acc rm)
-- foldr (+) 0 [7, 8, 9, 10]
-- 34

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op acc [] = acc
foldl op acc (hd : rm) = foldl op (op acc hd) rm

-- week 1 Homework
length :: [a] -> Int
length [] = 0
length (hd : []) = 1
length (hd : rm) = 1 + length(rm)

length' :: [a] -> Int
length' list = foldl plusOne 0 list
               where plusOne acc v2 = acc + 1

-- 1.2.4
elem :: Eq a => a -> [a] -> Bool
elem v [] = False
elem v (hd : rm) = if v == hd then True
                   else elem v rm

elem' :: Eq a => a -> [a] -> Bool
elem' v list =  foldl ifSame False list
                where ifSame acc entry = if entry == v then True else acc;

-- 1.2.6
map :: (a -> b) -> [a] -> [b]
map func [] = []
map func [a] = [func a]
map func (hd : ta) = [func hd] ++ (map func ta)

map' :: (a -> b) -> [a] -> [b]
map' func [] = []
map' func list = foldl applyFunc [] list
                      where applyFunc acc v = acc ++ [func v]

-- vs. using foldr
-- map' func list = foldr applyFunc [] list
--                      where applyFunc v acc = [func v] ++ acc

-- 1.2.8
append' :: [a] -> [a] -> [a]
append' listA listB = foldr addTo listB listA
                      where addTo v acc = v : acc

-- 1.2.10
maxmin :: [Float] -> (Float, Float)
maxmin (hd : rm) = (max, min)
            where getSmaller a b = if a < b then a else b
                  getGreater a b = if a < b then b else a
                  max = foldl getGreater hd rm
                  min = foldl getSmaller hd rm

minmax' :: [Float] -> (Float, Float)
minmax' lst = let
                      infinity = 1 / 0
                      getSmaller :: Float -> Float -> Float
                      getSmaller a b = if a < b then a else b
                      getGreater :: Float -> Float -> Float
                      getGreater a b = if a < b then b else a
                    in foldr
                      (\n (min, max) -> (getSmaller n min, getGreater n max))
                      (infinity, 0)
                      lst

append :: [a] -> [a] -> [a]
append list1 [] = list1
append [] list2 = list2
append (x : xs) list2 = x : (append xs list2)

reverse' :: [a] -> [a] -> [a]
reverse' [] acc = acc
reverse' (x : xs) acc = reverse' xs (x : acc)
reverse :: [a] -> [a]
-- reverse list = reverse' list []

-- foldr (+) 0 [7, 8, 9, 10]
-- 34

-- reverse list = let appendList:: [a] -> a -> [a]
                    -- appendList accList item = item : accList
                    -- in foldl appendList [] list

reverse list = let appendList :: a -> [a] -> [a]
                   appendList item accList = append accList [item]
              in foldr appendList [] list

maxmin :: [Float] -> (Float, Float)
maxmin list = foldl getCurrMaxMin (0, infinity) list
              where infinity = 1/0
                    getCurrMaxMin :: (Float, Float) -> Float -> (Float, Float)
                    getCurrMaxMin (currMax, currMin) val = (newMax, newMin)
                                                          where newMax = if currMax > val then currMax else val
                                                                newMin = if currMin < val then currMin else val

main :: IO ()
main = do
  putStrLn "hello world"
