module Main where
import Prelude hiding (even
                      )
import Data.Char

------ SUM TYPE ------
plusOne :: Integer -> Integer
-- plusOne n = n + 1
-- = point free style
plusOne = (+1)

-- doubleThenTriple :: Integer -> Integer
-- doubleThenTriple = triple . double -- dot can be understood as after

-- non-functional data values
data Fruit -- all needs to be captalized: sum type
  = Cherry -- data constructors
  | Strawberry
  | Orange
  | Pretzel
  deriving Show -- Q: what is this?

nextFruit :: Fruit -> Fruit
nextFruit Cherry = Strawberry
nextFruit Strawberry = Orange
nextFruit Orange = Pretzel
nextFruit Pretzel = Pretzel

-- pattern matching
-- functional data values
data BoolOrString
  = B Bool
  | S String
  deriving Show

-- S "matt"
-- B False

lengthOrTruthValue :: BoolOrString -> Int
lengthOrTruthValue x
  = case x of 
      S str -> length str
      B True -> 1
      B False -> 0

even :: Integer -> Bool
even x = case x `mod` 2 of 
  0 -> True
  _ -> False

doubleString :: BoolOrString -> BoolOrString
-- instead of writing it like: doubleString (B bool) = B bool; use @ sign as-pattern
doubleString b@(B _) = b
doubleString (S str) = S (str ++ str)
-- doubleString (S "str")
-- doubleString (B True)

-- pattern guards
amIEven :: Integer -> String
amIEven n
  | n `mod` 2 == 0 = "Yes, I'm even"
  | otherwise = "No, I'm not"

------ PRODUCT TYPE ------
-- data Both
 -- = Both Bool String
 -- deriving Show

-- polymorphic
data Both a b
 = Both a b
 deriving Show

-- Both "stuff" 2

-- Tuple's def
-- data (,) a b -- Q: How does it go from (,) a b to (a, b)
 -- = (,) a b
 -- deriving Show

-- ("stuff", 2)

data RecordBoth -- type
 = RecordBoth { bool :: Bool -- type definition; RecordBoth is data constructor. It can be any name.
              , str :: String }
   deriving Show

-- trueGiraffe = RecordBoth { bool = True, str = "giraffe" }
trueGiraffe = RecordBoth True "giraffe"
-- bool trueGiraffe
-- str trueGiraffe

data Camel
 = Camel { name :: String
         , numberOfHumps :: Integer
         , isFriendly :: Bool }
   deriving Show

joe = Camel "Joe Camel" 2 True

joeLater = joe { isFriendly = False }

-- data IBinTree
  -- = Final Integer
  -- | Intermediate Integer IBinTree IBinTree

-- data BinTree a
  -- = Final a
  -- | Intermediate a (BinTree a) (BinTree a)

data NaryTree a
  = Final a
  | Intermediate a [NaryTree a]

---------- Illegal State; No Representation ---------
placeCall :: String -> IO ()
placeCall number = putStrLn $ -- Q: what is $ sign for?
  "Dialing \n"
  ++ number ++
  "in browser."

rightLength :: String -> Bool
rightLength str = length str == 7

justDigits :: String -> Bool
justDigits str = all isNumber str

data Digit
  = Zero | One | Two | Three -- ...
  deriving Show

data ODigit
  = OOne | OTwo | OThree -- ... (no Zero)
  deriving Show

data OfficeCode
  = OC ODigit Digit Digit
  deriving Show

data PhoneNumber
  = PN Digit Digit Digit
  deriving Show
example = PN Zero One Two

-- if not (justDigits str)
-- pattern guards or case expression to do if else branch checking
-- functional core/imperative shell

-- use pattern guard or case expression for nested / if else checks


main :: IO ()
main = do
  putStrLn "hello world"
