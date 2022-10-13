divide :: Integer -> Integer -> Integer
divide x y = x `div` y

main :: IO ()
main =  do

putStr "Answer of x / y = "
print(divide 10 0)



divide :: Integer -> Integer -> Maybe Integer
divide _ 0 = Nothing
divide x y = Just $ x `div` y

main :: IO ()
main =  do

putStr "Answer of x / y = "
print(divide 10 0)
