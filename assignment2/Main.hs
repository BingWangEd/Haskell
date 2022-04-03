module Main where

-- Lab
-- 1.1 Trees
-- pattern matching
data Tree a = Node a [Tree a]
              deriving Show

-- 1.1.1
tree1 :: Tree Integer
tree1 = Node 0 [Node 1 [Node 2 [], Node 4 []], Node 3 []]

-- 1.1.2
tree2 :: Tree String
tree2 = Node "cat" []

-- 1.1.3
tree3 :: Tree Bool
tree3 = Node True [Node False [Node False [], Node True [], Node False []]]

-- 1.1.4
treeElem :: Eq a => a -> Tree a -> Bool
treeElem a (Node currValue []) = if currValue /=a then False else True
treeElem a (Node currValue nextTrees)
 | currValue /= a = elem True (map searchA nextTrees)
 | otherwise = True
 where searchA tree = treeElem a tree

-- 1.1.5
depth :: Tree a -> Integer
depth (Node _ []) = 1
depth (Node _ trees) = 1 + (maximum (map depth trees))

-- 1.1.6
combine :: Tree Integer -> Tree Integer -> Tree Integer
combine (Node v1 []) (Node v2 []) = Node (v1 + v2) []
-- combine (Node v1 []) nil = Node v1 []
-- combine nil (Node v2 []) = Node v2 []
combine (Node v1 tree1) (Node v2 tree2) = Node (v1 + v2) (map combinePair (zipTrees tree1 tree2))
                                          where combinePair (n1, n2) = combine n1 n2
                                                              
zipTrees :: [Tree Integer] -> [Tree Integer] -> [(Tree Integer, Tree Integer)]
zipTrees tree1 tree2 = if l1 > l2 then zip tree1 (tree2 ++ replicate (l1 - l2) (Node 0 []))
                       else zip (tree1 ++ replicate (l2 - l1) (Node 0 [])) tree2
                          where l1 = length tree1
                                l2 = length tree2

main :: IO ()
main = do
  putStrLn "hello world"
