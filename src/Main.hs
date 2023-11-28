module Main (main) where

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node left root right) = Node (fmap f left) (f root) (fmap f right)

t1 :: Tree Int
t1 = Node (Leaf 1) 2 (Leaf 3)

t2 :: Tree Int
t2 = fmap (+ 1) t1

main :: IO ()
main = do
  print t2
