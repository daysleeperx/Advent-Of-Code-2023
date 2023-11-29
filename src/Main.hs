module Main (main) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node root left right) = Node (f root) (fmap f left) (fmap f right)

t1 :: Tree Int
t1 = Node 1 (Node 2 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree)

t2 :: Tree Int
t2 = fmap (+ 1) t1

main :: IO ()
main = do
  print t2
