{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show, Read)

import ITMOPrelude.Algebra

-- Всё что угодно, главное, чтобы соответствовало
-- заданию

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Read)

undefined = undefined

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton a = Node a Leaf Leaf

insertLeft :: a -> Tree a -> Tree a
insertLeft a Leaf = Node a Leaf Leaf
insertLeft a (Node x l r) = Node x (insertLeft a l) r

insertRight :: a -> Tree a -> Tree a
insertRight a Leaf = Node a Leaf Leaf
insertRight a (Node x l r) = Node x l (insertRight a r)

rotateLeft :: Tree a -> Tree a
rotateLeft (Node p a (Node q b c)) = Node q (Node p a b) c

rotateRight :: Tree a -> Tree a
rotateRight (Node q (Node p a b) c) = Node p a (Node q b c)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Leaf = Leaf
treeMap f (Node x l r) = Node (f x) (treeMap f l) (treeMap f r)

foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
foldMap = undefined

