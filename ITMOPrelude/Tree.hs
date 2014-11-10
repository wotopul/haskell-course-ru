{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show, Read, Eq, (==), (<), (>))
import ITMOPrelude.Primitive

-- Всё что угодно, главное, чтобы соответствовало
-- заданию

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x Leaf Leaf

elem :: a -> Tree a -> Bool
elem _ Leaf = False
elem x (Node a left right)
    | x == a = True
    | x < a  = elem x left
    | x > a  = elem x right
