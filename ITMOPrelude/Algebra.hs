{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPredule.Algebra where

import Prelude (Show, Read)

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
-- всевозможные инстансы для классов ниже 

-- Если не страшно, то реализуйте их и для
import ITMOPrelude.List
import ITMOPrelude.Tree

-- Классы
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Monoid a => Group a where
    ginv :: a -> a

-- Инстансы писать сюда

instance Monoid Unit where
    mempty = Unit
    Unit `mappend` Unit = Unit

instance Group Unit where
    ginv Unit = Unit

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = Pair mempty mempty
    Pair a b `mappend` Pair a' b' = Pair (a `mappend` a') (b `mappend` b')

instance (Group a, Group b) => Group (Pair a b) where
    ginv (Pair a b) = Pair (ginv a) (ginv b)

instance (Monoid a) => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

-- Моноид Maybe, позволяющий получить первый Just
newtype First a = First { getFirst :: Maybe a } deriving (Show, Read)

instance Monoid (First a) where
    mempty = First Nothing
    f@(First (Just a)) `mappend` _ = f
    First Nothing `mappend` f = f

-- Моноид Maybe, позволяющий получить последний Just
newtype Last a = Last { getLast :: Maybe a } deriving (Show, Read)

instance Monoid (Last a) where
    mempty = Last Nothing
    _ `mappend` l@(Last (Just a)) = l
    l `mappend` Last Nothing = l

newtype Any = Any { getAny :: Bool } deriving (Show, Read)

instance Monoid Any where
    mempty = Any False
    Any a `mappend` Any b = Any (a || b)

newtype All = All { getAll :: Bool } deriving (Show, Read)

instance Monoid All where
    mempty = All True
    All a `mappend` All b = All (a && b)
