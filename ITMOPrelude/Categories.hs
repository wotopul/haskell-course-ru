{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree
-- всевозможные инстансы для классов ниже

--------------------------------------------------------------------------------
-- Классы
class Category cat where
    id  :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\_ -> mb)

--------------------------------------------------------------------------------
-- Инстансы писать сюда

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)

instance Functor (Either e) where
    fmap f (Left e) = Left e
    fmap f (Right a) = Right (f a)

instance Functor List where
    fmap = map

instance Functor Tree where
    fmap = treeMap

instance Monad Maybe where
    return = Just
    Nothing >>= f = Nothing
    Just a >>= f = f a

instance Monad (Either e) where
    return = Right
    Left e >>= f = Left e
    Right a >>= f = f a

instance Monad List where
    return a = Cons a Nil
    xs >>= f = concatMap f xs

--------------------------------------------------------------------------------
-- Монада State

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return a = State $ \s -> (s, a)
    (State g) >>= f = State $ \s -> let (s', a) = g s
                                        (State h) = f a
                                    in h s'
