{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

-- Эти
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- делаем нас

instance Monad m => Functor m where
    fmap f x = x >>= return . f

instance Monad m => MonadJoin m where
    retutnJoin = return
    join x = (x >>= id)

instance Monad m => MonadFish m where
    returnFish = return
    f >=> f = \x -> (f x >>= g)
