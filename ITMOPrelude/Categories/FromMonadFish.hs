{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadFish where
import ITMOPrelude.Categories.MonadFish

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin

-- делаем из нас
instance MonadFish m => Monad where
    return = returnFish
    f >>= g = (id >=> g) f

instance MonadFish m => Functor m where
    fmap f x = id >=> (return . f)

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = id >=> id
