{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories hiding ((.), id)
import ITMOPrelude.Primitive ((.), id)

-- Эти
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- делаем нас

instance Monad m => Functor m where
    fmap f x = x >>= (return . f)

instance Monad m => MonadJoin m where
    returnJoin = return
    join x = (x >>= id)

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g = \x -> (f x >>= g)
