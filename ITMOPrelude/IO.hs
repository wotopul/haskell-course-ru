{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State $ \(RealWorld sIn sOut code) -> case sIn of
    Cons x xs -> (RealWorld xs sOut code, x)
    Nil -> undefined

putNat :: Nat -> IO ()
putNat n = State $ \(RealWorld sIn sOut code) ->
    (RealWorld sIn (Cons n sOut) code, ())

setExitCode :: Nat -> IO ()
setExitCode newCode = State $ \(RealWorld sIn sOut code) ->
    (RealWorld sIn sOut newCode, ())
