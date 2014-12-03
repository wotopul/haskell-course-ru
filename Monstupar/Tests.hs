module Monstupar.Tests where

import Monstupar.Core
import Monstupar.Derived

import System.Random

--------------------------------------------------------------------------------
-- В помощь хозяйке

mustParse s p = case runParser p s of
    Left  _ -> False
    Right _ -> True

mustFail s = not . mustParse s

infixl 2 &.&
(&.&) p1 p2 x = p1 x && p2 x

--------------------------------------------------------------------------------
-- Тесты

-- Правильная скобочная последовательность
balPar = bp >> eof where
    bp = (do
          char '('
          bp
          char ')'
          bp) <|> ok

balParTest = mustParse ""
         &.& mustFail  "("
         &.& mustFail  ")"
         &.& mustParse "()"
         &.& mustParse "(())()(())()"
         &.& mustFail  "())()(())()"
         &.& mustFail  "(())()(()()"
         &.& mustFail  "())()(()()"
         $ balPar

-- Ровно данная строка
type StringParser = String -> Monstupar Char String

isString :: String -> StringParser -> Monstupar Char ()
isString s p = p s >> eof

genStrings :: Int -> IO [String]
genStrings 0 = return []
genStrings n = do
    gen <- getStdGen
    let len = fst $ randomR (0, 20) gen
    gen' <- newStdGen
    let x = take len $ (randomRs ('a','z') gen') 
    xs <- genStrings (n - 1)
    return (x:xs)

isStringCheck :: [String] -> StringParser -> Bool
isStringCheck xs p = and $ map (\s -> mustParse s (isString s p)) xs

isStringTest :: StringParser -> IO Bool
isStringTest p = do
    xs <- genStrings 40
    return $ isStringCheck xs p

-- Список натуральных чисел
-- тут следует использовать класс Read
natList :: Monstupar Char [Integer]
natList = undefined

natListTest = mustFail  ""
          &.& mustParse "0"
          &.& mustParse "0,1"
          &.& mustFail  "0,1,"
          &.& mustParse "10,20,12,3423,2342,234,2234,2342,22342,22232,17583,9573"
          &.& mustFail  "10,20,12,3423,2342,234,-2234,2342,22342,22232,17583,9573"
          &.& mustFail  "10,20,12,3423,0.,234,234,2342,22342,22232,17583,9573"
          $ natList

