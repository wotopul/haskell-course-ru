-- Extremely simple but monstrously stupid (and slow) monadic parser
-- combinator library
module Monstupar.Core
    ( ParseError(..)
    , Monstupar, runParser
    , ok, isnot, eof, (<|>), like
    ) where

--------------------------------------------------------------------------------
-- Определения

-- Тело этого определения можно заменить на всё, что захочется
data ParseError = ParseError String
                deriving (Show) -- лишь бы show был

newtype Monstupar s a = Monstupar { runParser :: [s] -> Either ParseError ([s], a) }

instance Monad (Monstupar s) where
    return a = Monstupar $ \s -> Right (s, a)
    (Monstupar g) >>= f = Monstupar $ \s -> case g s of
        Right (s', a) -> h s' where Monstupar h = f a
        Left e -> Left e

--------------------------------------------------------------------------------
-- Примитивные парсеры.
-- Имена и сигнатуры функций менять нельзя, тела можно

-- Всё хорошо
ok :: Monstupar s ()
ok = Monstupar $ \s -> Right (s, ())

-- Не должно парситься парсером p
isnot :: Monstupar s () -> Monstupar s ()
isnot p = Monstupar $ \s -> case runParser p s of
    Left e  -> Right (s, ())
    Right _ -> Left $ ParseError "Failure expected"

-- Конец ввода
eof :: Monstupar s ()
eof = Monstupar $ \s -> case s of
    [] -> Right (s, ())
    _  -> Left $ ParseError "Eof expected"

infixr 2 <|>
-- Сначала первый парсер, если он фейлится, то второй
(<|>) :: Monstupar s a -> Monstupar s a -> Monstupar s a
a <|> b = Monstupar $ \s -> case runParser a s of
    Left _  -> runParser b s
    Right p -> Right p

-- В голове ввода сейчас нечто, удовлетворяющее p
like :: (s -> Bool) -> Monstupar s s
like p = Monstupar $ \s -> case s of
    []   -> Left $ ParseError "Eof reached"
    x:xs -> if p x then Right (xs, x)
                   else Left $ ParseError "Matching in like failed"

-- Сюда можно добавлять ещё какие-то примитивные парсеры
-- если они понадобятся

