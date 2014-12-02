{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero     Zero     = EQ
natCmp Zero     (Succ _) = LT
natCmp (Succ _) Zero     = GT
natCmp (Succ n) (Succ m) = natCmp n m

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq n m = case (n `natCmp` m) of EQ -> True
                                   _  -> False

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt n m = case (n `natCmp` m) of LT -> True
                                   _  -> False

-- n больше m
natGt :: Nat -> Nat -> Bool
natGt n m = case (n `natCmp` m) of GT -> True
                                   _  -> False

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
Zero     -. _        = Zero
n        -. Zero     = n
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n m = case (n `natCmp` m) of LT -> Pair natZero n
                                       EQ -> Pair natOne natZero
                                       GT -> Pair (Succ q) r where
                                          Pair q r = (n -. m) `natDivMod` m

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd a Zero = a
gcd a b = gcd b (a `natMod` b)

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Neg Nat | Pos Nat deriving (Show,Read)

intZero   = Pos natZero -- 0
intOne    = Pos natOne  -- 1
intNegOne = Neg natZero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero) = Pos Zero
intNeg (Pos (Succ n)) = Neg n
intNeg (Neg n) = Pos (Succ n)

abs :: Int -> Nat
abs (Pos n) = n
abs (Neg n) = Succ n

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Neg n) (Neg m) = natCmp m n
intCmp (Pos n) (Pos m) = natCmp n m
intCmp (Neg _) (Pos _) = LT
intCmp (Pos _) (Neg _) = GT

intEq :: Int -> Int -> Bool
intEq n m = case (n `intCmp` m) of EQ -> True
                                   _  -> False

intLt :: Int -> Int -> Bool
intLt n m = case (n `intCmp` m) of LT -> True
                                   _  -> False

intGt :: Int -> Int -> Bool
intGt n m = case (n `intCmp` m) of GT -> True
                                   _  -> False

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Pos n) .+. (Pos m) = Pos (n +. m)
(Neg n) .+. (Neg m) = Neg (Succ (n +. m))
(Pos n) .+. (Neg m) = case natCmp n (Succ m) of LT -> Neg (m -. n)
                                                _  -> Pos (n -. Succ m)
n .+. m = m .+. n

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Pos n) .*. (Pos m) = Pos (n *. m)
n .*. m@(Neg _) = intNeg (n .*. intNeg m)
n .*. m = m .*. n

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat deriving (Show, Read)

ratZero   = Rat intZero   natOne
ratOne    = Rat intOne    natOne
ratNegOne = Rat intNegOne natOne

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Pos x) y) = Rat (Pos y) x
ratInv q = ratNeg . ratInv . ratNeg $ q

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a b) (Rat c d) = intCmp (a .*. Pos d) (Pos b .*. c)

ratEq :: Rat -> Rat -> Bool
ratEq n m = case (n `ratCmp` m) of EQ -> True
                                   _  -> False

ratLt :: Rat -> Rat -> Bool
ratLt n m = case (n `ratCmp` m) of LT -> True
                                   _  -> False

ratGt :: Rat -> Rat -> Bool
ratGt n m = case (n `ratCmp` m) of GT -> True
                                   _  -> False

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat a b) %+ (Rat c d) =
    let nominator   = a .*. Pos d .+. Pos b .*. c
        denominator = b *. d
    in Rat nominator denominator

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat a b) %* (Rat c d) =
    let nominator   = a .*. c
        denominator = b *. d
    in Rat nominator denominator

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

toLowestTerms :: Rat -> Rat
toLowestTerms (Rat (Pos x) y) = Rat (Pos (x `natDiv` q)) (y `natDiv` q)
    where q = gcd x y
toLowestTerms q = ratNeg . toLowestTerms . ratNeg $ q

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

id = \x -> x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
