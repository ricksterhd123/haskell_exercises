-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||), (&&), gcd)

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2  ||

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True    = True
False || True   = True
True || False   = True
False || False  = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a


fact :: Int -> Int
fact n
    | n == 0    = 1
    | n > 0     = n * fact (n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
    | n == 0        = 0
    | n > 0         = m + mult (n - 1) m

divide :: Int -> Int -> Int
divide n m
    | n < m         = 0
    | otherwise     = 1 + divide (n - m) m

-- make && right associative
infixr 3 &&

(&&) :: Bool -> Bool -> Bool
False && False = False
a && b = if a == b then True else False

exOr :: Bool -> Bool -> Bool
exOr a b = if (a == b) then False else True

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a b = a
ifThenElse False a b = b

-- 1, 3, 5, 7, 8, 10, 12 = 31
-- 4, 6, 9, 11 = 30
-- 2 = 28
daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth a = if a /= 9 && a /= 11 && (a `mod` 2 /= 0 || a == 8 || a == 10 || a == 12) then 31 else 30

validDate :: Int -> Int -> Bool
validDate day month = day >= 1 && day <= daysInMonth month

sumNumbers :: Int -> Int
-- sumNumbers n
--     | n > 0 = n + sumNumbers (n - 1)
--     | otherwise = 0

-- without guards:
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

sumSquares :: Int -> Int

-- sumSquares n
--     | n > 0 = n^2 + sumSquares (n - 1)
--     | otherwise = 0

-- without guards
sumSquares 0 = 0
sumSquares n = n^2 + sumSquares (n - 1)

power :: Int -> Int -> Int

-- power n m
--     | m > 0 = n * power n (m - 1)
--     | otherwise = 1

-- without guards
power n 0 = 1
power n m = n * power n (m - 1)

sumFromTo :: Int -> Int -> Int
sumFromTo lower upper
    | lower <= upper = upper + sumFromTo lower (upper - 1)
    | otherwise = 0

gcd :: Int -> Int -> Int
gcd a b
    | b > 0 = gcd b (a `mod` b)
    | otherwise = a

intSquareRoot :: Int -> Int
intSquareRoot x
    | x >= 1 = floor (sqrt (fromIntegral x))
    | otherwise = error "Undefined for zero or negative integers"