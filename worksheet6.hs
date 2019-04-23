{- Week6.hs
 This module illustrates the use of functions as values
-}

import Prelude hiding (reverse)
import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double = multiply 2

doubleAll = map (*2)
areDigits = map isDigit

keepPositive = filter (>0)
keepDigits = filter isDigit

addUp :: Num a => [a] -> a
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

mult10 :: [Int] -> [Int]
mult10 lst = map (*10) lst

onlyLowerCase :: String -> String
onlyLowerCase str = filter (isLower) str

orAll :: [Bool] -> Bool
orAll lst = foldr (||) False lst

sumSquares :: [Int] -> Int
sumSquares lst = foldr (+) 0 (map (^2) lst)

zeroToTen :: [Int] -> [Int]
zeroToTen lst = filter (<=10) (filter (>=0) lst)

squareRoots :: [Float] -> [Float]
squareRoots lst = map (sqrt) (filter (>0) lst)

countBetween :: Int -> Int -> [Int] -> Int
countBetween lower upper lst = length (filter (<=upper) (filter (>=lower) lst))

alwaysPositive :: (Int -> Int) -> [Int] -> Bool
alwaysPositive f lst = (length lst) == length (filter (>0) (map f lst))

productSquareRoots :: [Float] -> Float
productSquareRoots lst = foldr (*) 1 (filter (>0) (map (sqrt) lst))

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst f (x:xs)
    | f x = xs
    | otherwise = (x:xs)

removeLast :: (a -> Bool) -> [a] -> [a]
removeLast f lst
    | f (last lst) = init lst
    | otherwise = lst

-- write lambda functions for each
zeroToTenNew :: [Int] -> [Int]
zeroToTenNew lst = filter (\x -> x >= 0 && x <= 10) lst

mult10New :: [Int] -> [Int]
mult10New lst = foldr (\x y -> (x * 10) : y) [] lst

--reverse :: [a] -> [a]
-- reverse lst = foldr (\x (y:ys) -> y : head ys : ys) lst lst
-- onlyLowerCase :: String -> String