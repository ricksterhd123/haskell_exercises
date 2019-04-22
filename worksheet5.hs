{-
Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)


-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne (x:xs) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x : x : xs

rotate :: [a] -> [a]
rotate [] = []
rotate (x:[]) = x:[]
rotate (x:xs) = head xs : x : tail xs

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

countElems :: Int -> [Int] -> Int
countElems _ [] = 0
countElems num (x:xs)
    | x == num = 1 + countElems num xs
    | otherwise = countElems num xs

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll num (x:xs)
    | x == num = removeAll num xs
    | otherwise = x : removeAll num xs

type StudentMark = (String, Int)

listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks name ((stuName, mark):xs)
    | name == stuName = mark : listMarks name xs
    | otherwise = listMarks name xs

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (y:ys) (x:xs) = y == x && prefix ys xs

subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = False
subSequence _ [] = False
subSequence pattern (y:ys) = prefix pattern (y:ys) || subSequence pattern ys
