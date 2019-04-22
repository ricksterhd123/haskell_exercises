import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2)
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stMarks = [ mk | (st,mk) <- stMarks ]

pass :: [StudentMark] -> [String]
pass stMarks = [ st | (st,mk) <- stMarks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y
    | x <= y            = (x,y)
    | otherwise         = (y,x)

sumDifference :: Int -> Int -> (Int, Int)
sumDifference a b = (a+b, a-b)

grade :: StudentMark -> Char
grade (student, mark)
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise = 'F'

capMark :: StudentMark -> StudentMark
capMark (student, mark) = (student, if mark > 40 then 40 else mark)

firstNumbers :: Int -> [Int]
firstNumbers n = [1 .. n]

firstSquares :: Int -> [Int]
firstSquares n = [i^2 | i <- firstNumbers n]

capitalise :: String -> String
capitalise str = [toUpper a | a <- str]

onlyDigits :: String -> String
onlyDigits str = [a | a <- str, isDigit a]

capMarks :: [StudentMark] -> [StudentMark]
capMarks marks = [capMark mark | mark <- marks]

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents marks
    = [(name, grade (name, mark)) | (name, mark) <- marks]

duplicate :: String -> Int -> String
duplicate str n
    | n > 1 = duplicate str (n - 1) ++ str
    | n == 1 = str
    | otherwise = []

divisors :: Int -> [Int]
divisors x
    = [ i | i <- [1 .. x], x `mod` i == 0]

isPrime :: Int -> Bool
isPrime x = length (divisors x) == 2

split :: [(a, b)] -> ([a], [b])
split list = ([a | (a, b) <- list], [b | (a, b) <- list])