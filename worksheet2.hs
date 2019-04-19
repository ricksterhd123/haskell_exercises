-- write a new version of absolute with guards
absolute :: Int -> Int
absolute x
    | x < 0 = -x
    | otherwise = x

-- returns 1 for positive, -1 for negative and 0 for zero-valued args
sign :: Int -> Int
sign x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

-- how many arguments are equal? 0 2 or 3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | a == b && b == c && a == c = 3
    | a == b || b == c || a == c = 2
    | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths a b c
    = diagonalLength a + diagonalLength b + diagonalLength c
    where
    diagonalLength x = sqrt(2) * x

{-|
A taxi company calculates fares based on distance travelled. Fares start at £2.20; 50p
is added for each kilometre covered for the first 10 kilometres; and 30p is added for
each additional kilometre. W
-}

taxiFare :: Int -> Float
taxiFare distance
    | distance <= 10 = 2.20 + (0.50 * fromIntegral distance)
    | otherwise = 7.20 + (fromIntegral distance - 10.0) * 0.30

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c
    | a > average a b c && b > average a b c && c > average a b c = 3
    | (a > average a b c && b > average a b c) || (b > average a b c && c > average a b c) || (a > average a b c && c > average a b c) = 2
    | a > average a b c || b > average a b c || c > average a b c = 1
    | otherwise = 0
    where
    average a b c = (fromIntegral a + fromIntegral b + fromIntegral c) `div` 3

validDate :: Int -> Int -> Bool
validDate day month
    | (month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) && day >= 1 && day <= 31 = True
    | (month == 4 || month == 6 || month == 9 || month == 11) && day >= 1 && day <= 30 = True
    | month == 2 && day >= 1 && day <= 28 = True
    | otherwise = False

daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | (month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) = 31
    | (month == 4 || month == 6 || month == 9 || month == 11) = 30
    | month == 2 = if year `mod` 4 == 0 then 29 else 28
    | otherwise = 0
