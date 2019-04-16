-- function which multiplies its arguments by 10
timesTen :: Int -> Int
timesTen x = x * 10

-- function which gives the sum of three integers
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

-- using constant pi and power operator, which a function which gives the area of a circle given its radius
areaOfCircle :: Float -> Float
areaOfCircle radius = pi * (radius ^ 2)

-- calculate volume of cylinder given cross sectional radius and length
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder length radius = areaOfCircle radius * length

-- get the distance between two points x1, y2, x2, y2
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ( sqrt (x2 - x1) + sqrt (y2 - y1) )

-- function returns true iff all of its three arguments are all different from on another
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = (a /= b) && (b /= c) && (a /= c)

-- using the mod function write a function that tests whether one integer is divisible by another:
divisibleBy :: Int -> Int -> Bool
divisibleBy a b = (a `mod` b) == 0

-- get the average of 3 Ints
-- note to self: a b c are assumed to be Int, however (/) :: Fractional a => a -> a -> a
-- so fromIntegral must convert Int -> Fractional
averageThree :: Int -> Int -> Int -> Float
averageThree a b c = ( ( fromIntegral a + fromIntegral b + fromIntegral c ) / 3 ) :: Float

-- using a conditional write a function that gives an absolute value of an integer (without using abs or sqrt)
absolute :: Int -> Int
absolute x = if x < 0 then -x else x
