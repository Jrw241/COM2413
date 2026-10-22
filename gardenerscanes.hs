-- Uses types to increase readability of the code
type BambooLengths = (Int,Int,Int,Int)

-- Generates all viable bamboo lengths, without order 
-- (so will get 2 possible options for the answer)
generator1 :: [BambooLengths]
generator1 = 
  [ lengths -- (a,b,c,d), a <= b, c <= d
  | x <- [1..49]
  , y <- [1..49]
  , let lengths = (x, 98-x, y,98 - y)
  ]

-- Checks if the bamboo lengths input satisfies the teaser conditions
selector1 :: BambooLengths -> Bool
selector1 (a, b, c, d) 
  -- check they are both integers, 
  -- and see if the abs value of s1 - s2 = 504.0
    = isInteger s1 && isInteger s2 && abs (s1 - s2) == 504.0
  where
    s1 = triangleArea 70 a b
    s2 = triangleArea 70 c d

-- Checker function to check if a double x is an integer
isInteger :: Double -> Bool
isInteger x = floor x == ceiling x

-- Uses Heron's formula to calculate the area of the triangle
triangleArea :: Int -> Int -> Int -> Double
triangleArea a b c = sqrt (s * (s - a') * (s - b') * (s - c'))
  where
    a' = fromIntegral a
    b' = fromIntegral b
    c' = fromIntegral c
    s  = (a' + b' + c') / 2.0

-- Prints the answer(s) to the teaser
main :: IO ()
main = print (filter selector1 generator1)