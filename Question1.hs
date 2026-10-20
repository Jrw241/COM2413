-- Generates all viable bamboo lengths, without order 
-- (so will get 2 possible options for the answer)
generator1 :: [(Int, Int, Int, Int)]
generator1 = 
  [ (x, 98 - x, y, 98 - y) 
  | x <- [1..49]
  , y <- [1..49]
  ]

--checks if the inputs is an answer to the teaser
selector1 :: (Int, Int, Int, Int) -> Bool
selector1 (a, b, c, d) 
  -- First checks that they are both valid triangles
  | not (isTriangle 70 a b) || not (isTriangle 70 c d) = False
  -- If both triangles, check they are both integers, 
  -- and see if the abs value of s1 - s2 = 504.0
  | otherwise = isInteger s1 && isInteger s2 && abs (s1 - s2) == 504.0
  where
    s1 = triangleArea 70 a b
    s2 = triangleArea 70 c d

-- Checker function to see if the inputted lengths make a valid triangle
isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = a + b > c && a + c > b && b + c > a

-- Checker function to check if a double x is an integer
isInteger :: Double -> Bool
isInteger x = floor x == ceiling x

-- Uses Heron's formula to calculate the area of the triangle
triangleArea :: Int -> Int -> Int -> Double
triangleArea a b c = sqrt (s * (s - ax) * (s - bx) * (s - cx))
  where
    ax = fromIntegral a
    bx = fromIntegral b
    cx = fromIntegral c
    s  = (ax + bx + cx) / 2.0

-- Prints the answer(s) to the teaser
main :: IO ()
main = print (filter selector1 generator1)