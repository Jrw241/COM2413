generator1 :: [(Int,Int,Int,Int)]
generator1 = 
    [(x, 98-x, y, 98-y)| x <- [1..49], y <- [1..49]]

selector1 :: (Int, Int, Int, Int) -> Bool
selector1 (a, b, c, d) 
    | not (isTriangle 70 a b) || not (isTriangle 70 c d) = False
    | otherwise = isInteger s1 && isInteger s2 && (s1 - s2 == 504.0 ||s2 - s1 == 504.0)
  where
    s1 = triangleArea 70 a b
    s2 = triangleArea 70 c d


isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = a + b > c && a + c > b && b + c > a

isInteger :: Double -> Bool
isInteger x = abs (x - fromIntegral (round x :: Integer)) < 1e-6

triangleArea :: Int -> Int -> Int -> Double
triangleArea a b c = sqrt (s * (s - ax) * (s - bx) * (s - cx))
  where
    ax = fromIntegral a
    bx = fromIntegral b
    cx = fromIntegral c
    s  = (ax + bx + cx) / 2.0


main :: IO()
main 
    = print (filter selector1 generator1)