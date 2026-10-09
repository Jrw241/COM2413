generator4 :: [String] 
generator4 
    = [show(a*100 + b*10 +c)| 
    a <- odds, b <- odds, c <- odds
    ]
    where
        odds = [1,3,5,7,9]

isPrime :: Int -> Bool
isPrime n = n >= 2 && null [x | x <- [2 .. floor (sqrt (fromIntegral n :: Double))], n `mod` x == 0]