generator2 :: [(String, String)]
generator2 = [(show(a*1000 + b*100 + c*10 + d) , show(e*1000 + f*100 + g*10 + h))| 
        a <- [3..9],
        b <- [2..8], a > b,
        c <- [1..7], b > c,
        d <- [0..6], c > d,
        e <- [0..9], e /= a, e /= b, e /= c, e /= d,
        f <- [0..9], f /= a, f /= b, f /= c, f /= d,
        g <- [0..9], g /= a, g /= b, g /= c, g /= d,
        h <- [0..9], h /= a, h /= b, h /= c, h /= d
        ]

selector2 :: (String, String) -> Bool
selector2 (t, c) = right * right == read c
    where
        right = read t `mod` 100

main :: IO()
main = print (filter selector2 generator2)