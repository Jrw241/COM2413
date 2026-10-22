import Data.List ((\\))

-- Uses types to increase readability of the code
type CTPair = (String, String)

-- Generates all possible (c, t) digit pairs for the teaser
generator2 :: [CTPair]
generator2 =
  [ (c, t)
  -- c1 > c2 > c3 > c4 (strictly decreasing digits)
  | c1 <- ['3'..'9']
  , c2 <- ['2'..c1] \\ [c1]
  , c3 <- ['1'..c2] \\ [c2]
  , c4 <- ['0'..c3] \\ [c3]
  , let c = [c1, c2, c3, c4]
  -- t represents any 4-digit combination ['0'..'9'] without any digits of c
  , t1 <- ['1'..'9'] \\ c
  , t2 <- ['0'..'9'] \\ c
  , t3 <- ['0'..'9'] \\ c
  , t4 <- ['0'..'9'] \\ c
  , let t = [t1, t2, t3, t4]
  ]

-- Checks if the given (c, t) pair input satisfies the teaser conditions
selector2 :: CTPair -> Bool
selector2 (c, t) = right * right == read t
  where
    right = read c `mod` 100

x_generator2 :: Int
x_generator2 = length [t | t <- [t1, t2, t3, t4, t5], t]
  where
    t1 = ("5430", "2661") `elem` generator2
    t2 = ("6531", "7424") `elem` generator2
    t3 = ("8740", "2161") `elem` generator2
    t4 = ("9280", "6461") `notElem` generator2
    t5 = ("1232", "5563") `notElem` generator2

x_selector2 :: Int
x_selector2 = length [t | t <- [t1, t2, t3, t4, t5], t]
  where
    t1 = selector2 ("7653", "2809") == True
    t2 = selector2 ("1193", "7542") == False
    t3 = selector2 ("3647", "8210") == False
    t4 = selector2 ("3166", "9750") == False
    t5 = selector2 ("0394", "8765") == False

-- Prints the answer(s) to the teaser
main :: IO ()
main = print x_generator2