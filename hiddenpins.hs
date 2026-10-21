import Data.List ((\\))

-- Uses types to increase readability of the code
type TcPair = (String, String)

-- Generates all possible (t, c) digit-string pairs for the teaser
generator2 :: [TcPair]
generator2 =
  [ (t, c)
  -- t1 > t2 > t3 > t4 (strictly decreasing digits)
  | t1 <- ['3'..'9']
  , t2 <- ['2'..t1] \\ [t1]
  , t3 <- ['1'..t2] \\ [t2]
  , t4 <- ['0'..t3] \\ [t3]
  , let t = [t1, t2, t3, t4]
  -- c represents any 4-digit combination ['0'..'9'] without any digits of t
  , c1 <- ['0'..'9'] \\ t
  , c2 <- ['0'..'9'] \\ t
  , c3 <- ['0'..'9'] \\ t
  , c4 <- ['0'..'9'] \\ t
  , let c = [c1, c2, c3, c4]
  ]

-- Checks if the given (t, c) pair input satisfies the teaser conditions
selector2 :: TcPair -> Bool
selector2 (t, c) = right * right == read c
  where
    right = read t `mod` 100

-- Prints the answer(s) to the teaser
main :: IO ()
main = print (filter selector2 generator2)