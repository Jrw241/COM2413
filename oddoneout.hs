import Data.List((\\))

type Grid = [String] -- A list of 3 Strings, each 3 Chars long

-- Generates possible solutions for the teaser
generator4 :: [String] 
generator4 = 
  [ n 
  | n1 <- ['1', '3'..'9'] 
  , n2 <- ['1', '3'..'9']
  , n3 <- ['1', '3'..'9']
  , let n = [n1, n2, n3]
  , prime (read n)
  , not (isReversiblePrime n) 
  ]

-- Checks if the given number input satisfies the teaser conditions
selector4 :: String -> Bool
selector4 n = not (null [ g' | g <- mGrids, g' <- possibleGGrids g n ])
  where
    mGrids = possibleMGrids n

-- Generates all valid Martha grids where one line is equal to n 
possibleMGrids :: String -> [Grid]
possibleMGrids n = 
  [ g
  | r1 <- n:possibleRows
  , r2 <- n:possibleRows
  , r3 <- n:possibleRows
  , let g = [r1, r2, r3]
  , isUnique (gridLines g)
  , countReversible g == 7
  , length (gridLines g \\ [n]) == 7
  ]
  where
    possibleRows = 
      [ [n1, n2, n3]
      | n1 <- ['1', '3'..'9']
      , n2 <- ['1', '3'..'9']
      , n3 <- ['1', '3'..'9']
      , isReversiblePrime [n1, n2, n3]
      ]

-- Gets all corresponding GeorgeGrids to the Martha Grid
possibleGGrids :: Grid -> String -> [Grid]
possibleGGrids g n =
  [ g'
  | g' <- singleDigitModifications g
  , n `notElem` gridLines g'
  , hasEightDistinctPrimes g'
  ]

-- Checks that all 8 lines are distinct reversible primes
hasEightDistinctPrimes :: Grid -> Bool
hasEightDistinctPrimes g = all isReversiblePrime lines && isUnique lines
  where
    lines = gridLines g

-- Checker to see if all lines are unique
isUnique :: [String] -> Bool
isUnique (x:xs) = x `notElem` xs && isUnique xs
isUnique []     = True

-- Generates all grids formed by changing exactly one character to another odd digit
singleDigitModifications :: Grid -> [Grid]
singleDigitModifications [[a,b,c], [d,e,f], [g,h,i]] =
  [ [[a',b,c], [d,e,f], [g,h,i]] | a' <- odd, a' /= a ] ++
  [ [[a,b',c], [d,e,f], [g,h,i]] | b' <- odd, b' /= b ] ++
  [ [[a,b,c'], [d,e,f], [g,h,i]] | c' <- odd, c' /= c ] ++
  [ [[a,b,c], [d',e,f], [g,h,i]] | d' <- odd, d' /= d ] ++
  [ [[a,b,c], [d,e',f], [g,h,i]] | e' <- odd, e' /= e ] ++
  [ [[a,b,c], [d,e,f'], [g,h,i]] | f' <- odd, f' /= f ] ++
  [ [[a,b,c], [d,e,f], [g',h,i]] | g' <- odd, g' /= g ] ++
  [ [[a,b,c], [d,e,f], [g,h',i]] | h' <- odd, h' /= h ] ++
  [ [[a,b,c], [d,e,f], [g,h,i']] | i' <- odd, i' /= i ]
  where
    odd = ['1', '3'..'9']

-- Counts how many lines in a grid are reversible primes
countReversible :: Grid -> Int
countReversible g = length (filter isReversiblePrime (gridLines g))

-- Extracts the 8 lines of a 3x3 grid
gridLines :: Grid -> [String]
gridLines [[g00, g01, g02], [g10, g11, g12], [g20, g21, g22]] =
  [ [g00, g01, g02]
  , [g10, g11, g12]
  , [g20, g21, g22]
  , [g00, g10, g20]
  , [g01, g11, g21]
  , [g02, g12, g22]
  , [g00, g11, g22]
  , [g20, g11, g02]
  ]

-- Checks if integer n is prime
prime :: Int -> Bool 
prime n 
  | n < 2     = False 
  | otherwise = null [ f | f <- [2..floor (sqrt (fromIntegral n))], n `mod` f == 0 ]

-- Checks if string n is prime both forwards and backwards
isReversiblePrime :: String -> Bool
isReversiblePrime n = prime (read n) && prime (read (reverse n))

x_generator4 :: Int
x_generator4 = length [t | t <- [t1, t2, t3, t4, t5], t]
  where
    t1 = "331" `elem` generator4
    t2 = "557" `elem` generator4
    t3 = "593" `elem` generator4
    t4 = "151" `notElem` generator4
    t5 = "991" `notElem` generator4

x_selector4 :: Int
x_selector4 = length [t | t <- [t1, t2, t3, t4, t5], t]
  where
    t1 = selector4 "193" == False
    t2 = selector4 "397" == False
    t3 = selector4 "557" == False
    t4 = selector4 "911" == False
    t5 = selector4 "997" == False

main :: IO () 
main = print (filter selector4 generator4)