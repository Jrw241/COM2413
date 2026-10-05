quicksort :: [Int] -> [Int]
quicksort (x:xs) 
    = quicksort[n|n  <- xs, n < x] ++ 
    [x] ++ 
    quicksort[n|n <- xs, n >= x]
quicksort [] = []  

generator :: [Int]
generator = [0..624]

selector :: Int -> Bool
selector n = quicksort (digits5 n) == quicksort (digits8 n)

digits5 :: Int -> [Int]
digits5 n
    | n < 5 = [n]
    | otherwise = digits5 (n `div` 5) ++ [n `mod` 5]

digits8 :: Int -> [Int]
digits8 n
    | n < 8 = [n]
    | otherwise = digits8 (n `div` 8) ++ [n `mod` 8]

char16 :: Int -> Char
char16 n 
    | n == 0 = '0'
    | n == 1 = '1'
    | n == 2 = '2'
    | n == 3 = '3'
    | n == 4 = '4'
    | n == 5 = '5'
    | n == 6 = '6'
    | n == 7 = '7'
    | n == 8 = '8'
    | n == 9 = '9'
    | n == 10 = 'A'
    | n == 11 = 'B'
    | n == 12 = 'C'
    | n == 13 = 'D'
    | n == 14 = 'E'
    | n == 15 = 'F'


main :: IO ()
main = print (filter selector generator)