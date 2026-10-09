type Card = (Char, Int)
type Hand = (Card, Card)
type Game = (Hand, Hand)

generator3 :: [Game]
generator3 =
    [ (h1, h2) |
        s1 <- suits, n1 <- nums,
        s2 <- suits, s2 /= s1,
        n2 <- nums,  n2 /= n1,
        s3 <- suits, s3 /= s1, s3 /= s2,
        n3 <- nums,  n3 /= n1, n3 /= n2,
        s4 <- suits, s4 /= s1, s4 /= s2, s4 /= s3,
        n4 <- nums,  n4 /= n1, n4 /= n2, n4 /= n3,
        let h1 = ((s1, n1), (s2, n2)),
        let h2 = ((s3, n3), (s4, n4)),
        let score1 = handSum h1,
        let score2 = handSum h2,
        score1 > 0,
        isInteger score1,
        score2 == -score1
    ]
    where
        suits =['S', 'H', 'C', 'D']
        nums = [1..13]

exchange :: Game -> [Game]
exchange (((s1, n1), (s2, n2)), ((s3, n3), (s4, n4)))  
    = [(((s1, n1), (s2', n2')), ((s3, n3), (s4', n4')))| 
        s2' <- suits, 
        n2' <- nums, 
        (s2', n2') `notElem` initCards,
        s2' /= s1, s2' /= s3,
        s4' <- suits, 
        n4' <- nums,  
        (s4', n4') `notElem` initCards,
        (s2', n2') /= (s4', n4'),
        s4' /= s1, s4' /= s3, s4' /= s2',
        handSum ((s1, n1), (s2', n2')) == joInit,
        handSum ((s3, n3), (s4', n4')) == boInit
            ]
        where 
        suits =['S', 'H', 'C', 'D']
        nums = [1..13]
        initCards = [(s1, n1), (s2, n2), (s3, n3), (s4, n4)]
        joInit = handSum((s1, n1), (s2, n2))
        boInit = handSum((s3, n3), (s4, n4))


selector3 :: Game -> Bool
selector3 g 
    = not(null (exchange g))

isInteger :: Double -> Bool
isInteger x 
    = abs (x - fromIntegral (round x :: Integer)) < 1e-6

handSum :: Hand -> Double
handSum ((s1, n1), (s2, n2)) 
    | n1 == n2 = 0
    | otherwise = card1 + card2
  where
    dn1 = fromIntegral n1
    dn2 = fromIntegral n2
    card1 = cardVal (s1, n1) dn2
    card2 = cardVal (s2, n2) dn1

cardVal :: Card -> Double -> Double 
cardVal (s, n1) n2
    = value 
    where 
    dn1 = fromIntegral n1
    value = case s of
        'S' -> dn1 * dn1
        'H' -> dn1 / 2
        'C' -> -dn1
        'D' -> dn1 / n2
        _   -> 0

x_generator3 :: Int
x_generator3
    = length [ t | t <- [ t1 , t2 , t3 , t4 , t5 ] , t ]
    where
    t1 = ((( 'D' ,6) ,( 'H' ,2) ) ,(( 'C' ,5) ,('S' ,1) ) ) `elem` generator3
    t2 = ((( 'D' ,10) ,( 'H' ,2) ) ,(( 'C' ,7) ,( 'S' ,1) ) ) `elem` generator3
    t3 = ((( 'S',1) ,( 'D' ,6) ) ,(( 'C' ,9) ,( 'H' ,4) ) ) `elem` generator3
    t4 = ((( 'S' ,2) ,( 'D' ,8) ) ,(( 'C' ,10) ,( 'H' ,4) ) ) `elem` generator3
    t5 = ((( 'D' ,6) ,( 'S' ,1) ) ,(( 'C' ,13) ,( 'H' ,12) ) ) `elem` generator3

x_selector3 :: Int
x_selector3
    = length [ t | t <- [ t1 , t2 , t3 , t4 , t5 ] , t ]
    where
    t1 = selector3 ((( 'D' ,7) ,( 'S' ,7) ) ,(( 'C' ,1) ,( 'H' ,2) ) ) == False
    t2 = selector3 ((( 'D' ,3) ,( 'H' ,3) ) ,(( 'C' ,3) ,( 'S' ,3) ) ) == False
    t3 = selector3 ((( 'S' ,1) ,( 'D' ,4) ) ,(( 'C' ,7) ,( 'H' ,4) ) ) == False
    t4 = selector3 ((( 'D' ,10) ,( 'H' ,2) ) ,(( 'C' ,10) ,( 'S' ,2) ) ) == False
    t5 = selector3 (((  'D' ,2) ,( 'H' ,2) ) ,(( 'C' ,13) ,( 'S' ,13) ) ) == False

main :: IO()
main = print (filter selector3 generator3)