import Data.List ((\\))

-- Uses types to increase readability of the code
type Card = (Char, Int)
type Hand = (Card, Card)
type Game = (Hand, Hand)

-- Generates 4 cards, each with a different suit
generator3 :: [Game]
generator3 =
  [ (h1, h2)
  | s1 <- suits
  , n1 <- nums
  , s2 <- suits \\ [s1]
  , n2 <- nums
  -- h1 = Jo's final hand
  , let h1 = ((s1, n1), (s2, n2))
  , s3 <- suits \\ [s1, s2]
  , n3 <- nums
  , s4 <- suits \\ [s1, s2, s3]
  , n4 <- nums
  -- h2 = Bo's final hand
  , let h2 = ((s3, n3), (s4, n4))
  ]
  where
    suits = ['S', 'H', 'C', 'D']
    nums  = [1..13]

-- Exchanges the final hand to get the first hand as it is the same swap,
-- Makes sure all numbers are different as well as all the suits are different
exchange :: Game -> [Game]
exchange (((s1, n1), (s2, n2)), ((s3, n3), (s4, n4)))
  | n1 == n3  = []
  | otherwise =
      [ (((s1, n1), (s2', n2')), ((s3, n3), (s4', n4')))
      -- Pick available card for Hand 1 (excluding values n1 and n3)
      | (s2', n2') <- availableCards [n1, n3] usedSuits initCards 
      -- Pick available card for Hand 2 (excluding values n1, n3 and n2')
      , (s4', n4') <- availableCards [n1, n3, n2'] usedSuits initCards
      -- Ensures cards are not the same
      , (s2', n2') /= (s4', n4') 
      -- Checks the suits aren't the same
      , s2' /= s4' 
      -- Checks the handsums to check they stay the same
      , handSum ((s1, n1), (s2', n2')) == joInit
      , handSum ((s3, n3), (s4', n4')) == boInit
      ]
  where
    initCards = [(s1, n1), (s2, n2), (s3, n3), (s4, n4)]
    usedSuits = [s1, s3]
    joInit    = handSum ((s1, n1), (s2, n2))
    boInit    = handSum ((s3, n3), (s4, n4))

-- Finds all valid replacement cards in the deck, that can replace (s2,n2) and (s4,n4)
availableCards :: [Int] -> [Char] -> [Card] -> [Card]
availableCards usedNums usedSuits initCards =
  [ (s, n)
  -- All suit that aren't in the used suits
  | s <- ['S', 'H', 'C', 'D'] \\ usedSuits
  -- Checks number are in used nums
  , n <- [1..13] \\ usedNums
  -- Checks the generated card isn't in the initial cards
  , (s, n) `notElem` initCards
  ]

-- Checks if the game input satisfies the teaser conditions
selector3 :: Game -> Bool
selector3 (h1, h2)
  -- Checks whether the card sums are integers, and that they follow the condition
  -- joInit > 0 and boInit = -joInit
  | not (isInteger joInit) || joInit < 0 || joInit /= -boInit = False
  -- Otherwise check if there is an exchange possible
  | otherwise = not (null (exchange (h1, h2)))
  where
    joInit = handSum h1
    boInit = handSum h2

-- Checker function to see if the double is an integer
isInteger :: Double -> Bool
isInteger x = floor x == ceiling x

-- Gets the sum of the input hand
handSum :: Hand -> Double
handSum ((s1, n1), (s2, n2))
  -- First checks the face values of the cards to see if they are equal (return 0)
  | n1 == n2  = 0
  -- Otherwise calculate the card values using the helper function
  | otherwise = card1 + card2
  where
    dn1   = fromIntegral n1
    dn2   = fromIntegral n2
    card1 = cardVal (s1, n1) dn2
    card2 = cardVal (s2, n2) dn1

-- Gets the value of the card when applying its suit operation
cardVal :: Card -> Double -> Double
cardVal (s, n1) n2 = case s of
  'S' -> dn1 * dn1  -- Spade squares the number
  'H' -> dn1 / 2    -- Heart halves the number
  'C' -> -dn1       -- Clubs negates the number
  'D' -> dn1 / n2   -- Diamonds divides it by the other card's value
  where
    dn1 = fromIntegral n1

-- Prints the answer(s) to the teaser
main :: IO ()
main = print (filter selector3 generator3)