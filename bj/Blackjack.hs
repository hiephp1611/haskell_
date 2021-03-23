{- Lab 2 Authors: Hung, Shahda, August Lab group: 6 -} 

module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

--Task A1
hand2 :: Hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

--- list of step that size takes to count length of a Hand
sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 1 + 1 + size []
            , 1 + 1 + 0
            , 2
            ]

aCard1 :: Card
aCard1 = Card (Numeric 3) Diamonds -- define your favorite card here

aCard2 :: Card
aCard2 = Card Ace Clubs -- define another card here

aHand :: Hand
aHand = aCard1 : aCard2: ([]) -- a Hand with two Cards, aCard1 and aCard2

--Task A2

-- Give a suit its unique icon
icon :: Suit -> String
icon Hearts   = "\9829"
icon Spades   = "\9824"
icon Diamonds = "\9830"
icon Clubs    = "\9827"

-- Display Card as Rank of Suit (Ex. Ace of Spades)
displayCard :: Card -> String
displayCard (Card (Numeric i) s)   = show i ++ " of " ++ icon s
displayCard (Card r s)             = show r ++ " of " ++ icon s

-- Display all cards in hand as Rank of Suit
display :: Hand -> String
display []   = ""
display hand = unlines [displayCard card | card <- hand]


--Task A3

-- Give hand the correct point of Aces (Ace can be 1 or 11 according to total points of hand)
value :: Hand -> Int
value hand  | valueHand hand > 21  = valueHand hand - 10*(numberOfAces hand)
            | otherwise            = valueHand hand

-- Calculate value of hand with Ace is 11 point
valueHand :: Hand -> Int
valueHand [] = 0 
valueHand (card:hand)= valueCard card + valueHand hand

-- Calculate value of rank (King, Queen and Jack are 10, Ace is 11 by default)
valueRank :: Rank -> Int
valueRank (Numeric i)   = i
valueRank Ace           = 11
valueRank _             = 10

-- Give card the points as value of Rank
valueCard :: Card -> Int
valueCard (Card r _)  = valueRank r

-- Count number of Aces in hand
numberOfAces :: Hand -> Int
numberOfAces []                  = 0
numberOfAces ((Card Ace _):hand) = 1 + numberOfAces hand
numberOfAces (card:hand)         = numberOfAces hand

--Task A4

-- Check if player's hand is bust
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Check the winner in Blackjack game (hand1 = bank, hand2 = guest)
winner :: Hand -> Hand -> Player
winner hand1 hand2 | gameOver(hand1)                = Bank
                   | gameOver(hand2)                = Guest
                   | value(hand1) > value(hand2)    = Guest
                   | otherwise                      = Bank

--B1
-- allSuit is a list contains all the suits values
--allSuit :: [Suit]
--allSuit = [Hearts, Diamonds, Clubs, Spades]

-- allRank is a list contains all the rank values
--allRank :: [Rank]
--allRank = [Numeric i | i <- [2..10]] ++ [Jack, Queen, King, Ace]

--fullDeck uses list comprehension to get every available combinations of rank and suit, create a complete deck with 52 cards
fullDeck :: Deck
fullDeck = [(Card r s)|  r <- allRank,s <- allSuit]
  where
      allRank = [Numeric i | i <- [2..10]] ++ [Jack, Queen, King, Ace]
      allSuit = [Hearts, Diamonds, Clubs, Spades]

--check the size of list fullDeck which must include 52 elements type Card
prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

--B2
-- Draw a card from top of the deck, meaning first element of list type Deck
draw :: Deck -> Hand -> (Deck, Hand)
draw [] _             = error "draw: The deck is empty."
draw (card:deck) hand = (deck ,card : hand)

--B3
--Function play for bank and back bank's final hand
playBank :: Deck -> Hand
playBank deck = playBank' deck []

--Function that keeps drawing card for bank but stops drawing card efter it's score is 16 or higher
playBank' :: Deck -> Hand -> Hand
playBank' deck bankHand | value bankHand < 16   = playBank' deck' bankHand'
                        | otherwise             = bankHand
  where (deck' , bankHand') = draw deck bankHand

--B4
--Shuffle the deck randomly with give random list of double number in range 0 to 1
--take a card from position x*(length of deck) and put in new deck, continue to do so untill all 52 cards is shuffled
shuffle :: [Double] -> Deck -> Deck
shuffle [] _        = []
shuffle _ []        = []
shuffle (x:xs) deck = card' : shuffle xs deck'
  where 
    position    = round ( x* (fromIntegral (length deck)) )
    (card',deck') = pickCard deck position

--Pick a card from given position in deck, give back card and deck without the card
pickCard :: Deck -> Int -> (Card,Deck)
pickCard (card : deck) n | n<=1 = (card,deck)
pickCard (card : deck) n        = (card',card:deck')
    where
      (card',deck')= pickCard deck (n-1)

--B5
--check if element of type Card is in Deck
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

--Check if shuffled deck still has all the same card
prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

--Check for duplicate card in shuffle deck
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck =
    size deck == size (shuffle randomlist deck)

--B6
--Implementation Blackjack game's interface from RunGame.hs
implementation  = Interface
  { iFullDeck   = fullDeck
  , iValue      = value
  , iDisplay    = display
  , iGameOver   = gameOver
  , iWinner     = winner
  , iDraw       = draw
  , iPlayBank   = playBank
  , iShuffle    = shuffle
  }

--Run the program with main, use given function runGame
main :: IO ()
main = runGame implementation