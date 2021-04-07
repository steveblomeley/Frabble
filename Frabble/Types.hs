module Frabble.Types where

type Tile = Char
type Word = [Tile]
type Bag = [Tile]
type Rack = [Tile]

-- Data types to describe a move
-- e.g. STDIN> A 12 Across FLIPPER
--    becomes: Move (Posn 'A' 12) Horizontal "FLIPPER"
--
-- [ The user enters Down or Across for the move direction - but
--   internally these map to alignments of Vertical or Horizontal, as  
--   "Down" is already used as one of the 4 directions to read letters ]

data Alignment = Horizontal | Vertical deriving (Show, Read, Eq)
data Direction = Up | Down | Left | Right deriving Show
data Position = Pos Char Int deriving (Show, Read, Eq)
data Move = Move Position Alignment Frabble.Types.Word deriving (Show, Read)

data Bonus = Word Int | Letter Int deriving Eq

type LiveBonus = (Position,Bonus)
type LiveBonuses = [LiveBonus]
type LiveTile = (Position,Tile)
type LiveWord = [LiveTile]

type Board = [(Position,Tile)]
type BonusBoard = [LiveBonus] 

instance Show Bonus where
    show (Word n) 
        | n == 2    = "+"
        | n == 3    = "*"
        | otherwise = "?"
    show (Letter n)
        | n == 2    = "-"
        | n == 3    = "="
        | otherwise = "?"

-- Simple data types - declared to make function type declarations more explicit
boardSize :: Int
boardSize = 15

rackSize :: Int
rackSize = 7

cols :: [Char]
cols = take boardSize ['A'..'Z']

rows :: [Int]
rows = [1..boardSize]

ltrCounts :: [Int]
ltrCounts = [9,2,2,4,12,2,3,2,9,1,1,4,2,6,8,2,1,6,4,6,4,2,2,1,2,1]

ltrScores :: [Int]
ltrScores = [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

fullBag :: Bag
fullBag = concat . zipWith replicate ltrCounts $ ['A'..'Z']

scores = zip ['A'..'Z'] ltrScores

l2 :: Bonus
l2 = Letter 2

l3 :: Bonus
l3 = Letter 3

w2 :: Bonus
w2 = Word 2

w3 :: Bonus
w3 = Word 3

bonuses :: BonusBoard
bonuses = [(Pos 'A' 1,  w3), (Pos 'H' 1,  w3), (Pos 'O' 1,  w3),
           (Pos 'A' 8,  w3), (Pos 'O' 8,  w3),
           (Pos 'A' 15, w3), (Pos 'H' 15, w3), (Pos 'O' 15, w3),
           (Pos 'B' 2,  w2), (Pos 'N' 2,  w2),
           (Pos 'C' 3,  w2), (Pos 'M' 3,  w2),
           (Pos 'D' 4,  w2), (Pos 'L' 4,  w2),
           (Pos 'E' 5,  w2), (Pos 'K' 5,  w2),
           (Pos 'H' 8,  w2),
           (Pos 'E' 11, w2), (Pos 'K' 11, w2),
           (Pos 'D' 12, w2), (Pos 'L' 12, w2),
           (Pos 'C' 13, w2), (Pos 'M' 13, w2),
           (Pos 'B' 14, w2), (Pos 'N' 14, w2),
           (Pos 'D' 1,  l2), (Pos 'L' 1,  l2),
           (Pos 'G' 3,  l2), (Pos 'I' 3,  l2),
           (Pos 'A' 4,  l2), (Pos 'H' 4,  l2), (Pos 'O' 4,  l2),
           (Pos 'C' 7,  l2), (Pos 'G' 7,  l2), (Pos 'I' 7,  l2), (Pos 'M' 7,  l2),
           (Pos 'D' 8,  l2), (Pos 'L' 8,  l2),
           (Pos 'C' 9,  l2), (Pos 'G' 9,  l2), (Pos 'I' 9,  l2), (Pos 'M' 9,  l2),
           (Pos 'A' 12, l2), (Pos 'H' 12, l2), (Pos 'O' 12, l2),
           (Pos 'G' 13, l2), (Pos 'I' 13, l2),
           (Pos 'D' 15, l2), (Pos 'L' 15, l2),
           (Pos 'F' 2,  l3), (Pos 'J' 2,  l3),
           (Pos 'B' 6,  l3), (Pos 'F' 6,  l3), (Pos 'J' 6,  l3), (Pos 'N' 6,  l3),
           (Pos 'B' 10, l3), (Pos 'F' 10, l3), (Pos 'J' 10, l3), (Pos 'N' 10, l3),
           (Pos 'F' 14, l3), (Pos 'J' 14, l3)]
