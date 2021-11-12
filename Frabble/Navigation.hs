module Frabble.Navigation (offBoard, nextPos, prevPos, isEmpty, shift, adjacent) where

import Data.Char
import Data.Maybe
import Frabble.Types
import Frabble.Useful

shift :: Direction -> Int -> Position -> Position
shift Up                  n (Pos c r) = Pos c (r - n)
shift Down                n (Pos c r) = Pos c (r + n)
shift Frabble.Types.Right n (Pos c r) = Pos (chr(ord c + n)) r
shift Frabble.Types.Left  n (Pos c r) = Pos (chr(ord c - n)) r

nextPos :: Alignment -> Position -> Position
nextPos Horizontal = shift Frabble.Types.Right 1
nextPos Vertical   = shift Down                1

prevPos :: Alignment -> Position -> Position
prevPos Horizontal = shift Frabble.Types.Left 1
prevPos Vertical   = shift Up                 1

adjacent :: Direction -> Position -> Position
adjacent d = shift d 1

isEmpty :: Board -> Position -> Bool
isEmpty b p = isNothing (tryFind p b)

offBoard :: Position -> Bool
offBoard (Pos col row) = col < head cols || col > last cols || row < head rows || row > last rows