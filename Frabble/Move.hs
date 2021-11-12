module Frabble.Move (addTiles, checkMove, checkWordBoundaries) where

import Data.Char
import Frabble.Types
import Frabble.Useful
import Frabble.Navigation

-- Basic move validation
onlyAtoZ :: Frabble.Types.Word -> Bool
onlyAtoZ = foldr (\c b -> b && elem c ['A'..'Z']) True

runsOffBoard :: Move -> Bool
runsOffBoard (Move (Pos col  _) Horizontal word) = lastColOfWord > lastColOnBoard
                                                   where
                                                       lastColOfWord  = ord col + length word - 1
                                                       lastColOnBoard = ord (last cols)
runsOffBoard (Move (Pos _ row) Vertical word) = lastRowOfWord > boardSize
                                                where
                                                    lastRowOfWord = row + length word - 1

checkMove :: Move -> Either String Move
checkMove (Move (Pos c r) a w)
    | r `notElem` rows                  = Prelude.Left ("Row should be in the range 1 to " ++ show boardSize)
    | c `notElem` cols                  = Prelude.Left ("Column should be in the range 'A' to " ++ show (last cols))
    | null w                            = Prelude.Left "Word must be at least 1 letter long"
    | not (onlyAtoZ w)                  = Prelude.Left "Word should contain only the letters 'A' to 'Z'"
    | runsOffBoard (Move (Pos c r) a w) = Prelude.Left "That word runs off the edge of the board"
    | otherwise                         = Prelude.Right (Move (Pos c r) a w)

-- Trio of functions to add tiles to board to complete a move, returning modified board & rack, & applicable bonuses
addNewTile :: Board -> Rack -> Move -> Either String (Board,Rack)
addNewTile b r (Move p a (t:ts)) =
    if t `notElem` r then
        Prelude.Left "That word uses a tile that you don't have"
    else
        addTiles boardWithTileAdded rackWithTileRemoved remainderOfMove
        where
            boardWithTileAdded = (p,t):b
            rackWithTileRemoved = r `without1` t
            remainderOfMove = Move (nextPos a p) a ts

useExistingTile :: Board -> Rack -> Move -> Tile -> Either String (Board,Rack)
useExistingTile b r (Move p a (t:ts)) tExisting =
    if t /= tExisting then
        Prelude.Left "One or more letters in that word do not match tiles already on the board"
    else
        addTiles b r remainderOfMove
        where
            remainderOfMove = Move (nextPos a p) a ts

addTiles :: Board -> Rack -> Move -> Either String (Board,Rack)
addTiles b r (Move _ _ []) = Prelude.Right (b,r)
addTiles b r (Move p a (t:ts)) =
    case tryFind p b of
        Nothing -> addNewTile b r (Move p a (t:ts))
        Just t' -> useExistingTile b r (Move p a (t:ts)) t'

-- Check a move starts and ends at either the edge of the board, or a blank square
checkWordBoundaries :: Board -> Move -> Either String Bool
checkWordBoundaries b (Move p a w) =
    if (offBoard pBeforeStart || isEmpty b pBeforeStart) &&
       (offBoard pAfterEnd    || isEmpty b pAfterEnd)
        then Prelude.Right True
        else Prelude.Left  "That word partly overlaps an existing word - Please enter the FULL new word"
    where
        pBeforeStart = prevPos a p
        pAfterEnd    = shift direction (length w) p
        direction    = if a == Horizontal then Frabble.Types.Right else Down
