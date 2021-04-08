module Frabble.Find (findXWords, findWord) where

import Frabble.Types
import Frabble.Useful
import Frabble.Navigation

-- Return all letters from a given position that lie in the specified direction
findLiveTiles :: Direction -> Board -> Position -> LiveWord
findLiveTiles d b p
    | offBoard p  = []
    | isEmpty b p = []
    | otherwise   = (findPair p b) : findLiveTiles d b (adjacent d p)       

findLiveTilesUp :: Board -> Position -> LiveWord
findLiveTilesUp b p = reverse (findLiveTiles Up b p)
    
findLiveTilesDown :: Board -> Position -> LiveWord
findLiveTilesDown = findLiveTiles Down
    
findLiveTilesLeft :: Board -> Position -> LiveWord
findLiveTilesLeft b p = reverse (findLiveTiles Frabble.Types.Left b p)
    
findLiveTilesRight :: Board -> Position -> LiveWord
findLiveTilesRight = findLiveTiles Frabble.Types.Right
    
-- Find a word that crosses perpendicular to an existing word at the specified
-- position
findXWord :: Alignment -> Board -> Position -> LiveWord
findXWord Horizontal b p = (findLiveTilesUp b p) ++ tail (findLiveTilesDown b p)
findXWord Vertical   b p = (findLiveTilesLeft b p) ++ tail (findLiveTilesRight b p)

findWord :: Alignment -> Board -> Position -> LiveWord
findWord Horizontal = findLiveTilesRight
findWord Vertical   = findLiveTilesDown

findXWords :: Alignment -> Board -> [LiveTile] -> [LiveWord]
findXWords a b ts = filter (\x -> length x > 1) [findXWord a b p | (p,_) <- ts]
