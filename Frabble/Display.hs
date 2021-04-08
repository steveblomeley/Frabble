module Frabble.Display (printBoard, interleave) where

import Frabble.Types
import Frabble.Useful

{-
   Display the board, like this:

     A   B   C   D  etc...
    --- --- --- --- 
1  |   | Y |   |   |
    --- --- --- --- 
2  | H | E | L | P |
    --- --- --- --- 
3  |   | L |   |   |
    --- --- --- --- 
4  |   | P |   |   |
    --- --- --- ---

-}

interleave :: [a] -> [a] -> [a]
interleave [] (y:ys) = [y]
interleave (x:xs) [] = [x]
interleave (x:xs) ys = x : interleave ys xs

printHeader :: IO ()
printHeader = putStrLn ("  "   ++ concat(interleave (repeat "   ") (map (\x -> [x]) cols)))

printDivider :: IO ()
printDivider = putStrLn ("    " ++ concat(interleave (replicate boardSize "---") (repeat " ")))

bonusContent :: Position -> BonusBoard -> String
bonusContent pos board = case tryFind pos board of
                             Nothing -> "   "
                             Just x  -> " " ++ (show x) ++ " "                          

squareContent :: Position -> Board -> BonusBoard -> String
squareContent pos b bb = case tryFind pos b of
                             Nothing -> bonusContent pos bb
                             Just x  -> " " ++ [x] ++ " "                          

rowContents :: Board -> BonusBoard -> Int -> [String]
rowContents b bb row = [squareContent (Pos col row) b bb | col <- cols]

rowNumber :: Int -> String
rowNumber row = n ++ (concat(replicate (3-l) " "))
                where n = show row
                      l = length n 

printRow :: Board -> BonusBoard -> Int -> IO ()
printRow b bb row = putStrLn ((rowNumber row) ++ concat(interleave (repeat "|") (rowContents b bb row)))

printBody :: Board -> BonusBoard -> Int -> IO ()
printBody b bb row
    | row > boardSize = do printDivider
    | otherwise       = do printDivider 
                           printRow b bb row
                           printBody b bb (row+1)

printBoard :: Board -> BonusBoard ->IO ()
printBoard b bb = do
    printHeader
    printBody b bb 1
