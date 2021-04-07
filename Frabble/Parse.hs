module Frabble.Parse where 
    
import Frabble.Types
import Text.Read

-- Parse a move
-- TODO: Consider using Parsec instead of handballing this?  
parseAlignment :: String -> Maybe Alignment
parseAlignment x
    | x == "Across" = Just Horizontal
    | x == "Down"   = Just Vertical
    | otherwise     = Nothing

parse2MoveComponents :: Position -> String -> String -> Either String Move
parse2MoveComponents pos a word = case alignmentResult of
                                      Nothing         -> Prelude.Left "The direction of your move should be either Across or Down"
                                      Just alignment  -> Prelude.Right (Move pos alignment word)
                                  where
                                      alignmentResult = parseAlignment a    

parsePositionRow :: Char -> String -> Either String Position
parsePositionRow col x = case rowResult of
                             Nothing  -> Prelude.Left "The start position of a move should be a column (A-O) & row (1-15)"
                             Just row -> Prelude.Right (Pos col row)
                         where
                             rowResult = readMaybe x :: Maybe Int

parsePositionComponents :: Char -> String -> Either String Position
parsePositionComponents c r = if notElem c cols
                                  then Prelude.Left "The start position of a move should be a column & row, column between A and O"
                                  else parsePositionRow c r

parsePosition :: String -> Either String Position
parsePosition p = if length p < 2 || length p > 3 
                     then Prelude.Left "The start position of a move should be a column & row, e.g. A12, or C8"
                     else parsePositionComponents (head p) (tail p)

parse3MoveComponents :: String -> String -> String -> Either String Move
parse3MoveComponents p a word = case posResult of
                                    Prelude.Left  e   -> Prelude.Left e
                                    Prelude.Right pos -> parse2MoveComponents pos a word
                                where 
                                    posResult = parsePosition p

parseMove :: String -> Either String Move
parseMove m = if length ws == 3 
                    then parse3MoveComponents (ws!!0) (ws!!1) (ws!!2)
                    else Prelude.Left "Move should have 3 parts - a position (like A1 or K10), direction (Down or Across), and a word"
               where
                   ws = words m
