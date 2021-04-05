import Data.Char
import System.Random
import Text.Read
import Data.List

-- Simple data types - declared to make function type declarations more explicit
boardSize :: Int
boardSize = 15

rackSize :: Int
rackSize = 7

cols :: [Char]
cols = take boardSize ['A'..'Z']

rows :: [Int]
rows = [1..boardSize]

type Tile = Char
type Word = [Tile]
type Bag = [Tile]
type Rack = [Tile]
type Board = [(Position,Tile)]

data Bonus = Word Int | Letter Int deriving Eq
type LiveBonus = (Position,Bonus)
type LiveTile = (Position,Tile)
type LiveWord = [LiveTile]
type BonusBoard = [LiveBonus] 
type LiveBonuses = [LiveBonus]

instance Show Bonus where
    show (Word n) 
        | n == 2    = "+"
        | n == 3    = "*"
        | otherwise = "?"
    show (Letter n)
        | n == 2    = "-"
        | n == 3    = "="
        | otherwise = "?"

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

ltrCounts = [9,2,2,4,12,2,3,2,9,1,1,4,2,6,8,2,1,6,4,6,4,2,2,1,2,1]
ltrScores = [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

fullBag :: Bag
fullBag = concat . zipWith replicate ltrCounts $ ['A'..'Z']

scores = zip ['A'..'Z'] ltrScores

tryFind :: Eq k => k -> [(k,v)] -> Maybe v
tryFind k kvs = if null vs then Nothing else Just (head vs)
                where 
                    vs = [v | (k',v) <- kvs, k' == k]

tryFindPair :: Eq k => k -> [(k,v)] -> Maybe (k,v)
tryFindPair k kvs = if null kvs' then Nothing else Just (head kvs')
                    where 
                        kvs' = [(k',v) | (k',v) <- kvs, k' == k]

tryFindManyPair :: (Eq k, Eq v) => [k] -> [(k,v)] -> [(k,v)]
tryFindManyPair ks kvs = nub [(k,v) | (k,v) <- kvs, k' <- ks, k' == k]         

find :: Eq k => k -> [(k,v)] -> v
find k kvs = head [v | (k',v) <- kvs, k' == k]

findPair :: Eq k => k -> [(k,v)] -> (k,v)
findPair k kvs = head [(k',v) | (k',v) <- kvs, k' == k]

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
data Move = Move Position Alignment Main.Word deriving (Show, Read)

-- Randomly pick tiles from bag to rack
randomPick1 :: [a] -> IO a
randomPick1 xs = do
    i <- randomRIO (0, (length xs) - 1)
    return (xs !! i)    

randomPick :: Eq a => [a] -> Int -> IO [a]
randomPick _ 0  = return []
randomPick xs n = do
    x   <- randomPick1 xs
    xs' <- randomPick (xs `without1` x) (n-1)
    return (x : xs')

without1 :: Eq a => [a] -> a -> [a]
without1 [] y = []
without1 (x:xs) y 
   | x == y    = xs
   | otherwise = x : (xs `without1` y)

without :: Eq a => [a] -> [a] -> [a]
without xs []     = xs
without xs (y:ys) = (xs `without1` y) `without` ys 

fillRack :: Rack -> Bag -> IO (Rack,Bag)
fillRack rack bag = do
    ls <- randomPick bag (rackSize - (length rack))
    return (rack ++ ls, bag `without` ls)

testFillRack :: IO ()
testFillRack = do
    print fullBag
    (r,b) <- fillRack [] fullBag
    print r
    print b

-- Basic move validation
onlyAtoZ :: Main.Word -> Bool
onlyAtoZ = foldr (\c b -> b && elem c ['A'..'Z']) True

runsOffBoard :: Move -> Bool
runsOffBoard (Move (Pos col  _) Horizontal word) = lastColOfWord > lastColOnBoard
                                                   where
                                                       lastColOfWord  = ord col + length word - 1
                                                       lastColOnBoard = ord (last cols)
runsOffBoard (Move (Pos _ row) Vertical word) = lastRowOfWord > boardSize
                                                where
                                                    lastRowOfWord = row + length word - 1

checkMove :: Move -> Either String Bool
checkMove (Move (Pos c r) a w) 
    | not (elem r rows)                 = Prelude.Left ("Row should be in the range 1 to " ++ (show boardSize))
    | not (elem c cols)                 = Prelude.Left ("Column should be in the range 'A' to " ++ (show $ last cols))
    | length w < 2                      = Prelude.Left ("Word must be at least 1 letter long")
    | not (onlyAtoZ w)                  = Prelude.Left ("Word should contain only the letters 'A' to 'Z'")
    | runsOffBoard (Move (Pos c r) a w) = Prelude.Left ("That word runs off the edge of the board")
    | otherwise                         = Prelude.Right True

-- Model the board
--
-- How do we add a word to the board?
--
-- First consider how we pass the state of the board between moves
-- - Pass the board itself (i.e. the kv list)
-- - Pass the list of moves so far, and play them against a blank board
-- Let's choose to pass the board initially
--
-- Checks still to be added:
--   Once the word has been added to the board, check that at least one tile
--   has been used from the player's rack, and that the word adjoins letters
--   that are already on the board
--
-- Perpendicular words & dictionary check
--   Search for perpendicular words from each position in the new word.
--   Then check that the played word, plus all perp words are in the dictionary.
--
-- State
--   What state needs to be passed to next turn? 
--   - Board
--   - Bonuses
--   - Racks (pass ALL racks in play - not just the one that has the next turn)
--   - Players' scores
--   - Player whose turn it is next

offBoard :: Position -> Bool
offBoard (Pos col row) = col < head cols || col > last cols || row < head rows || row > last rows 

shift :: Direction -> Int -> Position -> Position
shift Up         n (Pos c r) = Pos c (r - n)
shift Down       n (Pos c r) = Pos c (r + n)
shift Main.Right n (Pos c r) = Pos (chr((ord c) + n)) r
shift Main.Left  n (Pos c r) = Pos (chr((ord c) - n)) r

nextPos :: Alignment -> Position -> Position
nextPos Horizontal = shift Main.Right 1
nextPos Vertical   = shift Down       1

prevPos :: Alignment -> Position -> Position
prevPos Horizontal = shift Main.Left 1
prevPos Vertical   = shift Up        1

liveBonus :: Position -> LiveBonuses -> LiveBonuses
liveBonus p lbs = case tryFindPair p bonuses of
                        Nothing -> lbs
                        Just lb -> (lb:lbs)

-- Add tiles to board to complete move; return modified board & rack, and applicable bonuses
addNewTile :: Board -> Rack -> Move -> Either String (Board,Rack)
addNewTile b r (Move p a (t:ts)) = 
    if notElem t r then 
        Prelude.Left "That word needs a tile that isn't on your rack" 
    else
        addTiles boardWithTileAdded rackWithTileRemoved remainderOfMove
        where boardWithTileAdded = (p,t):b
              rackWithTileRemoved = r `without1` t
              remainderOfMove = Move (nextPos a p) a ts

useExistingTile :: Board -> Rack -> Move -> Tile -> Either String (Board,Rack)
useExistingTile b r (Move p a (t:ts)) tExisting =
    if t /= tExisting then
        Prelude.Left "One or more letters in that word do not match tiles already on the board"
    else
        addTiles b r remainderOfMove
        where remainderOfMove = Move (nextPos a p) a ts

addTiles :: Board -> Rack -> Move -> Either String (Board,Rack)
addTiles b r (Move _ _ []) = Prelude.Right (b,r)
addTiles b r (Move p a (t:ts)) = 
    case tryFind p b of
        Nothing -> addNewTile b r (Move p a (t:ts))
        Just t' -> useExistingTile b r (Move p a (t:ts)) t'

isEmpty :: Board -> Position -> Bool
isEmpty b p = tryFind p b == Nothing                     

-- Check a move starts and ends at either the edge of the board, or a blank square
checkWordBoundaries :: Board -> Move -> Bool
checkWordBoundaries b (Move p a w) = (offBoard pBeforeStart || isEmpty b pBeforeStart) &&
                                     (offBoard pAfterEnd    || isEmpty b pAfterEnd)
                                     where
                                         pBeforeStart  = prevPos a p
                                         pAfterEnd     = nextPos a lastPosInWord
                                         lastPosInWord = shift direction (length w) p
                                         direction     = if a == Horizontal then Main.Right else Down

-- Return the adjacent position, in the specified direction                                         
adjacent :: Direction -> Position -> Position
adjacent d = shift d 1

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
findLiveTilesLeft b p = reverse (findLiveTiles Main.Left b p)
    
findLiveTilesRight :: Board -> Position -> LiveWord
findLiveTilesRight = findLiveTiles Main.Right
    
-- Find a word that crosses perpendicular to an existing word at the specified
-- position
findXWord :: Alignment -> Board -> Position -> LiveWord
findXWord Horizontal b p = (findLiveTilesUp b p) ++ tail (findLiveTilesDown b p)
findXWord Vertical   b p = (findLiveTilesLeft b p) ++ tail (findLiveTilesRight b p)

findWord :: Alignment -> Board -> Position -> LiveWord
findWord Horizontal = findLiveTilesRight
findWord Vertical   = findLiveTilesDown

findXWords :: Alignment -> Board -> Position -> [LiveWord]
findXWords a b p
    | offBoard p  = []
    | isEmpty b p = []
    | otherwise   = if length word > 1
                       then word : findXWords a b (nextPos a p)
                       else findXWords a b (nextPos a p)
                    where
                        word = findXWord a b p

-- A turn:
-- DONE: Parse player's move
-- DONE: Basic validation - checkMove
-- DONE: Add tiles to board & retrieve applicable bonuses - addTiles
-- DONE: Find perpendicular words - findXWords
-- TODO: Check word connects with at least one existing word on board. Once
--   new tiles added to board . . .
--   - If first move of game, then OK
--   - If # letters used from rack < # letters in word then OK
--   - Otherwise there must be at least one adjoining perpendicular word
-- TODO: Check played word and perpendicular words are in dictionary
-- DONE: Calculate score
-- TODO: refill rack
-- TODO: Calculate player's total score
-- TODO: Calculate next player
-- TODO: Next turn

-- Play function
--
-- move <- getMove       -- read the player's move for this turn
-- checkMove move
-- checkWordBoundaries board move
-- addTiles 
-- check that at least one tile was used from player's rack
-- findXWords
-- checkFlow             -- Does the word connect with existing words on board? At this
-- checkDictionary          point we have the info we need to work this out, i.e. is this
-- calculateScore           1st move of game? is word > num tiles used? are there perp words?
-- refillRack
-- play dictionary board bonuses racks scores
--
-- The basic flow we want is:
-- 
-- --> Player A makes move --> Valid move? ---> Yes --> Player B makes move --> Valid move? --> etc ..
--                                         \
--                                          +--> No --> Player A makes move --> Valid move? --> etc ..


-- Print the board, like this:
{-

     A   B   C   D  etc...
   +---+---+---+---+
1  |   | Y |   |   |
   +---+---+---+---+
2  | H | E | L | P |
   +---+---+---+---+
3  |   | L |   |   |
   +---+---+---+---+
4  |   | P |   |   |
   +---+---+---+---+

-}
interleave :: [a] -> [a] -> [a]
interleave [] (y:ys) = [y]
interleave (x:xs) [] = [x]
interleave (x:xs) ys = x : interleave ys xs

printHeader :: IO ()
printHeader = putStrLn ("  "   ++ concat(interleave (repeat "   ") (map (\x -> [x]) cols)))

printDivider :: IO ()
printDivider = putStrLn ("   +" ++ concat(interleave (replicate boardSize "---") (repeat "+")))

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

-- Parse a move
-- TODO: Consider using Parsec instead of handballing this?  
parseAlignment :: String -> Maybe Alignment
parseAlignment x
    | x == "Across" = Just Horizontal
    | x == "Down"   = Just Vertical
    | otherwise     = Nothing

parse2MoveComponents :: Position -> [String] -> Either String Move
parse2MoveComponents p (x:xs) = case alignment of
                                    Nothing -> Prelude.Left "The direction of your move should be either Across or Down"
                                    Just a  -> Prelude.Right (Move p a (head xs))
                                where
                                    alignment = parseAlignment x    

parsePositionRow :: Char -> String -> Either String Position
parsePositionRow col x = case r of
                             Nothing  -> Prelude.Left "The start position of a move should be a column & row, row between 1 and 15"
                             Just row -> if row < 0 || row > 15
                                            then Prelude.Left "The start position of a move should be a column & row, row between 1 and 15"
                                            else Prelude.Right (Pos col row)
                         where
                             r = readMaybe x :: Maybe Int

parsePositionComponents :: Char -> String -> Either String Position
parsePositionComponents c r = if notElem c cols
                                  then Prelude.Left "The start position of a move should be a column & row, column between A and O"
                                  else parsePositionRow c r

parsePosition :: String -> Either String Position
parsePosition p = if length p < 2 || length p > 3 
                     then Prelude.Left "The start position of a move should be a column & row, e.g. A12, or C8"
                     else parsePositionComponents (head p) (tail p)

parse3MoveComponents :: [String] -> Either String Move
parse3MoveComponents (w:ws) = case pos of
                                 Prelude.Left s  -> Prelude.Left s
                                 Prelude.Right p -> parse2MoveComponents p ws
                              where 
                                  pos = parsePosition w 

parseMove :: [String] -> Either String Move
parseMove ws = if length ws == 3 
                   then parse3MoveComponents ws
                   else Prelude.Left "Move should have 3 parts - a position (like A1 or K10), direction (Down or Across), and a word"

getMove :: IO (Either String Move)
getMove = do
    putStrLn "Enter next move (e.g. A1 Across WORD) : "
    move <- getLine
    return (parseMove (words move))

-- Score a word
wordBonus :: Position -> LiveBonuses -> Int
wordBonus p bs = case b of
                     Just (Word n) -> n
                     _             -> 1
                 where b = tryFind p bs

letterBonus :: Position -> LiveBonuses -> Int
letterBonus p bs = case b of
                       Just (Letter n) -> n
                       _               -> 1
                   where b = tryFind p bs
                 
wordBonuses :: LiveWord -> LiveBonuses -> Int
wordBonuses [] _ = 1
wordBonuses (x:xs) bs = (wordBonus p bs) * (wordBonuses xs bs)
                        where (p,_) = x 

letterScores :: LiveWord -> LiveBonuses -> Int
letterScores [] _ = 0
letterScores (x:xs) bonuses = (score * bonus) + (letterScores xs bonuses)
                              where (pos,letter) = x
                                    score = Main.find letter scores 
                                    bonus = letterBonus pos bonuses        

wordScore :: LiveWord -> LiveBonuses -> Int
wordScore word bonuses = (letterScores word bonuses) * (wordBonuses word bonuses)


-- Get a move - ANY move - and add it to the board
-- Q: How to avoid endless indentation in control flow?
--    - Could encapsulate components of game state in a type
--      Each function could then accept & return "Either String GameState"
--      Pattern match each function so that:
--      - Left s = Left s                        (i.e. return immediately)
--      - Right gs = do stuff with game state
--      But . . . would need to incorporate move into game state. Not simple,
--      may need to pass move history between turns, instead of board state.
--      Then play previous moves into blank board anytime we need the current
--      state of the board?
--      

testGetMove :: Board -> Rack -> IO ()
testGetMove b r = do
    m <- getMove
    case m of 
        Prelude.Left s -> retryMove s
        Prelude.Right m -> 
            case checkMove m of
                Prelude.Left s -> retryMove s
                Prelude.Right _ ->
                    case addTiles b r m of
                        Prelude.Left s -> retryMove s
                        Prelude.Right (b',r') -> do printBoard b' bonuses
                                                    testGetMove b' r'
    where
        retryMove s = do putStrLn s
                         testGetMove b r                                                    
 
testFindXWords :: IO () 
testFindXWords = do
    let 
        Prelude.Right (b1,r1) = addTiles [] fullBag (Move (Pos 'D' 5) Vertical "STRING")
        Prelude.Right (b2,r2) = addTiles b1 fullBag (Move (Pos 'B' 7) Horizontal "BOREDOM")
        Prelude.Right (b3,r3) = addTiles b2 fullBag (Move (Pos 'D' 10) Horizontal "GUAGE")
        Prelude.Right (b4,r4) = addTiles b3 fullBag (Move (Pos 'A' 5) Horizontal "BITS")
        Prelude.Right (b5,r5) = addTiles b4 fullBag (Move (Pos 'B' 5) Vertical "IMBIBE")
        word = findWord Vertical b5 (Pos 'B' 5)                                                 
        newTiles = b5 `without` b4
        newBonuses = tryFindManyPair [p | (p,_) <- newTiles] bonuses
        score = wordScore word newBonuses
    printBoard b5 bonuses                         
    print word
    print newTiles
    print newBonuses
    print score
    --print score