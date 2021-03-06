import Frabble.Types
import Frabble.Useful
import Frabble.Dictionary
import Frabble.Scoring
import Frabble.Display
import Frabble.Parse
import Frabble.Navigation
import Frabble.Find
import Frabble.Move

-- A turn:
--       Parse player's move
--       Basic validation - checkMove
--       Add tiles to board - addTiles
--       Check at least one tile from rack has been used
--       Check that both ends of word are adjacent to empty square or edge of board
--       Check word connects with at least one existing word on board:
--       - If first move of game, then OK
--       - If # letters used from rack < # letters in word then OK
--       - Otherwise there must be at least one adjoining perpendicular word
--       Check that if first move then centre tile on board has been covered
--       Retrieve applicable bonuses
--       Find perpendicular words - findXWords
--       Check played word and perpendicular words are in dictionary
--       Calculate score - wordScore
--       Refill rack - fillRack
-- TODO: Calculate player's total score
-- TODO: Calculate next player
--       Next turn

-- The game flow is:
-- 
-- --> Player A makes move --> Valid move? ---> Yes --> Player B makes move --> Valid move? --> etc ..
--                                         \
--                                          +--> No --> Player A makes move --> Valid move? --> etc ..

fillRack :: Rack -> Bag -> IO (Rack,Bag)
fillRack rack bag = do
    ls <- randomPick bag (rackSize - length rack)
    return (rack ++ ls, bag `without` ls)

getTilesPlaced :: Board -> Board -> Either String [LiveTile]
getTilesPlaced bNew bOld =
    if null tilesPlaced
        then Prelude.Left  "You must place at least one tile from your rack"
        else Prelude.Right tilesPlaced
    where
        tilesPlaced = bNew `without` bOld

checkCentreSquareUsed :: Board -> Either String Bool
checkCentreSquareUsed b =
    case tryFind centreSquare b of
        Just _  -> Prelude.Right True
        Nothing -> Prelude.Left "First move of the game must use the centre square"

checkJoinsExistingTiles :: Board -> Board -> Rack -> Rack -> Move -> Either String Bool
checkJoinsExistingTiles bNew bOld rNew rOld (Move p a w) =
    if existingTilesUsed || perpendicularWords
        then Prelude.Right True
        else Prelude.Left  "Play a word that joins up with existing tiles on the board"
    where
        existingTilesUsed  = length w > length (rOld `without` rNew)
        perpendicularWords = not (null (findXWords a bNew (bNew `without` bOld)))

validateWordPlacement :: Board -> Board -> Rack -> Rack -> Move -> Either String Bool
validateWordPlacement bNew bOld rNew rOld (Move p a w) =
    if firstWordOnBoard
        then checkCentreSquareUsed bNew
        else checkJoinsExistingTiles bNew bOld rNew rOld (Move p a w)
    where
        firstWordOnBoard = null bOld

validateMove :: Board -> Board -> Rack -> Rack -> Move -> Either String [LiveTile]
validateMove bNew bOld rNew rOld move = do
    checkWordBoundaries bNew move
    tiles <- getTilesPlaced bNew bOld
    validateWordPlacement bNew bOld rNew rOld move
    return tiles

validateNewWords :: Dictionary -> Board -> Move -> [LiveTile] -> Either String [LiveWord]
validateNewWords d b (Move p a w) newTiles =
    case checkDictionary d (wordsToStrings newWords) of
        []       -> Prelude.Right newWords
        badWords -> Prelude.Left ("Not in dictionary: " ++ unwords badWords)
    where
        newWord   = findWord a b p
        newXWords = findXWords a b newTiles
        newWords  = newWord : newXWords

-- This does all the validation etc to parse a move, validate it, and apply to the board
-- Could define a new ADT to represent a special result from a turn - either an error, 
-- or a command (such as Quit, Redraw Board, etc)
tryMakeMove :: Dictionary -> Board -> Rack -> String -> Either String (Board,Rack,Int)
tryMakeMove d b r input = do
    move <- parseMove input
    checkMove move
    (b',r') <- addTiles b r move
    newTiles <- validateMove b' b r' r move
    newWords <- validateNewWords d b' move newTiles
    let score = calculateScore b' newWords newTiles r r'
    return (b',r',score)

showRack :: Rack -> IO ()
showRack r = putStrLn ("Your tiles are: " ++ interleave r (repeat ' '))

getMove :: Rack -> IO String
getMove r =
    showRack r >> putStr "Enter next move (e.g. A1 Across WORD)\n> " >> getLine

-- The main game loop - get, parse & validate a move, apply to the board, score it, refill
-- the players rack - repeat
play :: Dictionary -> Board -> Rack -> Bag -> IO ()
play d b r ts = do
        result <- tryMakeMove d b r <$> getMove r
        case result of
            Prelude.Left  e         -> retryMove e
            Prelude.Right (b',r',s) -> nextMove b' r' s
        where
            retryMove e    = do putStrLn e
                                play d b r ts
            nextMove b r s = do printBoard b bonuses
                                putStrLn ("Move scored " ++ show s)
                                (r',ts') <- fillRack r ts
                                play d b r' ts'

startGame :: IO ()
startGame = do
    (rack,bag) <- fillRack [] fullBag
    dict <- getDictionary
    printBoard [] bonuses
    play dict [] rack bag