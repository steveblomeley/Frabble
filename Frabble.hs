import Frabble.Types
import Frabble.Useful
import Frabble.Scoring
import Frabble.Display
import Frabble.Parse
import Frabble.Navigation
import Frabble.Find
import Frabble.Move

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
-- TODO: Check that if first move then covers centre tile on board
--       Retrieve applicable bonuses
--       Find perpendicular words - findXWords
-- TODO: Check played word and perpendicular words are in dictionary
--       Calculate score - wordScore
-- TODO: refill rack - fillRack
-- TODO: Calculate player's total score
-- TODO: Calculate next player
-- TODO: Next turn

-- The game flow is:
-- 
-- --> Player A makes move --> Valid move? ---> Yes --> Player B makes move --> Valid move? --> etc ..
--                                         \
--                                          +--> No --> Player A makes move --> Valid move? --> etc ..

getMove :: IO String
getMove = putStr "Enter next move (e.g. A1 Across WORD)\n> " >> getLine >>= return

getTilesPlaced :: Board -> Board -> Either String [LiveTile]
getTilesPlaced bNew bOld = 
    if length tilesPlaced < 1
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
    tiles <- getTilesPlaced bNew bOld
    validateWordPlacement bNew bOld rNew rOld move
    return tiles

validateNewWords :: Board -> Move -> [LiveTile] -> Either String [LiveWord]
validateNewWords b (Move p a w) newTiles =
    --TODO: return Left "error" if any new words not in dictionary
    Prelude.Right (newWord : newXWords)
    where
        newWord = findWord a b p
        newXWords = findXWords a b newTiles

-- TODO: 50 point bonus for emptying rack
calculateScore :: Board -> [LiveWord] -> [LiveTile] -> Int
calculateScore b ws ts = 
    sum (map (\w -> wordScore w bs) ws)
    where
        bs = tryFindManyPair [p | (p,_) <- ts] bonuses
        
-- This does all the validation etc to parse a move, validate it, and apply to the board
tryMakeMove :: Board -> Rack -> String -> Either String (Board,Rack,Int)
tryMakeMove b r m = do
    move <- parseMove m
    checkMove move
    (b',r') <- addTiles b r move
    checkWordBoundaries b' move
    newTiles <- validateMove b' b r' r move
    newWords <- validateNewWords b' move newTiles
    let score = calculateScore b' newWords newTiles
    return (b',r',score) 

-- Test getting a move and adding it to the board - effectively the main game loop
play :: Board -> Rack -> IO ()
play b r = do
        m <- getMove
        case tryMakeMove b r m of
            Prelude.Left  e         -> retryMove e
            Prelude.Right (b',r',s) -> nextMove b' r' s
        where
            retryMove e    = do putStrLn e
                                play b r 
            nextMove b r s = do printBoard b bonuses
                                putStrLn ("Move scored " ++ (show s))
                                play b r
