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
-- DONE: Parse player's move
-- DONE: Basic validation - checkMove
-- DONE: Add tiles to board - addTiles
-- TODO: Check at least one tile from rack has been used
-- DONE: Check that both ends of word are adjacent to empty square or edge of board
-- TODO: Retrieve applicable bonuses
-- TODO: Find perpendicular words - findXWords
-- TODO: Check word connects with at least one existing word on board. Once
--   new tiles added to board . . .
--   - If first move of game, then OK
--   - If # letters used from rack < # letters in word then OK
--   - Otherwise there must be at least one adjoining perpendicular word
-- TODO: Check played word and perpendicular words are in dictionary
-- TODO: Calculate score - wordScore
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

{-
validateMove :: Board -> Board -> Rack -> Move -> Either String (Int,Rack)
validateMove bBefore bNow rNow m =
    if length tilesPlaced < 1
        then Left "You must place at least one tile from your rack"
        else 
    where
        tilesPlaced = bNow `without` bBefore 
-}

-- This does all the validation etc to parse a move, validate it, and apply to the board
tryMakeMove :: Board -> Rack -> String -> Either String (Board,Rack)
tryMakeMove b r m = do
    move <- parseMove m
    checkMove move
    (b',r') <- addTiles b r move
    checkWordBoundaries b' move
    return (b',r') 

-- Test getting a move and adding it to the board - effectively the main game loop
testGetMove :: Board -> Rack -> IO ()
testGetMove b r = do
        m <- getMove
        case tryMakeMove b r m of
            Prelude.Left  e       -> retryMove e
            Prelude.Right (b',r') -> nextMove b' r'    
        where
            retryMove e  = do putStrLn e
                              testGetMove b r 
            nextMove b r = do printBoard b bonuses
                              testGetMove b r
        
testFindXWords :: IO () 
testFindXWords = do
    let 
        Prelude.Right (b1,r1) = addTiles [] fullBag (Move (Pos 'D' 5) Vertical "STRING")
        Prelude.Right (b2,r2) = addTiles b1 fullBag (Move (Pos 'B' 7) Horizontal "BOREDOM")
        Prelude.Right (b3,r3) = addTiles b2 fullBag (Move (Pos 'D' 11) Horizontal "SPRINKLE")
        newTiles = b3 `without` b2
        xwords = findNewXWords Horizontal b3 newTiles
--        word = findWord Vertical b5 (Pos 'B' 5)                                                 
--        newBonuses = tryFindManyPair [p | (p,_) <- newTiles] bonuses
--        score = wordScore word newBonuses
    printBoard b3 bonuses   
    print xwords                      
--    print word
--    print newTiles
--    print newBonuses
--    print score