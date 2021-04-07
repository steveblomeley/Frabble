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
-- TODO: Check that both ends of word are adjacent to empty square or edge of board
-- TODO: Retrieve applicable bonuses
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

-- TODO: Strip this down to just the IO actions (prompt & get move)
-- Move the parse and check into an "Either monad" context, along with
-- all the other pure functions that validate & perform the move
getMove :: IO (Either String Move)
getMove = do
    putStrLn "Enter next move (e.g. A1 Across WORD) : "
    move <- getLine
    return (checkMove $ parseMove $ words move)

{-
validateMove :: Board -> Board -> Rack -> Move -> Either String (Int,Rack)
validateMove bBefore bNow rNow m =
    if length tilesPlaced < 1
        then Left "You must place at least one tile from your rack"
        else 
    where
        tilesPlaced = bNow `without` bBefore 
-}
        -- TODO: Retrieve applicable bonuses
-- DONE: Find perpendicular words - findXWords
-- TODO: Check word connects with at least one existing word on board. Once
--   new tiles added to board . . .
--   - If first move of game, then OK
--   - If # letters used from rack < # letters in word then OK
--   - Otherwise there must be at least one adjoining perpendicular word
-- TODO: Check played word and perpendicular words are in dictionary
-- DONE: Calculate score
-- TODO: refill rack


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
        Prelude.Left e -> retryMove e
        Prelude.Right m -> 
            case addTiles b r m of
                Prelude.Left e -> retryMove e
                Prelude.Right (b',r') -> do printBoard b' bonuses
                                            testGetMove b' r'
{-                                            
                    case validateMove b b' r r' m of
                        Prelude.Left e -> retryMove e
                        Prelude.Right s -> do printBoard b' bonuses
                                              testGetMove b' r'
-}                                              
    where
        retryMove s = do putStrLn s
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