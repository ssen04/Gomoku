module MainGame where
-- Import functions and data types from Setup.hs
import Setup
import Helper

-- Using Gloss for project GUI
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
    ( Key(MouseButton, Char, SpecialKey),
      KeyState(Down),
      MouseButton(LeftButton),
      Event(EventKey),
      SpecialKey(KeyEsc) )
import Data.List ( nub, group, tails, transpose )
import Data.Maybe
import System.Exit ( exitWith, ExitCode( ExitSuccess ) )
import System.IO.Error (tryIOError)
import Control.Concurrent (threadDelay)

-- Declares the status of a Gomoku game being played
-- Game status can be inProgress where there is no result
--                    won by one of the players
--                    draw where none of the players won
data GameStatus = InProgress | Won Player | Draw
                  deriving (Eq, Show)

-- Game state
data GameState = GameState
  { board   :: Board -- Store current state of the board - which cells are occupied by which players the cells that are empty (type Board from Setup.hs)
  , currentPlayer :: Player -- Demonstrates player's turn (type Player) - either X or Y.
  , gameStatus :: GameStatus --  Rep the current game status, whether it is in progress, won by a player, or a draw (type GameStatus)
  , availableMoves :: [(Int, Int)] -- Store what moves are available on the board. 
  , mode :: Bool
  }

-- To put GameState into Show; to make one string file
instance Show GameState where
  show (GameState board currentPlayer gameStatus moves mode) = show board ++ "\n"
    ++ show currentPlayer ++ "\n" ++ show gameStatus ++ "\n" ++ show moves ++ "\n" ++ show mode

-- Initial game state the initial state of the game
-- initial state is assigned the value empty bpard (start of the game board is empty)
--                               x is the player that starts the game (!can change later!)
--                               in progress is the gameStatus since in the beginning of the game neither of the players has won or lost
initialState :: Bool -> Player -> GameState
initialState mode player = GameState emptyBoard player InProgress [] mode

-- Check if a move is valid
-- x >= 0, y>=0 to ensure non-negative x and y coordinates
-- x < boardsize and y<boardsize to ensure x and y coords within [0, boardsize-1]
-- isNothing (b !! y !! x): coordinate (x,y) on board is empty (then the player can place his move there, or else not valid)
--                          !! to check access element at pos (x,y)
isValidMove :: Board -> (Int, Int) -> Bool
isValidMove b (x, y) = x >= 0 && x < boardSize && y >= 0 && y < boardSize && isNothing (b !! y !! x)

-- Mouse event handling
-- source: https://stackoverflow.com/questions/15867671/getting-mouse-coordinates-in-glosshaskell
-- EventKey (MouseButton LeftButton) Down _ mousePos: This pattern matches when a left mouse (MouseButton LeftButton) button is pressed (down). The program gets the mouse position (mousePos) when button is prssed
-- gameState@(GameState b _ InProgress _): @ Symbol simultaneously match a pattern and bind the entire structure to a variable.
--                                      Here, gameState@(GameState b _ InProgress _) matches the GameState structure (GameState type consists of three fields: board, currentPlayer, and gameStatus), and binds the entire structure to the variable gameState.
-- Key event handling
-- source: https://stackoverflow.com/questions/52871673/haskell-gloss-do-something-every-frame-key-is-pressed
-- Using case expressions
-- source: https://zvon.org/other/haskell/Outputsyntax/caseQexpressions_reference.html
handleEventIO :: Event -> GameState -> IO GameState
handleEventIO event gameState@(GameState _ player _ moves mode) =
  case event of
    EventKey (SpecialKey KeyEsc) _ _ _ -> exitWith ExitSuccess -- ESC key is not working in playIO; should be implemented explicitly
    EventKey (MouseButton LeftButton) Down _ mousePos ->
      return $ handleEventMousePress event gameState
    EventKey (Char 's') Down _ _ -> do
      saveGameState gameState
      return gameState
--    EventKey (Char 'l') Down _ _ -> do
--      loaded <- loadGameState
--      case loaded of
--        Just loadedState -> do -- Keyword 'Just': https://stackoverflow.com/questions/18808258/what-does-the-just-syntax-mean-in-haskell
----          putStrLn "Loaded game state: "
----          print loadedState
--          return loadedState
--        Nothing -> do
--          putStrLn "Failed to load game state."
--          return gameState
    _ -> if mode && player == Y
           then do 
            threadDelay 500000
            updateIO 0 (aiPlayer gameState)  -- Render AI's move if it's AI's turn
          else return gameState -- wild-card pattern match for all other cases

aiPlayer :: GameState -> GameState
aiPlayer gameState@(GameState b player _ moves mode) = 
  if (isWinner b X) then gameState 
  else makeNewMove gameState (fst (minimax gameState 1))

handleEventMousePress :: Event -> GameState -> GameState
handleEventMousePress (EventKey (MouseButton LeftButton) Down _ mousePos) gameState@(GameState b player InProgress moves mode) =
  let (x, y) = mouseToCellCoords mousePos -- Calculate the grid cell coordinates (x, y) corresponding to the position of mouse (mousePos). mouseToCellCoords to convert mouse coordinates -> cell coordinates
  in if isValidMove b (x, y) then makeNewMove gameState (x, y) -- Check if the move converted to the cell coordinates (x, y) is a valid move on the game board b
        else gameState -- Else -> return current game state (no new changes made)
handleEventMousePress _ gameState = gameState

-- Save the data as a string file; separated by lines for simplicity
saveGameState :: GameState -> IO ()
saveGameState gameState = do
  writeFile "gameState.txt" (show gameState)
  putStrLn("Saved successfully.")

-- Load the data from the string file and return by calling parseGameState function
-- tryIOError: https://hackage.haskell.org/package/base-4.19.1.0/docs/System-IO-Error.html
loadGameState :: IO (Maybe GameState)
loadGameState = do
  result <- tryIOError $ readFile "gameState.txt"
  case result of
    Left _ -> do
      return Nothing
    Right contents -> return $ parseGameState contents

-- Parse the game state
parseGameState :: String -> Maybe GameState
parseGameState contents = case lines contents of
  [boardStr, playerStr, statusStr, moveStr, modeStr] -> do
    board <- Just (read boardStr :: [[Maybe Player]]) -- Added 'deriving Read' to 'data Player' for parsing here
    player <- case playerStr of
      "X" -> Just X
      "Y" -> Just Y
      _ -> Nothing
    status <- case statusStr of
      "InProgress" -> Just InProgress
      "Won X" -> Just (Won X)
      "Won Y" -> Just (Won Y)
      "Draw" -> Just Draw
      _ -> Nothing
    moves <- Just (read moveStr :: [(Int, Int)])
    mode <- Just (read modeStr :: Bool)
    Just (GameState board player status moves mode)
  _ -> Nothing

-- Draw function
-- !Use of pattern matching! Decompose GameState into b (the board) and game status
draw :: GameState -> Picture
draw gameState@(GameState b _ status _ _) = pictures [ drawGomokuBoard b, drawStatus status]

-- adjust y coordinate of display text if a player won
ycoordDisplayTextWon = fromIntegral (windowHeight `div` 2) + 70
-- adjust y coordinate of display text if the game is draw
ycoordDisplayTextDraw = fromIntegral (windowHeight `div` 2) + 30

-- If a player has won - Get player as input,
--        (fromIntegral (windowHeight 'div' 2) - 10): Divide windowHeight by 2 to find the vertical midpoint of the window and subtract 10 from the midpoint (!CHANGE LATER FOR BETTER VISUAL!)
--        scale 0.2 0.2: Scale the result above
--        color (playerColor player): Display the winning text in the color of the player who won the game
-- If the game draws, display text saying the game is a draw

drawStatus (Won player) = translate (-100) (ycoordDisplayTextWon) $ scale 0.35 0.35 $ color (playerColor player) $ text ("Player " ++ show player ++ " won!")
drawStatus Draw = translate (-250) (ycoordDisplayTextDraw) $ scale 0.35 0.35 $ color red $ text "The Gomoku game is a Draw!"
drawStatus _ = blank

-- Update function: state update logic of the game
-- source: https://gamedev.stackexchange.com/questions/148361/when-where-should-a-game-state-transition-take-place
updateIO :: Float -> GameState -> IO GameState
updateIO _ gameState = return gameState

-- Place a piece on the board
-- source: https://www21.in.tum.de/teaching/fpv/WS20/wett/week07/ge35zur/Exercise08.hs
-- It takes the first y rows of the board b (excluding row y) and appends them to the new board -> unchanged rows from the top of the board
-- take x (b !! y): take the first x elements (extracts the elements from the beginning of the row up to index x (exclusive)) from the row of the board b located at position y
-- [Just p]: create list containing the player's piece Just p.
-- drop (x + 1) (b !! y): remove the elements from index 0 to index x (inclusive), leaving the elements from index x + 1 onwards in the row of the board b located at position y
-- drop (y+1) b: drop the first y + 1 of the board b and appends the remaining rows to the new board to ensure the unchanged rows from the bottom of the board are intact
placePieceOnBoard :: Board -> (Int, Int) -> Player -> Board
placePieceOnBoard b (x, y) p = take y b ++ [putPiece] ++ drop (y + 1) b
  where putPiece = take x (b !! y) ++ [Just p] ++ drop (x + 1) (b !! y)

-- Place a piece on the board
-- Takes a game state and (Int, Int represents the coordinates of the move) and returns the modified game state
makeNewMove :: GameState -> (Int, Int) -> GameState
makeNewMove (GameState b currentPlayer _ availableMoves mode) (x, y) =
  let newBoard = placePieceOnBoard b (x, y) currentPlayer -- produce the new board by placing the current player's piece at the coordinates (x, y)
      newStatus -- determine new status
        | isWinner newBoard currentPlayer = Won currentPlayer -- If the current player has won -> status is set to Won currentPlayer
        | isDraw newBoard = Draw  -- If the game draws -> status set to draw
        | otherwise = InProgress -- else -> status remains in progress
      nextPlayer = if currentPlayer == X then Y else X -- choose the next player
      newAvailableMoves = neighboringIndices newBoard
  in GameState newBoard nextPlayer newStatus newAvailableMoves mode -- new gameState with the updated newBoard, nextPlayer, newStatus of the game, and newAvaiableMoves

-- AI implementation: 
inf :: Double
inf = 1 / 0 -- Positive infinity

count :: [[Maybe Player]] -> Int
count matrix = length [() | row <- matrix, cell <- row, not (isNothing cell)]

minimax :: GameState -> Int -> ((Int, Int), Double)
minimax st depth 
    | length avail == 0 = ((div boardSize 2, div boardSize 2), 0) -- Condition for empty board 
    | currPlayer == Y = argmax (moveAction st depth) avail (-inf) inf
    | currPlayer == X = argmin (moveAction st depth) avail (-inf) inf
    where GameState b currPlayer _ avail _ = st 

moveAction :: GameState -> Int -> (Int, Int) -> Double 
moveAction st depth move = moveValue (makeNewMove st move) depth

moveValue :: GameState -> Int -> Double 
moveValue st depth
    | status == Draw = 0 -- When neither player wins, return a score of 0
    | status == Won Y = 100000 -- When the AI wins, return a score of 1
    | status == Won X = -100000 -- When the player wins, return a score of -1
    | depth == 0 = if player == Y then evaluateBoard board player boardPositions else - (evaluateBoard board player boardPositions) 
    | otherwise = snd (minimax st (depth - 1)) 
    where GameState board player status _ _ = st 

-- Evaluates the given board to see how to score the board 
evaluate :: GameState -> Double
evaluate st 
    | (checkSequences board player 4) = if player == Y then 1000 else -1000
    | (checkSequences board player 3) = if player == Y then 100 else -100
    | (checkSequences board player 2) = if player == Y then 10 else -10
    | otherwise = 0
    where GameState board player _ _ _ = st

-- Inspiration was dervied from this source: https://github.com/sowakarol/gomoku-haskell/blob/master/Game.hs
evaluateBoard :: Board -> Player -> [[(Int, Int)]] -> Double
evaluateBoard board player points = 
    sum [
        ((rateVertical board player (points !! (x-1)) 0) + (rateVertical board player ((transpose . reverse) points !! (x-1)) 0) + (rateVertical board player ((diags points) !! (x-1)) 0) + (rateVertical board player ((diags ((transpose . reverse) points)) !! (x-1)) 0) ) - 
        ((rateVertical board swapPlayer (points !! (x-1)) 0)) - 
        (rateVertical board swapPlayer ( (transpose . reverse) points !! (x-1)) 0) - 
        (rateVertical board swapPlayer ((diags points) !! (x-1)) 0) - 
        (rateVertical board swapPlayer ((diags ( (transpose . reverse) points)) !! (x-1)) 0) | x <- [1..boardSize]
    ]
    where 
        swapPlayer = if player == Y then X else Y

rateVertical :: Board -> Player -> [(Int, Int)] -> Int -> Double
rateVertical board player ((x,y):ts) rate = if (comparePieceOnBoard (board !! y !! x) player) then rateVertical board player ts (rate+1) else (rateFragment rate) + (rateVertical board player ts 0)
rateVertical board player [] rate 
    | rate == 0 = 0
    | rate == 1 = 5
    | rate == 2 = 12
    | rate == 3 = 100
    | rate == 4 = 1000
    | rate == 5= 10000

rateFragment:: Int -> Double
rateFragment rate
    | rate == 0 = 0
    | rate == 1 = 5
    | rate == 2 = 10
    | rate == 3 = 100
    | rate == 4 = 1000
    | rate == 5 = 10000

diags :: [[a]] -> [[a]]
diags = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

comparePieceOnBoard :: Maybe Player -> Player -> Bool
comparePieceOnBoard Nothing _ = False  
comparePieceOnBoard (Just piece) player = piece == player

checkSequences :: Board -> Player -> Int -> Bool
checkSequences b p n = any (checkNLine p n) (rows ++ cols ++ diags)
    where
        rows = b
        cols = transpose b
        diags = diagonals b

checkNLine :: Player -> Int -> [Maybe Player] -> Bool
checkNLine p n = any (any (== Just p)) . filter (\l -> length l >= n) . group

argmax :: Ord v => (e -> v) -> [e] -> v -> v -> (e,v)
argmax f [e] _ _ = (e, f e)
argmax f (h:t) alpha beta 
    | fh > ft = (h,fh)
    | otherwise = (bt, ft)
    where
        (bt,ft) = argmax f t alpha' beta 
        fh = f h
        alpha' = max alpha fh

argmin :: Ord v => (e -> v) -> [e] -> v -> v -> (e,v)
argmin f [e] _ _ = (e, f e)
argmin f (h:t) alpha beta
    | fh < ft = (h,fh)
    | otherwise = (bt, ft)
    where
        (bt,ft) = argmin f t alpha beta'
        fh = f h
        beta' = min beta fh 

-- Function to retrieve neighboring indices of non-Nothing elements in the matrix
neighboringIndices :: [[Maybe Player]] -> [(Int, Int)]
neighboringIndices matrix = nub [neighbor | neighbor <- nonNothingIndices, (isValidMove matrix neighbor)]
    where
        nonNothingIndices = concat [getNeighbors index | (row, i) <- zip matrix [0..], (elem, j) <- zip row [0..], not (isNothing elem), let index = (j, i)]
        getNeighbors (i, j) = [(i + dx, j + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

