module Setup where

-- Using Gloss for project GUI
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
    ( Key(MouseButton),
      KeyState(Down),
      MouseButton(LeftButton),
      Event(EventKey) )
import Data.List
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import Text.Read(readMaybe)

-- Defining data type Player (derive Eq and Show for comparison and display for debugging purposes)
--                            added: derive Read to read upon file load
-- source type: https://www1.cs.columbia.edu/~sedwards/classes/2019/4995-fall/reports/gomoku.pdf
data Player = X | Y deriving (Eq, Show, Read)

-- Outer List: Row; Inner List: Each box/cell on board
-- Board can be 'Empty' or cells can be occupied by player(s)
-- source: https://github.com/mkuzmik/gomoku-game/blob/master/Gomoku.hs
type Board = [[Maybe Player]]

-- Define size of board (!MAKE THIS A VARIABLE IN THE FUTURE!)
-- inputBoardSize an integer from the user to use as the board size
readIntegerinputBoardSizeBoardSize :: String -> IO Int
readIntegerinputBoardSizeBoardSize displayPrompt = do
    putStrLn displayPrompt
    inputBoardSize <- getLine
    case readMaybe inputBoardSize of
            Just size -> return size -- pattern matches when the result of readMaybe -> inputBoardSize is Just size meaning user inputBoardSize was valid
            Nothing -> do
                putStrLn "You have entered an invalid board size. Using default board size for this game: 15"
                return 15

-- Check if inputBoardSize is a valid integer
isValidinputBoardSize :: Int -> Bool
isValidinputBoardSize size = size >= 5 -- Gomoku board cannot be less than 5 -> cannot get a winning pattern

-- Read board size from user inputBoardSize
-- source: https://downloads.haskell.org/ghc/9.8.1/docs/users_guide/exts/pragmas.html#:~:text=6.20.6.3.%20NOINLINE%20pragma-,%C2%B6,-%7B%2D%23%20NOINLINE%20%E2%9F%A8name%E2%9F%A9%20%23%2D%7D
{-# NOINLINE boardSize #-} -- compiler will not optimize boardSize var, boardSize -> seperate variable that will be evaluated independently. Ensure that var is evaluated ONCE (value depends on IO action)
boardSize :: Int
boardSize = unsafePerformIO $ do
  inputBoardSize <- readIntegerinputBoardSizeBoardSize "Enter board size: "
  if isValidinputBoardSize inputBoardSize -- check if board size if valid, i.e., >= 5
      then return inputBoardSize
      else do
        putStrLn "Gomoku board size cannot be less than 5x5, using default board size of 15!"
        return 15

{-# NOINLINE gameMode #-} -- compiler will not optimize gameMode var, boardSize -> seperate variable that will be evaluated independently. Ensure that var is evaluated ONCE (value depends on IO action)
gameMode :: Bool
gameMode = unsafePerformIO $ do
  putStrLn("What game mode what you like to play? \n [1] Player vs Player \n [2] Player vs AI\n")
  input <- getLine
  case input of
    "1" -> return False
    "2" -> return True
    _   -> do
      putStrLn "Invalid input, playing AI by default.\n"
      return True

-- Defines cell size in pixels
cellSize :: Float
cellSize = 30

-- Define the starting window size
windowWidth, windowHeight :: Int
windowWidth = round (cellSize * fromIntegral boardSize)
windowHeight = round (cellSize * fromIntegral boardSize)

-- Initialize the window variable
-- InWindow is a constructor
-- Title: Gomoku Project CPSC 312
-- Size: previously defined
-- (100, 100) is the position of the window on the screen
window :: Display
window = InWindow "Gomoku Project CPSC 312" (windowWidth, windowHeight) (100, 100)

-- Specify the backgroundColor color (!CAN MAKE THIS A VARIABLE - INTO A DROPDOWN!)
backgroundColor :: Color
backgroundColor = white

-- Define empty board of type Board (defined earlier)
-- replicate boardsize (...) - create a list of lengrh boardsize, say list a
-- each element in lisr a is a list of size board size - each containing the value nothing at the start (not occupied by any player)
emptyBoard :: Board
emptyBoard = replicate boardSize (replicate boardSize Nothing)

-- Converts the board into an array of indices 
-- This function is needed to evaluate the board 
boardPositions :: [[(Int, Int)]]
boardPositions = [[(i, j) | (j, _) <- zip [0..] row] | (i, row) <- zip [0..] emptyBoard]

-- Decide player Color
playerColor :: Player -> Color
playerColor X = blue
playerColor Y = green

-- halfWidth = fromIntegral windowWidth / 2 -- half width is the half of the window width
-- halfHeight = fromIntegral windowHeight / 2 -- half height is half of the window height
halfWidth = fromIntegral (windowWidth - round cellSize) / 2
halfHeight = fromIntegral (windowHeight - round cellSize) / 2

-- Prepare cell coordinates
xcoord x = fromIntegral x * cellSize - halfWidth
ycoord y = fromIntegral y * cellSize - halfHeight

-- Draw the Gomoku playing board
drawGomokuBoard :: Board -> Picture
-- pictures: onstructs a list of pictures, one for each cell in the Gomoku board
--  drawEachCell (x,y) cell : draws a cell on the Gomoku player bpard at its x and y coordinates and the player occupying it, if any
-- value of x and y coordinates ranges from 0 to (boardsize-1) since it is a square board
-- let cell = b !! y !! x: retrieves the player on the board b (inputBoardSize) at coordinates (x,y) and store it in the value cell
--            Access the cell by retrieving the row 'y' and then getting the particular cell by pointing out column 'x' in row y
drawGomokuBoard b = pictures [ drawEachCell (x, y) cell | x <- [0..boardSize-1], y <- [0..boardSize-1], let cell = b !! y !! x]
  where
    drawEachCell :: (Int, Int) -> Maybe Player -> Picture
    -- Use of pattern matching exmaple
    -- x * cellsize to calculate the position of the x coordinate on the window
    -- subtract halfwidhth to position the cell relative to the center of the window (otherwise board was showing up on the top right corner)
    -- color cellColor specifies the color fo the cell
    -- rectangleWire - finction provided by Gomoku: draws the orders of an empty cell to represent a blank cell equal to the size of the cell
    drawEachCell (x, y) Nothing = translate (xcoord x) (ycoord y) $ color cellColor $ rectangleWire cellSize cellSize
    -- color xColor/yColor are the colors specified to x/y players
    -- Draws a circle of x/ycolor in the cell occupoed by player x/y where radius of circle is cellSize/3
    -- Just X/Y pattern matches when the 'Maybe Player' is just X/Y
    drawEachCell (x, y) (Just X) = translate (xcoord x) (ycoord y) $ color xColor $ circleSolid (cellSize / 3)
    drawEachCell (x, y) (Just Y) = translate (xcoord x) (ycoord y) $ color ycolor $ circleSolid (cellSize / 3)
    cellColor = makeColor 0 0 0 1 -- Black for the rectangular wire
    xColor = makeColor 0 0 1 1 -- Blue for the blue circle assigned to playerX
    ycolor = makeColor 0 1 0 1 -- Green for the green  circle assigned to playerO
