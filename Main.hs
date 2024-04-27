module Main where

import MainGame
import Setup
import Helper
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.IO
import System.IO.Error
import Control.Exception

-- Load a BMP image
-- source: https://stackoverflow.com/questions/41390771/haskell-gloss-loading-bmp
loadBMPImage :: FilePath -> IO Picture
loadBMPImage path = loadBMP path

-- Draw function with background image
-- pictures []: make a composite picture by combining the background image (bmp) and other elements (otherElements) into a single picture
-- otherElements = MainGame.draw gameState: draw all the game elements - board and player pieces on top of the background image
drawAll :: Picture -> GameState -> IO Picture
drawAll backgroundImage gameState = do
    return $ pictures [backgroundImage, otherElements]
    where
        otherElements = MainGame.draw gameState

-- Start a new game
startNewGame :: IO ()
startNewGame = do
  putStrLn("Starting a new game...\n")
  putStrLn("What game mode what you like to play? \n [1] Player vs Player \n [2] Player vs AI\n")
  input <- getLine
  mode <- do
    case input of
      "1" -> return False
      "2" -> return True
  putStrLn("Who is playing first? \n [1] Player X (1st player) \n [2] Player Y (2nd player / AI)\n")
  input <- getLine
  player <- do
    case input of
      "1" -> return X
      "2" -> return Y
  let initialState = if mode == True && player == Y then aiPlayer (MainGame.initialState mode player) else (MainGame.initialState mode player)
  backgroundImage <- loadBMPImage "Background.bmp"
  let fps = 50
  playIO window backgroundColor fps initialState (drawAll backgroundImage) handleEventIO updateIO

-- Load the saved game
loadTheGame :: GameState -> IO ()
loadTheGame game = do
  putStrLn("Loading the game...")
  let boardSize = length (board game)
  backgroundImage <- loadBMPImage "Background.bmp"
  let fps = 50
  playIO window backgroundColor fps game (drawAll backgroundImage) handleEventIO updateIO

-- Main function
main :: IO ()
main = do
  loaded <- MainGame.loadGameState -- Call loadGameState function in MainGame.hs
  case loaded of
    Just game -> do
      putStrLn("Saved game found. Do you want to load it? (y / n)\n")
      decision <- getLine
      case decision of
        "y" -> do
          loadTheGame game
        "n" -> startNewGame
        _   -> putStrLn("Invalid input.")
    Nothing -> do
      putStrLn("Saved game not found.\n")
      startNewGame
  where
    -- calculate the size of window
    sizeofBoard = cellSize * fromIntegral boardSize
    windowWidth = round (sizeofBoard)
    windowHeight = round (sizeofBoard)
    -- Inwindow with window name, size and position on screen
    window = InWindow "Gomoku Project CPSC 312" (windowWidth, windowHeight) (100, 100)
    backgroundColor = white
