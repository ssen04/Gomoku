module Helper where
-- Import functions and data types from Setup.hs
import Setup
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
    ( Key(MouseButton),
      KeyState(Down),
      MouseButton(LeftButton),
      Event(EventKey) )
import Data.List ( group, tails, transpose )
import Data.Maybe

-- Convert mouse coordinates to cell coordinates
-- (floor ((x + halfWidth) / cellSize), floor ((y + halfHeight) / cellSize)) computes the row and column indices of the NEAREST cell corresponding to the mouse click
--              ADJUSTED x and y coordinates to consider offset from thr center of the game window. Floor function to round to nearest integer (to get cell indices)
--              source: https://stackoverflow.com/questions/71000668/how-to-calculate-the-screen-height-and-position-an-element-below-the-fold
mouseToCellCoords :: (Float, Float) -> (Int, Int)
mouseToCellCoords (x, y) = (floor ((x + halfWidth) / cellSize), floor ((y + halfHeight) / cellSize))
  where
    halfWidth = fromIntegral windowWidth / 2
    halfHeight = fromIntegral windowHeight / 2

-- Check if the board is full
-- (all isJust): inner 'all isJust' checks if all elements in a row are Just values (cells occupied?)
-- all (...): outer all checks if all rows in the board satisfy the condition of having all cells occupied
isDraw :: Board -> Bool
isDraw = all (all isJust)


-- Check if a player has won
-- (rows ++ cols ++ diags): join all rows, columns, and diagonals to a list -> check for winning patterns in all directions on board
-- any (checkFiveLine p): check if there is any winning pattern for the player p
isWinner :: Board -> Player -> Bool
isWinner b p = any (checkFiveLine p) (rows ++ cols ++ diags)
  where
    rows  = b
    cols  = transpose b
    diags = diagonals b

-- source: https://stackoverflow.com/questions/47754747/gomoku-diagonal-winning-condition
-- [Maybe Player]: list representing a line on the game board -> such as a row/column/diagonal
-- group: groups adjacent elements in the line into sublists containing consecutive elements with the same value. For example, [Just X, Just X, Nothing, Just Y, Just Y, Just Y] would be grouped into [[Just X, Just X], [Nothing], [Just Y, Just Y, Just Y]]
-- filter (\l -> length l >= 5): filter out sublists that have a length less than 5 (winning pattern needd >= 5 consecutive pieces)
-- any (all (== Just p)): check if any sublist contains 5 consecutive pieces belonging to player p -> if all elements in the sublist are equal to Just p
checkFiveLine :: Player -> [Maybe Player] -> Bool
checkFiveLine p = any (all (== Just p)) . filter (\l -> length l >= 5) . group

-- Get the diagonal elements of the board
-- source: https://stackoverflow.com/questions/3998891/what-is-the-best-way-to-extract-a-diagonal-from-a-matrix-in-haskell
-- [[maybe players]] represent the diag elems

-- Get the diagonal elements of the board
-- diagonals' b: apply diagonals'to b -> get diagonals from the top-right corner to the bottom-left
-- diagonals' (transpose b): generate the forward diagonals of the transposed board
diagonals :: Board -> [[Maybe Player]]
diagonals b = diagonals' b ++ diagonals' (transpose b)
  where
    -- forwardDiagonals b': generate the forward diagonals of the board b'
    -- map reverse b': reverse each row of the board b'
    diagonals' :: Board -> [[Maybe Player]]
    diagonals' b' = forwardDiagonals b' ++ forwardDiagonals (map reverse b')

    -- [diagonal n b'' | n <- [-(boardSize - 1) .. boardSize - 1]]: generate the forward diagonals by iterating over all possible offsets n ranging from (boardSize - 1) to (boardSize - 1). 
    -- For each offset, it extracts the diagonal with that offset using the diagonal function (start from top left) -> get all forward diags
    -- source: https://stackoverflow.com/questions/6313308/get-all-the-diagonals-in-a-matrix-list-of-lists-in-python
    forwardDiagonals :: Board -> [[Maybe Player]]
    forwardDiagonals b'' = [diagonal n b'' | n <- [-(boardSize - 1) .. boardSize - 1]]
      

    -- n -> offset parameter to determine the diagonal line to extract from the board (+ve n -> top-left to bottom-right, -ve n -> top-right to bottom-left)
    -- row <- [0 .. boardSize - 1]: get row indices of the board, from 0 to boardSize - 1.
    -- let col = row + n: for each row index, calculate the column index col for the diagonal on the offset n
    -- col >= 0 && col < boardSize: calculate col index is within the range of column indices
    -- b'' !! row !! col: get the element at the position (row, col) from the board b''(get diagonal element along curr diag)
  
    diagonal :: Int -> Board -> [Maybe Player]
    diagonal n b'' = [b'' !! row !! col | row <- [0 .. boardSize - 1], let col = row + n, col >= 0 && col < boardSize]

    -- why use b''? b'' is a parameter to forwardDiagonals to represent the board from which forward diagonals are to be extracted, allowing the function to work correctly with transposed boards
