import Test.HUnit
import Control.Monad
import Helper
import Setup
import MainGame

-- Define the HUnit test case for isValidMove from mainGame
testIsValidMoveValidCoords :: Test
testIsValidMoveValidCoords = 
    "Test isValidMove with valid coordinates" ~: do
    let validCoords = (1, 1)  -- coordinates are valid
    let result = isValidMove emptyBoard validCoords
    assertBool "Move should be valid for valid coordinates" result

testIsValidMoveInvalidCoordsNeg :: Test
testIsValidMoveInvalidCoordsNeg = 
    "Test isValidMove with valid coordinates" ~: do
    let validCoords = (-10, -10)  -- coordinates are invalid (-ve)
    let result = isValidMove emptyBoard validCoords
    assertBool "Move should be valid for valid coordinates" result

testIsValidMoveInvalidCoordsBeyondBoardSize :: Test
testIsValidMoveInvalidCoordsBeyondBoardSize = 
    "Test isValidMove with valid coordinates" ~: do
    let validCoords = (boardSize+1, boardSize+1)  -- coordinates are invalid (oob +ve)
    let result = isValidMove emptyBoard validCoords
    assertBool "Move should be valid for valid coordinates" result

-- Tests for AI 

testAIWinsFourInARow :: Test
testAIWinsFourInARow = 
    "Test AI takes win when there is four in a row" ~: do
    let board = [[Just Y, Nothing, Nothing, Nothing, Nothing], [Just Y, Nothing, Nothing, Nothing, Nothing], [Just Y, Nothing, Nothing, Nothing, Nothing], [Just Y, Nothing, Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing, Nothing, Nothing]] 
    let gameState = GameState board Y InProgress (neighboringIndices board) True 
    let result = (fst (minimax gameState 1))
    assertBool "Move should match coordinates:" (result == (0,4))

testAIBlocksFourInARow :: Test
testAIBlocksFourInARow = 
    "Test AI takes win when there is four in a row" ~: do
    let board = [[Just X, Nothing, Nothing, Nothing, Nothing], [Just X, Nothing, Nothing, Nothing, Nothing], [Just X, Nothing, Nothing, Nothing, Nothing], [Just X, Nothing, Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing, Nothing, Nothing]] 
    let gameState = GameState board Y InProgress (neighboringIndices board) True 
    let result = (fst (minimax gameState 1))
    assertBool "Move should match coordinates:" (result == (0,4))

-- comb all the tests into a single list
allTests :: Test
allTests = TestList [ TestLabel "testIsValidMoveValidCoords" testIsValidMoveValidCoords, 
                        TestLabel "testIsValidMoveInvalidCoordsNeg" testIsValidMoveInvalidCoordsNeg,
                        TestLabel "testIsValidMoveInvalidCoordsBeyondBoardSize" testIsValidMoveInvalidCoordsBeyondBoardSize,
                        TestLabel "testAIWinsFourInARow" testAIWinsFourInARow
                    ]

-- Run all the tests
main :: IO Counts
main = runTestTT allTests
