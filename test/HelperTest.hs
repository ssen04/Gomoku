import Test.HUnit
import Control.Monad
import Helper
import Setup
-- Define the HUnit test case
testMouseToCellCoords :: Test
testMouseToCellCoords = "Test mouseToCellCoords with default values" ~:
    do
        let (x, y) = mouseToCellCoords (100, 100)  -- Arbitrary mouse coordinates
        assertBool "is X coordinate within boardSize? " (x <= boardSize - 1 && x>=0)
        assertBool "is Y coordinate is within boardSize? " (y <= boardSize - 1 && y>=0)
    
allHelperTests = TestList [TestLabel "testMouseToCellCoords" testMouseToCellCoords]

-- Run the test(s)
main :: IO Counts
main = runTestTT testMouseToCellCoords
