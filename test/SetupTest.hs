import Test.HUnit
import Control.Monad
import Setup

-- TESTS FOR isValidinputBoardSize
-- Test case for a valid board size when board size = 10 > 5
testIsValidBoardSize :: Test
testIsValidBoardSize = TestCase $ do
    let size = 10
    assertBool "Board size: 10 is valid?" (isValidinputBoardSize size)

-- Test case for a valid board size when board size = 10 < 5
testIsNotValidBoardSize :: Test
testIsNotValidBoardSize = TestCase $ do
    let size = 3
    assertBool "Board size: 3 is valid?" (isValidinputBoardSize size)

-- Test case for a valid board size when board size = 0
testIsNotValidBoardSizeZero :: Test
testIsNotValidBoardSizeZero = TestCase $ do
    let size = 0
    assertBool "Board size: 0 is valid?" (isValidinputBoardSize size)

-- Test case for a valid board size when board size < 0
testIsNotValidBoardSizeNeg :: Test
testIsNotValidBoardSizeNeg = TestCase $ do
    let size = -1
    assertBool "Board size: -1 is valid?" (isValidinputBoardSize size)

allSetupTests :: Test
allSetupTests = TestList [TestLabel "testIsValidBoardSize" testIsValidBoardSize
                 ,TestLabel "testIsNotValidBoardSize" testIsNotValidBoardSize, 
                 TestLabel "testIsNotValidBoardSizeZero" testIsNotValidBoardSizeZero, 
                 TestLabel "testIsNotValidBoardSizeNeg" testIsNotValidBoardSizeNeg]

-- Run the tests
main :: IO ()
main = void $ runTestTT allSetupTests
