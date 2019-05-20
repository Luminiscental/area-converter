
import           Test.HUnit
import           Lib

flipTest = TestCase
    (do
        let oldFullArea     = Area (Pair 0 0) (Pair 100 100)
            oldArea         = Area (Pair 10 10) (Pair 80 80)
            newFullArea     = Area (Pair 100 100) (Pair (-100) (-100))
            expectedNewArea = Area (Pair 90 90) (Pair (-80) (-80))
            returnedNewArea = convertArea oldFullArea newFullArea oldArea
        assertEqual "convertArea works in a specific case"
                    expectedNewArea
                    returnedNewArea
    )

-- TODO: Why does this fail?
inlineTest = TestCase
    (do
        let oldFullArea     = Area (Pair 2 3) (Pair 110 (-100))
            oldArea         = Area (Pair 6 (-1)) (Pair 50 (-20))
            newFullArea     = Area (Pair 0 1) (Pair 50 60)
            expectedNewArea = Area
                (Pair (0 + (6 - 2) * 50 / 110) (1 + ((-1) - 3) * (-20) / (-100))
                )
                (Pair (50 * 50 / 110) ((-20) * 60 / (-100)))
            returnedNewArea = convertArea oldFullArea newFullArea oldArea
        assertEqual "convertArea equals the inline formula"
                    expectedNewArea
                    returnedNewArea
    )

tests =
    TestList [TestLabel "flipTest" flipTest, TestLabel "inlineTest" inlineTest]

main :: IO ()
main = do
    runTestTT tests
    return ()
