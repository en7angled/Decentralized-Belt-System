import BJJPropertyTests
-- import ValidatorPropertyTests
import Test.Tasty (defaultMain, testGroup)
import UnitTests (unitTests)

runTest :: IO ()
runTest =
  defaultMain (testGroup "Tests" [unitTests, bjjPropertyTests])

main :: IO ()
main = do
  runTest
