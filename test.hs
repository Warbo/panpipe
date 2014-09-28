import Test.Framework (Test)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding ((==>))

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = []
