import qualified ParserTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  ParserTest.tests
