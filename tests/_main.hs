import qualified ParserTest
import qualified ContainerTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  ParserTest.tests
  ContainerTest.tests
