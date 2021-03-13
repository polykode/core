import qualified CodeBlocksTest
import qualified ContainerTest
import qualified ParserTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  ParserTest.tests
  ContainerTest.tests
  CodeBlocksTest.tests
