import qualified ContainerTest
import qualified MdEvalTest
import qualified ParserTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  ParserTest.tests
  MdEvalTest.tests
  ContainerTest.tests
