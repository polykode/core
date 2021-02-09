{-# LANGUAGE LambdaCase #-}

module CodeBlocksTest where

import CodeBlocks
import CodeExecutor
import Container.Eff
import EffectMock
import GHC.IO.Exception
import Test.Hspec
import Test.Hspec.Golden

eval :: [CodeBlock] -> IO (Either Error [CodeBlockResult])
eval = runMock . evaluateBlocks "test" (Container "hello")

cleanEvalResult = map $ \case
  RunBlockResult name _ (code, stdout, _) -> RunBlockResult name (Bash "") (code, stdout, "")
  ModuleBlockResult name _ exports -> ModuleBlockResult name (Bash "") exports

tests = describe "CodeBlocks" $ do
  describe "evaluateBlocks" $ do
    describe "run" $ do
      result <-
        runIO $
          eval
            [ RunBlock "1" $ Bash "echo foobar",
              RunBlock "2" $ NodeJs "console.log(1)"
            ]
      it "should execute the code in the right container" $ do
        fmap cleanEvalResult result
          `shouldBe` Right
            [ RunBlockResult "1" (Bash "") (ExitSuccess, "Executing (hello)", ""),
              RunBlockResult "2" (Bash "") (ExitSuccess, "Executing (hello)", "")
            ]
      it "(snapshot) should resolve with the correct results" $ do
        defaultGolden "CodeBlocks.evaluateBlocks.run" $ show result
