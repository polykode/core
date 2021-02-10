{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module CodeBlocksTest where

import CodeBlocks
import CodeExecutor
import Container.Eff
import GHC.IO.Exception
import Parser
import Test.Hspec
import Test.Hspec.Golden
import TestUtils.EffectMock
import TestUtils.Utils
import Text.RawString.QQ (r)

eval :: [CodeBlock] -> IO (Either Error [CodeBlockResult])
eval = runMock . evaluateBlocks "test" (Container "hello")

testMdContent =
  [r|
# Hello world
Wow `nice` tits

<!--@ (module random) -->
```js
console.log(1);
```

<!--@ (noop) -->
```bash
echo 1;
```

<!--@ (dependencies (npm.ramda 0.0.1)) -->
```js
alert(1)
```
|]

tests = describe "CodeBlocks" $ do
  describe "evaluateBlocks" $ do
    describe "run" $ do
      result <-
        runIO $
          eval
            [ CodeBlock (defaultHints {hType = RunBlock "1"}) $ Bash "echo foobar",
              CodeBlock (defaultHints {hType = RunBlock "2"}) $ NodeJs "console.log(1)"
            ]
      it "should execute the code in the right container" $ do
        fmap cleanEvalResult result
          `shouldBe` Right
            [ RunBlockResult "1" (Bash "") (ExitSuccess, "Executing (hello)", ""),
              RunBlockResult "2" (Bash "") (ExitSuccess, "Executing (hello)", "")
            ]
      it "(snapshot) should resolve with the correct results" $ do
        defaultGolden "CodeBlocks.evaluateBlocks.run" $ show result
  describe "mdToCodeBlocks" $ do
    it "should extract code blocks with correct properties from md" $ do
      mdToCodeBlocks (parseMarkdown testMdContent)
        `shouldBe` [ CodeBlock (defaultHints {hType = ModuleBlock "random"}) (NodeJs "console.log(1);\n"),
                     CodeBlock (defaultHints {hType = Noop}) (Bash "echo 1;\n"),
                     CodeBlock
                       ( defaultHints
                           { hType = RunBlock "",
                             hDependencies = [("npm.ramda", "0.0.1")]
                           }
                       )
                       (NodeJs "alert(1)\n")
                   ]

--
--
