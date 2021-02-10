{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserTest where

import CMark
import CodeExecutor
import qualified Data.Text as Text
import Parser
import Test.Hspec
import Test.Hspec.Golden
import Text.RawString.QQ

emptyNode = Node Nothing (TEXT . Text.pack $ "") []

cleanMdTree = map $ \case
  RenderNode _ children -> RenderNode emptyNode $ cleanMdTree children
  CodeBlock hints code -> CodeBlock hints code

simpleMd =
  [r|
# Helo world
params

```js
console.log("Hello world")
```

**HEllo**

```bash
echo 1
```
|]

withModulesMd =
  [r|
# Helo world

<!--@
(module foobar)
(dependencies
  (npm.ramda)
  (npm.@algebraic-effects/core 5.0.0)
  (python.something)
)
-->
```js
console.log("Hello world")
```

<!--@ (noop) -->
```bash
console.log("Hello world")
```
|]

tests = describe "Parser" $ do
  describe "parseHints" $ do
    it "should parse noop hint" $ do
      parseHints "<!--@ (noop) -->" `shouldBe` Right [HNoop]
    it "should parse module name hint" $ do
      parseHints "<!--@ (module foobar) -->" `shouldBe` Right [HModule "foobar"]
    it "should parse module name with dependency list" $ do
      parseHints
        [r|<!--@
        (module foobar)
        (dependencies
          npm.lodash
          (npm.test *)
          (python.some-dependency)
          (python.random >=2.0.0)
          (crates.xyz-package ~2.0.0)
          (npm.@scope/package ^5.0.0)
        )
      -->|]
        `shouldBe` Right
          [ HModule "foobar",
            HDependencies
              [ ("npm.lodash", "*"),
                ("npm.test", "*"),
                ("python.some-dependency", "*"),
                ("python.random", ">=2.0.0"),
                ("crates.xyz-package", "~2.0.0"),
                ("npm.@scope/package", "^5.0.0")
              ]
          ]
  describe "parseMarkdown" $ do
    it "(snapshot) should parse markdown for simple codeblocks" $ do
      defaultGolden "Parser.parseMarkdown.simple" . show . parseMarkdown $ simpleMd
    it "(snapshot) should parse markdown with annotated codeblocks" $ do
      defaultGolden "Parser.parseMarkdown.with-hints" . show . parseMarkdown $ withModulesMd
    it "should parse markdown with annotated codeblocks" $ do
      cleanMdTree
        ( parseMarkdown
            [r|
<!--@
(module foobar)
(dependencies (npm.rambda 1.0.0))
-->
```js
console.log(1)
```
        |]
        )
        `shouldBe` [ RenderNode
                       (Node Nothing (TEXT . Text.pack $ "") [])
                       [ CodeBlock
                           [ HModule "foobar",
                             HDependencies
                               [ ("npm.rambda", "1.0.0")
                               ]
                           ]
                           (NodeJs "console.log(1)\n")
                       ]
                   ]
