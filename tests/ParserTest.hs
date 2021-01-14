{-# LANGUAGE QuasiQuotes #-}

module ParserTest where

import CMark
import Evaluator
import Parser
import Test.Hspec
import Text.RawString.QQ

content =
  [r|
# Helo world
params

```js
console.log("Hello world")
```
|]

tests = describe "Stuff" $ do
  runIO $ do
    result <- evaluate content
    print result
  it "should do stuff" $ do
    1 `shouldBe` 0
