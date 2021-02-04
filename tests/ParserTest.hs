{-# LANGUAGE QuasiQuotes #-}

module ParserTest where

import CMark
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
  it "should do stuff" $ do
    1 `shouldBe` 1
