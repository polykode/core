{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserTest where

import CMark
import qualified Data.Text as Text
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
```bash
echo 1
```
|]

emptyNode = Node Nothing (TEXT $ Text.pack "") []

cleanTree = \case
  MetaData _ -> MetaData ""
  RawNode _ nodes -> RawNode emptyNode . map cleanTree $ nodes
  Code lang code node -> Code lang code emptyNode

tests = describe "Parser" $ do
  it "should parse markdown for codeblocks" $ do
    map cleanTree (parse content)
      `shouldBe` [ RawNode
                     emptyNode
                     [ RawNode emptyNode [RawNode emptyNode []],
                       RawNode emptyNode [RawNode emptyNode []],
                       Code "js" "console.log(\"Hello world\")\n" emptyNode,
                       Code "bash" "echo 1\n" emptyNode
                     ]
                 ]
  it "should parse codeblock decorators" $ do
    map cleanTree (parse content)
      `shouldBe` [ RawNode
                     emptyNode
                     [ RawNode emptyNode [RawNode emptyNode []],
                       RawNode emptyNode [RawNode emptyNode []],
                       Code "js" "console.log(\"Hello world\")\n" emptyNode,
                       Code "bash" "echo 1\n" emptyNode
                     ]
                 ]
