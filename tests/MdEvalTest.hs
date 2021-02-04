{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module MdEvalTest where

import CMark
import Container.Eff
import Container.Pool
import Control.Algebra
import qualified Data.Text as Text
import EffectMock
import GHC.IO.Exception
import MdEval
import Test.Hspec
import Text.RawString.QQ

content =
  [r|
# Hello
params

```js
console.log("Hello world", { a: 'b' })
```
|]

emptyNode = Node Nothing (TEXT $ Text.pack "") []

withEmptyRender = \case
  RenderNode _ rns -> RenderNode emptyNode (map withEmptyRender rns)
  EvalNode _ result -> EvalNode emptyNode result

evalMd :: String -> IO (Either Error [ResultNode])
evalMd = fmap (fmap (map withEmptyRender)) . runMock . evaluate (Container "hello")

tests = describe "MdEval" $ do
  it "should evaluate sample markdown and execute content" $ do
    evalMd content
      `shouldReturn` Right
        [ RenderNode
            emptyNode
            [ RenderNode emptyNode [RenderNode emptyNode []],
              RenderNode emptyNode [RenderNode emptyNode []],
              EvalNode
                emptyNode
                (ExitSuccess, "Executing (hello): bash -c console.log(\"Hello world\", { a: 'b' })\n", "")
            ]
        ]
