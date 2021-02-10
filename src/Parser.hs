{-# LANGUAGE LambdaCase #-}

module Parser where

import CMark
import CodeExecutor
import Data.List
import qualified Data.Text as Text
import Debug.Trace
import Text.Parsec
import Utils
import Prelude

data HintExpression = HintExpr [HintExpression] | HintLabel String
  deriving (Eq, Show)

data Hint = HModule String | HDependencies [(String, String)] | HNoop deriving (Show, Eq)

-- TODO: Actual meta data please
data MarkdownNode
  = CodeBlock [Hint] Code
  | RenderNode Node [MarkdownNode]
  deriving (Show, Eq)

cmarkOptions = [optUnsafe, optNormalize]

parseMarkdown = wrapNodes . (: []) . commonmarkToNode cmarkOptions . Text.pack

valueP :: Parsec String u HintExpression
valueP = HintLabel <$> many1 (alphaNum <|> oneOf allowedChars)
  where
    allowedChars = ['+', '-', '*', '/', '<', '>', '=', '!', '%', '&', '.', '?', '@', '$', '_', '^', '~']

hintExpressionP :: Parsec String u HintExpression
hintExpressionP = withWhitespace $ do
  char '('
  whitespace
  expr <- (valueP <|> hintExpressionP) `sepBy` whitespace
  whitespace
  char ')'
  return $ HintExpr expr

hintHtmlCommentParser :: Parsec String u [HintExpression]
hintHtmlCommentParser = do
  string "<!--@"
  res <- hintExpressionP `sepBy` whitespace
  try $ string "-->"
  return res

isHintComment :: String -> Bool
isHintComment = isPrefixOf "<!--@"

tokenizeHints :: String -> Either ParseError [HintExpression]
tokenizeHints = parse hintHtmlCommentParser "ParserError"

parseHints :: String -> Either ParseError [Hint]
parseHints = fmap toHintConfig . tokenizeHints
  where
    toHintConfig [] = []
    toHintConfig (head : tl) = case head of
      HintLabel str -> toHintConfig tl -- Ignore
      HintExpr (HintLabel "module" : HintLabel moduleName : _) -> HModule moduleName : toHintConfig tl
      HintExpr (HintLabel "noop" : _) -> HNoop : toHintConfig tl
      HintExpr (HintLabel "dependencies" : deps) -> HDependencies (toDepsList deps) : toHintConfig tl
        where
          toDepsList :: [HintExpression] -> [(String, String)]
          toDepsList = map $ \case
            HintLabel dep -> (dep, "*")
            HintExpr (HintLabel dep : HintLabel version : _) -> (dep, version)
            HintExpr (HintLabel dep : _) -> (dep, "*")

wrapNodes :: [Node] -> [MarkdownNode]
wrapNodes = \case
  [] -> []
  ((Node _ (HTML_BLOCK str) _) : (Node _ (CODE_BLOCK lang code) _) : rest) -> newNodes ++ wrapNodes rest
    where
      newNodes = [CodeBlock hints $ toCode (Text.unpack lang) (Text.unpack code)]
      hints = getEitherWithDef [] $ parseHints (Text.unpack str)
  ((Node _ (CODE_BLOCK lang code) _) : rest) -> newNodes ++ wrapNodes rest
    where
      newNodes = [CodeBlock [] $ toCode (Text.unpack lang) (Text.unpack code)]
  ((Node _ nodeType nodes) : rest) -> newNodes ++ wrapNodes rest
    where
      newNodes = [RenderNode (Node Nothing nodeType nodes) $ wrapNodes nodes]

--
--
--
--
