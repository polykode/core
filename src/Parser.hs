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

data CodeBlockType = RunBlock String | ModuleBlock String | Noop
  deriving (Show, Eq)

data HintExpression = HintExpr [HintExpression] | HintLabel String
  deriving (Show, Eq)

data Hints = Hints
  { hType :: CodeBlockType,
    hDependencies :: [(String, String)]
  }
  deriving (Show, Eq)

defaultHints = Hints {hType = RunBlock "", hDependencies = []}

-- TODO: Actual meta data please
data MarkdownNode
  = MdCodeBlock Hints Code
  | MdRenderNode Node [MarkdownNode]
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

parseHints :: String -> Either ParseError Hints
parseHints = fmap (toHintConfig $ Hints {hType = RunBlock "", hDependencies = []}) . tokenizeHints
  where
    toHintConfig h [] = h
    toHintConfig hints (head : tl) = case head of
      HintLabel str -> toHintConfig hints tl -- Ignore
      HintExpr (HintLabel "module" : HintLabel moduleName : _) ->
        toHintConfig (hints {hType = ModuleBlock moduleName}) tl
      HintExpr (HintLabel "noop" : _) ->
        toHintConfig (hints {hType = Noop}) tl
      HintExpr (HintLabel "dependencies" : deps) ->
        toHintConfig (hints {hDependencies = toDepsList deps}) tl
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
      newNodes = [MdCodeBlock hints $ toCode (Text.unpack lang) (Text.unpack code)]
      hints = getEitherWithDef defaultHints $ parseHints (Text.unpack str)
  ((Node _ (CODE_BLOCK lang code) _) : rest) -> newNodes ++ wrapNodes rest
    where
      newNodes = [MdCodeBlock defaultHints $ toCode (Text.unpack lang) (Text.unpack code)]
  ((Node _ nodeType nodes) : rest) -> newNodes ++ wrapNodes rest
    where
      newNodes = [MdRenderNode (Node Nothing nodeType nodes) $ wrapNodes nodes]

--
--
--
--
