{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeBlocks where

import CodeExecutor
import Container.Eff
import Control.Algebra
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Parser)
import qualified Data.Text as Text
import Parser
import Server.JsonResponse

type ModuleExports = [String] -- NOTE: Will containe more information about exported modules/data

data CodeBlock = CodeBlock Hints Code
  deriving (Show, Eq)

instance Json.FromJSON CodeBlock where
  parseJSON = Json.withObject "CodeBlock" $ \obj -> do
    name <- obj .: "name"
    lang <- obj .: "lang"
    code <- obj .: "code"
    -- TODO: Parse annotations (`annotations`)
    return $ CodeBlock (defaultHints {hType = RunBlock name}) $ toCode lang code

data CodeBlockResult = RunBlockResult String Code Result | ModuleBlockResult String Code ModuleExports
  deriving (Show, Eq)

instance Json.ToJSON CodeBlockResult where
  toJSON (RunBlockResult name code (exitCode, stdout, stderr)) =
    Json.object
      [ (Text.pack "type", toJsonString "run"),
        (Text.pack "name", toJsonString name),
        (Text.pack "lang", toJsonString . toLangName $ code),
        ( Text.pack "result",
          Json.object
            [ (Text.pack "exitCode", toJsonString . show $ exitCode),
              (Text.pack "stdout", toJsonString stdout),
              (Text.pack "stderr", toJsonString stderr)
            ]
        )
      ]
  toJSON (ModuleBlockResult name code exports) =
    Json.object
      [ (Text.pack "type", toJsonString "module"),
        (Text.pack "name", toJsonString name),
        (Text.pack "code", toJsonString . show $ code),
        (Text.pack "exports", Json.toJSONList exports)
      ]

-- Evaluate list of code blocks
evaluateBlocks :: Has LxcIOErr sig m => String -> Container -> [CodeBlock] -> m [CodeBlockResult]
evaluateBlocks _ _ [] = return []
evaluateBlocks execId container (h : tl) = do
  blockResult <- case h of
    CodeBlock (Hints (RunBlock name) _) code ->
      (: []) . RunBlockResult name code <$> executeCode execId container code
    CodeBlock (Hints (ModuleBlock name) _) code ->
      return [ModuleBlockResult name code []] -- TODO: Add module exports parsing
    _ -> return []
  rest <- evaluateBlocks execId container tl
  return $ blockResult ++ rest

mdToCodeBlocks :: [MarkdownNode] -> [CodeBlock]
mdToCodeBlocks [] = []
mdToCodeBlocks (MdRenderNode _ children : tl) = mdToCodeBlocks children ++ mdToCodeBlocks tl
mdToCodeBlocks (MdCodeBlock hints code : tl) = newNodes ++ mdToCodeBlocks tl
  where
    newNodes = [CodeBlock hints code | hType hints /= Noop]
