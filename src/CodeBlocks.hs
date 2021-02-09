{-# LANGUAGE OverloadedStrings #-}

module CodeBlocks where

import CodeExecutor
import Container.Eff
import Control.Algebra
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Parser)
import qualified Data.Text as Text
import Server.JsonResponse

type ModuleExports = [String] -- NOTE: Will containe more information about exported modules/data

data CodeBlock = RunBlock String Code | ModuleBlock String Code
  deriving (Show, Eq)

instance Json.FromJSON CodeBlock where
  parseJSON (Json.Object obj) = do
    ctype <- obj .: "type" :: Parser String
    name <- obj .: "name"
    lang <- obj .: "lang"
    code <- obj .: "code"
    return $ case ctype of
      "run" -> RunBlock name (toCode lang code)
      "module" -> ModuleBlock name (toCode lang code)

data CodeBlockResult = RunBlockResult String Code Result | ModuleBlockResult String Code ModuleExports
  deriving (Show, Eq)

instance Json.ToJSON CodeBlockResult where
  toJSON (RunBlockResult name code (exitCode, stdout, stderr)) =
    Json.object
      [ (Text.pack "type", toJsonString "run"),
        (Text.pack "name", toJsonString name),
        (Text.pack "code", toJsonString . show $ code),
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
evaluateBlocks execId container [] = return []
evaluateBlocks execId container (h : tl) = do
  blockResult <- case h of
    RunBlock name code -> RunBlockResult name code <$> executeCode execId container code
    ModuleBlock name code -> return $ ModuleBlockResult name code [] -- TODO: Add module exports parsing
  rest <- evaluateBlocks execId container tl
  return $ blockResult : rest
