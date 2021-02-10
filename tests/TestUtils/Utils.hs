{-# LANGUAGE LambdaCase #-}

module TestUtils.Utils where

import CMark
import CodeBlocks
import CodeExecutor
import qualified Data.Text as Text
import Parser

emptyNode = Node Nothing (TEXT . Text.pack $ "") []

-- Replaces irrelevant info from nodes for cleaner assertions
cleanMdTree = map $ \case
  MdRenderNode _ children -> MdRenderNode emptyNode $ cleanMdTree children
  MdCodeBlock hints code -> MdCodeBlock hints code

-- Replaces code that was executed from the result block
cleanEvalResult = map $ \case
  RunBlockResult name _ (code, stdout, _) -> RunBlockResult name (Bash "") (code, stdout, "")
  ModuleBlockResult name _ exports -> ModuleBlockResult name (Bash "") exports
