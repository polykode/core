{-# LANGUAGE LambdaCase #-}

module MdEval where

import CMark
import CodeExecutor
import Container.Eff
import Control.Algebra
import qualified Data.Text as T
import Parser

type EvaluationResult = Result

data ResultNode
  = RenderNode Node [ResultNode]
  | EvalNode Node EvaluationResult
  deriving (Eq)

instance Show ResultNode where
  show (RenderNode (Node _ nodeType cNodes) cResults) =
    show nodeType ++ ": [ " ++ show cResults ++ "] " -- ++ "(" ++ show rest ++ ")"
  show (EvalNode node result) = show node ++ "\n\n--- Result ---\n" ++ show result

evaluateMd :: Has LxcIOErr sig m => String -> Container -> [XMDNode] -> m [ResultNode]
evaluateMd execId container [] = return []
evaluateMd execId container (hd : lst) = case hd of
  Code lang code node -> do
    eval <- EvalNode node <$> executeCode execId container (toCode lang code)
    rest <- evaluateMd execId container lst
    return $ eval : rest
  RawNode node xmdNodes -> do
    eval <- RenderNode node <$> evaluateMd execId container xmdNodes
    rest <- evaluateMd execId container lst
    return $ eval : rest

evaluate :: Has LxcIOErr sig m => String -> Container -> String -> m [ResultNode]
evaluate execId container = evaluateMd execId container . parse
