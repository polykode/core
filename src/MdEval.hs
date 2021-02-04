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

evaluateMd :: Has LxcIOErr sig m => Container -> [XMDNode] -> m [ResultNode]
evaluateMd container [] = return []
evaluateMd container (hd : lst) = case hd of
  Code lang code node -> do
    eval <- EvalNode node <$> executeCode container (toCode lang code)
    rest <- evaluateMd container lst
    return $ eval : rest
  RawNode node xmdNodes -> do
    eval <- RenderNode node <$> evaluateMd container xmdNodes
    rest <- evaluateMd container lst
    return $ eval : rest

evaluate :: Has LxcIOErr sig m => Container -> String -> m [ResultNode]
evaluate container = evaluateMd container . parse
