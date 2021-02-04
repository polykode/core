{-# LANGUAGE LambdaCase #-}

module MdEval where

import CMark
import Container.Eff
import Control.Algebra
import qualified Data.Text as T
import Parser

type EvaluationResult = Result

data ResultNode
  = RenderNode Node [ResultNode]
  | EvalNode Node EvaluationResult
  deriving (Show, Eq)

--instance Show ResultNode where
--show (RenderNode (Node _ nodeType cNodes) cResults) =
--show nodeType ++ ": [ " ++ show cResults ++ "] " --  ++ "(" ++ show rest ++ ")"
--show (EvalNode node result) = show result

executeBashCommand :: Has LxcIOErr sig m => Container -> String -> m Result
executeBashCommand container code = exec container ["bash", "-c", code]

evaluateMd :: Has LxcIOErr sig m => Container -> [XMDNode] -> m [ResultNode]
evaluateMd container [] = return []
evaluateMd container (hd : lst) = case hd of
  Code lang code node -> do
    eval <- EvalNode node <$> executeBashCommand container (T.unpack code)
    rest <- evaluateMd container lst
    return $ eval : rest
  RawNode node xmdNodes -> do
    eval <- RenderNode node <$> evaluateMd container xmdNodes
    rest <- evaluateMd container lst
    return $ eval : rest

evaluate :: Has LxcIOErr sig m => Container -> String -> m [ResultNode]
evaluate container = evaluateMd container . parse
