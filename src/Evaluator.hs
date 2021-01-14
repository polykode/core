{-# LANGUAGE LambdaCase #-}

module Evaluator where

import CMark
import Parser

type Result = String

data ResultNode
  = RenderNode Node [ResultNode]
  | EvalNode Node Result
  deriving (Show)

--instance Show ResultNode where
--show (RenderNode (Node _ nodeType cNodes) cResults) =
--show nodeType ++ ": [ " ++ show cResults ++ "] " --  ++ "(" ++ show rest ++ ")"
--show (EvalNode node result) = show result

evaluateMd :: [XMDNode] -> IO [ResultNode]
evaluateMd [] = return []
evaluateMd (hd : lst) = case hd of
  Code lang code node -> do
    result <- return . EvalNode node $ "Foobar"
    results <- evaluateMd lst
    return $ result : results
  RawNode node xmdNodes -> do
    result <- RenderNode node <$> evaluateMd xmdNodes
    results <- evaluateMd lst
    return $ result : results

evaluate = evaluateMd . parse
