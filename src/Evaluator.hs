{-# LANGUAGE LambdaCase #-}

module Evaluator where

import CMark
import Container
import qualified Data.Text as T
import Parser

type EvaluationResult = T.Text

data ResultNode
  = RenderNode [ResultNode] Node
  | EvalNode EvaluationResult Node
  deriving (Show)

--instance Show ResultNode where
--show (RenderNode (Node _ nodeType cNodes) cResults) =
--show nodeType ++ ": [ " ++ show cResults ++ "] " --  ++ "(" ++ show rest ++ ")"
--show (EvalNode node result) = show result

evaluateMd = 5

--executeBashCommand container code = do
  --printContainerState container
  --waitForStartup container
  --(exitCode, output, err) <- runCommand container "sh" ["-c", code]
  --liftIO $ do
    --putStrLn $ "Output: " ++ show output
    --putStrLn $ "Err: " ++ show err
  --return output

--evaluateMd :: ContainerContext -> [XMDNode] -> IOErr [ResultNode]
--evaluateMd container [] = return []
--evaluateMd container (hd : lst) = case hd of
  --Code lang code node -> do
    --eval <- (`EvalNode` node) <$> executeBashCommand container (T.unpack code)
    --rest <- evaluateMd container lst
    --return $ eval : rest
  --RawNode node xmdNodes -> do
    --eval <- (`RenderNode` node) <$> evaluateMd container xmdNodes
    --rest <- evaluateMd container lst
    --return $ eval : rest

--evaluate container mdStr = do
  --launch container
  --evaluateMd container . parse $ mdStr
