{-# LANGUAGE LambdaCase #-}

module MdEval where

import CMark
import CodeExecutor
import Container.Eff
import Control.Algebra
import qualified Data.Aeson as Json
import qualified Data.Text as Text
import Parser
import Server.JsonResponse

type EvaluationResult = Result

data ResultNode
  = RenderNode Node [ResultNode]
  | EvalNode Node EvaluationResult
  deriving (Eq)

instance Show ResultNode where
  show (RenderNode (Node _ nodeType cNodes) cResults) =
    show nodeType ++ ": [ " ++ show cResults ++ "] " -- ++ "(" ++ show rest ++ ")"
  show (EvalNode node result) = show node

instance Json.ToJSON ResultNode where
  toJSON (RenderNode node children) =
    Json.object
      [ (Text.pack "type", toJsonString "render"),
        -- (Text.pack "node", toJsonString . show $ node),
        (Text.pack "children", Json.toJSONList . map Json.toJSON $ children)
      ]
  toJSON (EvalNode node (exitCode, stdout, stderr)) =
    Json.object
      [ (Text.pack "type", toJsonString "eval"),
        (Text.pack "node", toJsonString . show $ node),
        ( Text.pack "result",
          Json.object
            [ (Text.pack "exitCode", toJsonString . show $ exitCode),
              (Text.pack "stdout", toJsonString stdout),
              (Text.pack "stderr", toJsonString stderr)
            ]
        )
      ]

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
