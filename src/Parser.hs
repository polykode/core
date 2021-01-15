{-# LANGUAGE LambdaCase #-}

module Parser where

import CMark
import qualified Data.Text as Text
import Prelude

-- TODO: Actual meta data please
data XMDNode
  = MetaData String
  | Code String Text.Text Node
  | RawNode Node [XMDNode]
  deriving (Show, Eq)

cmarkOptions = [optUnsafe, optNormalize]

wrapNodes :: [XMDNode] -> [Node] -> [XMDNode]
wrapNodes acc = \case
  [] -> acc
  ((Node _ (CODE_BLOCK lang code) _) : rest) -> wrapNodes (acc ++ newNodes) rest
    where
      newNodes = [Code (Text.unpack lang) code cmarkNode]
      cmarkNode = Node Nothing (CODE_BLOCK lang code) []
  ((Node _ nodeType nodes) : rest) -> wrapNodes (acc ++ newNodes) rest
    where
      newNodes = [RawNode (Node Nothing nodeType nodes) $ wrapNodes [] nodes]

parse = wrapNodes [] . (: []) . commonmarkToNode cmarkOptions . Text.pack
