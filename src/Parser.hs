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

mapDoc :: [XMDNode] -> [Node] -> [XMDNode]
mapDoc acc [] = acc
mapDoc acc (Node _ (CODE_BLOCK lang code) nodes : rest) = mapDoc (acc ++ newNodes) rest
  where
    cmarkNode = Node Nothing (CODE_BLOCK lang code) []
    newNodes = [Code (Text.unpack lang) code cmarkNode]
mapDoc acc (Node _ nodeType nodes : rest) = mapDoc (acc ++ newNodes) rest
  where
    newNodes = [RawNode (Node Nothing nodeType nodes) []]

parse = mapDoc [] . (: []) . commonmarkToNode cmarkOptions . Text.pack
