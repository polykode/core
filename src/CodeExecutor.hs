{-# LANGUAGE LambdaCase #-}

module CodeExecutor where

import qualified CodeExecutor.Langs.Bash
import qualified CodeExecutor.Langs.NodeJs
import Container.Eff
import Control.Algebra
import GHC.IO.Exception

data Code
  = Bash String
  | NodeJs String
  deriving (Show)

toCode :: String -> String -> Code
toCode lang code = case lang of
  "bash" -> Bash code
  "javascript" -> NodeJs code
  "js" -> NodeJs code

executeCode :: Has LxcIOErr sig m => String -> Container -> Code -> m Result
executeCode execId container = \case
  Bash code -> CodeExecutor.Langs.Bash.run execId container code
  NodeJs code -> CodeExecutor.Langs.NodeJs.run execId container code
