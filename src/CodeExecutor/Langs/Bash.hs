module CodeExecutor.Langs.Bash where

import CodeExecutor.Utils
import Container.Eff
import Control.Algebra
import Control.Monad.IO.Class
import GHC.IO.Exception

run :: Has LxcIOErr sig m => ([String] -> m Result) -> String -> m Result
run execCmd code = execCmd ["bash", "-c", code]
