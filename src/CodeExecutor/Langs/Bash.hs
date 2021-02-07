module CodeExecutor.Langs.Bash where

import CodeExecutor.Utils
import Container.Eff
import Control.Algebra
import Control.Monad.IO.Class
import GHC.IO.Exception

run :: Has LxcIOErr sig m => String -> Container -> String -> m Result
run execId c code = exec c ["bash", "-c", code]
