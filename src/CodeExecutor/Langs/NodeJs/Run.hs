module CodeExecutor.Langs.NodeJs.Run where

import CodeExecutor.Langs.NodeJs.Templates
import CodeExecutor.Utils
import Container.Eff
import Control.Algebra
import Control.Monad.IO.Class
import GHC.IO.Exception

wrap execId = importsTemplate execId . contextTemplate execId

run :: Has LxcIOErr sig m => String -> Container -> String -> m Result
run execId c code = exec c ["node", "-e", wrap execId code]
