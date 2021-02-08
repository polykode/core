{-# LANGUAGE QuasiQuotes #-}

module CodeExecutor.Langs.Bash.Run where

import CodeExecutor.Langs.Bash.Templates
import CodeExecutor.Utils
import Container.Eff
import Control.Algebra
import Control.Monad.IO.Class
import GHC.IO.Exception

wrap execId = importsTemplate execId . contextTemplate execId

run :: Has LxcIOErr sig m => String -> Container -> String -> m Result
run execId c code = exec c ["bash", "-c", wrap execId code]
