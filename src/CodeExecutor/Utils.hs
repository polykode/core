module CodeExecutor.Utils where

import Config
import Container.Eff
import Control.Algebra

runCmd :: Has LxcIOErr sig m => Container -> [String] -> [String] -> m Result
runCmd c env cmd = exec c ["sh", "-c", unwords $ defaultExecEnv ++ env ++ cmd]

createCodeFile :: Has LxcIOErr sig m => String -> Container -> String -> m String
createCodeFile name c code = codeFile <$ createFileInContainer c codeFile code
  where
    codeFile = "/tmp/" ++ name
