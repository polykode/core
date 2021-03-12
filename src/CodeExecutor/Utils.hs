module CodeExecutor.Utils where

import Container.Eff
import Control.Algebra

defaultEnv = ["NODE_PATH=./node_modules:/usr/local/lib/node_modules"]

runCmd :: Has LxcIOErr sig m => Container -> [String] -> [String] -> m Result
runCmd c env cmd = exec c ["sh", "-c", unwords $ defaultEnv ++ env ++ cmd]

createCodeFile :: Has LxcIOErr sig m => String -> Container -> String -> m String
createCodeFile name c code = codeFile <$ createFileInContainer c codeFile code
  where
    codeFile = "/tmp/" ++ name
