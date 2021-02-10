module CodeExecutor.Utils where

import Container.Eff
import Control.Algebra

createCodeFile :: Has LxcIOErr sig m => String -> Container -> String -> m String
createCodeFile name c code = codeFile <$ createFileInContainer c codeFile code
  where
    codeFile = "/tmp/" ++ name
