{-# LANGUAGE FlexibleContexts #-}

module Server.Routes.Md where

import CodeExecutor
import Container.Algebra
import Container.Eff
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
import qualified Data.Text as Text
import Happstack.Server
import Server.Context
import Server.JsonResponse
import Server.Utils

runCode :: Container -> Code -> IO (Either Error Result)
runCode c = withLxc . executeCode c

routes ctx =
  [ dir "execute" . root $ do
      method POST
      -- TODO: Create and cleanup state for exec id
      result <- liftIO $ do
        container <- getCurrentContainer ctx
        nextContainer ctx
        putStrLn $ "Running on " ++ name container
        runCode container $ Bash "echo fuck off"
      json emptyResponse
  ]
