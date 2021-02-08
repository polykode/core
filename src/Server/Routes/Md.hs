{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.Routes.Md where

import CodeExecutor
import Container.Algebra
import Container.Eff
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
import Data.Bifunctor (bimap)
import qualified Data.Text as Text
import Happstack.Server
import MdEval
import Server.Context
import Server.JsonResponse
import Server.Utils
import Text.RawString.QQ

runMdFile :: String -> Container -> IO (Either Error [ResultNode])
runMdFile execId c = do
  contents <- readFile "./examples/serial.md"
  withLxc $ evaluate execId c contents

executeMdAction ctx = do
  let execId = "foobar"
  result <- liftIO $ do
    container <- getCurrentContainer ctx
    putVariable execId "__exec_id__" (Json.String . Text.pack $ execId) ctx
    nextContainer ctx
    putStrLn $ "Running on " ++ name container
    runMdFile execId container
  liftIO $ garbageCollect execId ctx
  case result of
    Right nodes -> do
      json ok $
        JsonResponse
          { status = Success,
            value = Just . Json.toJSONList . map Json.toJSON $ nodes,
            message = "Result of evaluation"
          }
    Left e -> json ok $ JsonResponse {status = ContextError, value = Nothing, message = show e}

routes ctx =
  [ dir "execute" . root $ do
      method POST
      executeMdAction ctx
  ]
