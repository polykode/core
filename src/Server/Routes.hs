{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Server.Routes where

import CodeExecutor
import Container.Algebra
import Container.Eff
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import GHC.Generics
import Happstack.Server
import Server.Context
import Utils

root m = nullDir >> m

runCode :: Container -> Code -> IO (Either Error Result)
runCode c = withLxc . executeCode c

data JsonResponse = JsonResponse
  { status :: Int,
    message :: String,
    value :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance Json.FromJSON JsonResponse

instance Json.ToJSON JsonResponse

emptyResponse = JsonResponse {status = 1, message = "nothing", value = Just ""}

json :: FilterMonad Response m => JsonResponse -> m String
json = ok . ByteString.unpack . Json.encode

uffiRoutes ctx =
  [ dir "call" . path $ \mod -> path $ \fn -> root $ do
      method POST
      execId <- queryString $ look "exec_id"
      liftIO . putStrLn $ mod ++ "." ++ fn
      json emptyResponse,
    dir "variable" . path $ \var -> root $ do
      method GET
      execId <- queryString $ look "exec_id"
      value <- liftIO $ readVariable execId var ctx
      case value of
        Just value ->
          json $
            JsonResponse
              { status = 200,
                message = var ++ " found",
                value = Just $ show value
              }
        Nothing ->
          json $
            JsonResponse
              { status = 200,
                message = "Variable not found",
                value = Nothing
              },
    dir "variable" . path $ \var -> root $ do
      method POST
      execId <- queryString $ look "exec_id"
      decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
      strValue <- getDataFn $ look "value"
      case strValue of
        Right strValue -> do
          let variableValue = getMaybeWithDef (Json.String . Text.pack $ strValue) $ parseValue strValue
          liftIO $ putVariable execId var variableValue ctx
          value <- liftIO $ readVariable execId var ctx
          json JsonResponse {status = 200, message = "Saved variable", value = Nothing}
        Left e -> do
          json $ JsonResponse {status = 400, message = show e, value = Nothing}
  ]

mdRoutes ctx =
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

routes ctx =
  msum $
    [root $ ok "Fuck"]
      ++ map (dir "uffi") (uffiRoutes ctx)
      ++ map (dir "md") (mdRoutes ctx)
      ++ [notFound "Not sure how you got here but fuck off please"]
