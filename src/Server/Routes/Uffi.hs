{-# LANGUAGE FlexibleContexts #-}

module Server.Routes.Uffi where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import Happstack.Server
import Server.Context
import Server.JsonResponse
import Server.Utils
import Utils

readVariableAction var ctx = do
  execId <- queryString $ look "exec_id"
  value <- liftIO $ readVariable execId var ctx
  json $
    JsonResponse
      { status = case value of
          Just _ -> Success
          Nothing -> ContextError,
        message = case value of
          Just _ -> var ++ " found"
          Nothing -> "Variable " ++ var ++ " not found",
        value = value
      }

updateVariableAction var ctx = do
  execId <- queryString $ look "exec_id"
  decodePostBody
  strValue <- getDataFn $ look "value"
  case strValue of
    Right strValue -> do
      let variableValue = getMaybeWithDef (Json.String . Text.pack $ strValue) $ parseValue strValue
      liftIO $ putVariable execId var variableValue ctx
      value <- liftIO $ readVariable execId var ctx
      json JsonResponse {status = Success, message = "Saved variable", value = Nothing}
    Left e -> do
      json $ JsonResponse {status = RequestError, message = show e, value = Nothing}

routes ctx =
  [ dir "call" . path $ \mod -> path $ \fn -> root $ do
      method POST
      execId <- queryString $ look "exec_id"
      liftIO . putStrLn $ mod ++ "." ++ fn
      json emptyResponse,
    dir "variable" . path $ \var -> root $ do
      method GET
      readVariableAction var ctx,
    dir "variable" . path $ \var -> root $ do
      method POST
      updateVariableAction var ctx
  ]
