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

routes ctx =
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
                value = Just . ByteString.unpack . Json.encode $ value
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
