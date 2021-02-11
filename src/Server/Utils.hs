{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Utils where

import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict (toList)
import qualified Data.Text as T
import Server.JsonResponse
import Utils

type ExecId = String

data CtxAction
  = CtxGet String
  | CtxPut String Json.Value
  deriving (Show)

-- Needs from json
data RequestAction
  = RqMd -- Instant result (execute md)
  | RqContext ExecId CtxAction -- CtxGet -> Instant result (map lookup) | CtxPut -> No result
  | RqCall ExecId String String [Json.Value] -- Respond after the return value is recieved
  | RqReturn ExecId Json.Value -- No result ()
  | RqBadRequest String -- Error
  deriving (Show)

instance Json.FromJSON RequestAction where
  parseJSON (Json.Object obj) = do
    action <- obj .: "action" :: Parser String
    case action of
      "md/execute" -> return RqMd
      "ctx/get" -> do
        execId <- obj .: "exec_id"
        name <- obj .: "name"
        return $ RqContext execId (CtxGet name)
      "ctx/put" -> do
        execId <- obj .: "exec_id"
        name <- obj .: "name"
        value <- obj .: "value"
        return $ RqContext execId (CtxPut name value)
      "fn/call" -> do
        execId <- obj .: "exec_id"
        mod <- obj .: "module"
        name <- obj .: "name"
        params <- obj .: "params"
        return $ RqCall execId mod name params
      "fn/return" -> do
        execId <- obj .: "exec_id"
        result <- obj .: "result"
        return $ RqReturn execId result
      _ -> return $ RqBadRequest $ "Invalid action " ++ action

-- Needs to json
data ResponseAction
  = RsMd String -- Result of RqMd (cleanup on end)
  | RsContext Json.Value -- On CtxGet
  | RsCall String String [Json.Value] -- Call to module exports
  | RsReturn Json.Value --
  deriving (Show)

instance Json.ToJSON ResponseAction where
  toJSON (RsMd result) =
    Json.object
      [ ( "result",
          Json.object -- TODO: Real result
            [ ("exitCode", toJsonString . show $ 1),
              ("stdout", toJsonString $ show result),
              ("stderr", toJsonString "")
            ]
        )
      ]
  toJSON (RsContext result) = Json.object [("type", "ctx/get"), ("result", result)]
  toJSON (RsReturn result) = Json.object [("type", "fn/return"), ("result", result)]
  toJSON (RsCall mod fn params) =
    Json.object
      [ ("type", "fn/call"),
        ("module", toJsonString mod),
        ("name", toJsonString fn),
        ("params", Json.toJSONList params)
      ]

data Request = Request
  { rqId :: Integer,
    rqAction :: RequestAction
  }
  deriving (Show)

parseErrorRequest = Request (-1) $ RqBadRequest "Unable to parse request"

instance Json.FromJSON Request where
  parseJSON (Json.Object obj) = do
    id <- obj .: "id" :: Parser Integer
    action <- Json.parseJSON $ Json.Object obj
    return $ Request id action

data Response = Response
  { rsId :: Integer,
    rsAction :: ResponseAction,
    rsError :: Maybe String
  }
  deriving (Show)

instance Json.ToJSON Response where
  toJSON resp =
    Json.object $
      [ ("id", Json.Number . fromInteger . rsId $ resp),
        ("error", Json.String . T.pack . getMaybeWithDef "" . rsError $ resp)
      ]
        ++ extraProps
    where
      extraProps = case Json.toJSON $ rsAction resp of
        Json.Object ls -> toList ls
        _ -> []
