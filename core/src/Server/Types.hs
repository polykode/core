{-# LANGUAGE OverloadedStrings #-}

module Server.Types where

import CodeBlocks
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict (toList)
import qualified Data.Text as T
import Server.JsonResponse
import Utils

type ExecId = String

-- | Context action
data CtxAction
  = CtxGet String
  | CtxPut String Json.Value
  deriving (Show)

-- | Request action
data RequestAction
  = RqExecute [CodeBlock] -- Instant result (execute md)
  | RqContext ExecId CtxAction -- CtxGet -> Instant result (map lookup) | CtxPut -> No result
  | RqCall ExecId String String [Json.Value] -- Respond after the return value is recieved
  | RqReturn ExecId Json.Value -- No result ()
  | RqBadRequest String -- Error
  deriving (Show)

instance Json.FromJSON RequestAction where
  parseJSON = Json.withObject "RequestAction" $ \obj -> do
    action <- obj .: "action" :: Parser String
    case action of
      "blocks/execute" -> do
        codeblocks <- obj .: "blocks" :: Parser [CodeBlock]
        return $ RqExecute codeblocks
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

-- | Response action object
data ResponseAction
  = RsExecute [CodeBlockResult] -- Result of RqExecute (cleanup on end)
  | RsContext Json.Value -- On CtxGet
  | RsCall String String [Json.Value] -- Call to module exports
  | RsReturn Json.Value -- return value
  | RsNull
  deriving (Show)

instance Json.ToJSON ResponseAction where
  toJSON (RsExecute nodes) = Json.object [("type", "blocks/result"), ("result", Json.toJSONList nodes)]
  toJSON (RsContext result) = Json.object [("type", "ctx/get"), ("value", result)]
  toJSON (RsReturn result) = Json.object [("type", "fn/return"), ("value", result)]
  toJSON RsNull = Json.object []
  toJSON (RsCall mod fn params) =
    Json.object
      [ ("type", "fn/call"),
        ("module", toJsonString mod),
        ("name", toJsonString fn),
        ("params", Json.toJSONList params)
      ]

-- | Api request object
data Request = Request
  { rqId :: String,
    rqAction :: RequestAction
  }
  deriving (Show)

parseErrorRequest = Request "" $ RqBadRequest "Unable to parse request"

instance Json.FromJSON Request where
  parseJSON = Json.withObject "Request" $ \obj -> do
    id <- obj .: "id" :: Parser String
    action <- Json.parseJSON $ Json.Object obj
    return $ Request id action

-- | Api response object
data Response = Response
  { rsId :: String,
    rsAction :: ResponseAction,
    rsError :: Maybe String
  }
  deriving (Show)

instance Json.ToJSON Response where
  toJSON resp =
    Json.object $
      [ ("id", toJsonString . rsId $ resp),
        ("error", Json.String . T.pack . getMaybeWithDef "" . rsError $ resp)
      ]
        ++ extraProps
    where
      extraProps = case Json.toJSON $ rsAction resp of
        Json.Object ls -> toList ls
        _ -> []
