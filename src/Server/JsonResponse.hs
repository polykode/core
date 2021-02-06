{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Server.JsonResponse where

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import GHC.Generics
import Happstack.Server

-- Response status
data ResponseStatus = Success | ContextError | RequestError | ServerError | Ignore
  deriving (Show, Generic, Json.FromJSON, Json.ToJSON)

-- Api standard response shape
data JsonResponse = JsonResponse
  { status :: ResponseStatus,
    message :: String,
    value :: Maybe Json.Value
  }
  deriving (Show, Generic, Json.FromJSON, Json.ToJSON)

-- Helper functions

-- Just temporary json response placeholder for debugging
emptyResponse = JsonResponse {status = Ignore, message = "nothing", value = Just . Json.String . Text.pack $ ""}

-- send json response
-- TODO: Allow non-ok status
-- TODO: Set content type json
json :: FilterMonad Response m => JsonResponse -> m String
json = ok . ByteString.unpack . Json.encode
