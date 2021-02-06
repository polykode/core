{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.JsonResponse where

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as ByteString
import GHC.Generics
import Happstack.Server

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
