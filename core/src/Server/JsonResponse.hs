module Server.JsonResponse where

import qualified Data.Aeson as Json
import qualified Data.Text as Text

toJsonString = Json.String . Text.pack
