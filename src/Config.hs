module Config where

version = "0.0.0"

containerPoolSize = 3 :: Int

serverHost = "core.polykode.local"

serverPort = 3000 :: Int

serverBaseUrl = "ws://" ++ serverHost ++ ":" ++ show serverPort
