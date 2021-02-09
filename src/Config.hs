module Config where

import Happstack.Server

version = "0.0.0"

containerPoolSize = 3 :: Int

serverHost = "core.polykode.local"

serverPort = 3000

serverBaseUrl = "http://" ++ serverHost ++ ":" ++ show serverPort

serverConf =
  Conf
    { port = serverPort,
      validator = Nothing,
      logAccess = Just logMAccess,
      timeout = 30,
      threadGroup = Nothing
    }
