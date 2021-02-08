module Config where

import Happstack.Server

containerPoolSize = 3 :: Int

serverPort = 3000

serverHost = "10.118.192.132"

serverBaseUrl = "http://" ++ serverHost ++ ":" ++ show serverPort

serverConf =
  Conf
    { port = serverPort,
      validator = Nothing,
      logAccess = Just logMAccess,
      timeout = 30,
      threadGroup = Nothing
    }
