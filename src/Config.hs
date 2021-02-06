module Config where

import Happstack.Server

containerPoolSize = 3 :: Int

serverPort = 3000

serverConf =
  Conf
    { port = serverPort,
      validator = Nothing,
      logAccess = Just logMAccess,
      timeout = 30,
      threadGroup = Nothing
    }
