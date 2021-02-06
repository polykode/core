{-# LANGUAGE QuasiQuotes #-}

module Main where

import Config
import Happstack.Server
import Server.Context
import Server.Routes
import Text.RawString.QQ

main = do
  putStrLn "Creating container pool"
  ctx <- createMdxContext containerPoolSize

  putStrLn $ "Starting server on port " ++ show (port serverConf)
  simpleHTTP serverConf $ routes ctx
