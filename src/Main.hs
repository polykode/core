{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Config
import Control.Exception (SomeException, handle)
import Control.Monad (forever)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS
import qualified Network.WebSockets as WS
import Server.Actions
import Server.Context
import Server.Types
import Utils

application :: ServerContext -> WS.ServerApp
application ctx pending = do
  conn <- WS.acceptRequest pending
  forever $ do
    rawData <- WS.receiveData conn :: IO BS.ByteString
    let req = getMaybeWithDef parseErrorRequest . Json.decode $ rawData :: Request
    handle onDisconnect $ handleRequest ctx conn req pending
  where
    onDisconnect :: SomeException -> IO ()
    onDisconnect _ = pure ()

-- | The main entry point for the WS application
main :: IO ()
main = do
  putStrLn "Setting shit up..."
  ctx <- createMdxContext 3
  putStrLn $ "Server is running on port " ++ show serverPort
  WS.runServer "0.0.0.0" serverPort $ application ctx
