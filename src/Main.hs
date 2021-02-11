{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Exception (SomeException, fromException, handle)
import Control.Monad (forM_, forever, replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict (toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Server.Context
import Server.JsonResponse
import Server.Utils
import Utils

type Client = (T.Text, WS.Connection)

addClient :: Client -> [Client] -> [Client]
addClient client clients = client : clients

removeClient :: Client -> [Client] -> [Client]
removeClient client = filter ((/= fst client) . fst)

sendResponse :: WS.Connection -> Response -> IO ()
sendResponse conn resp = WS.sendTextData conn $ Json.encode resp

handleRequest :: MVar [Client] -> WS.Connection -> Request -> WS.ServerApp
handleRequest state conn req pending = do
  let send r = sendResponse conn $ Response {rsId = rqId req, rsAction = r, rsError = Nothing}
  let execId = "foobar"
  case rqAction req of
    RqMd -> do
      liftIO $ putStrLn "Running stuff"
      send $ RsMd "Foobar"
    -- Execute script
    RqContext execId act -> do
      case act of
        CtxGet name -> do
          liftIO $ putStrLn $ "Ctx lookup:" ++ name
          send $ RsContext $ Json.String "this was in ctx"
        CtxPut name value ->
          liftIO $ putStrLn $ "Ctx put: " ++ show value
    RqCall execId mod fn params -> do
      liftIO $ putStrLn $ "Fn call " ++ mod ++ "." ++ fn ++ show params
      send $ RsReturn $ Json.String "this was the return value"
    RqReturn execId result -> do
      liftIO $ putStrLn $ "Return value:" ++ show result
    RqBadRequest e -> do
      liftIO $ putStrLn "Bad req"
      sendResponse conn $ Response {rsId = rqId req, rsAction = RsMd "x", rsError = Just e}

application :: MVar [Client] -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  forever $ do
    req <- getMaybeWithDef parseErrorRequest . Json.decode <$> WS.receiveData conn :: IO Request
    liftIO $ print req
    handle onDisconnect $ handleRequest state conn req pending
    --clients <- liftIO $ readMVar state
    --let client = (query, conn)
    --modifyMVar_ state $ return . addClient client
    --perform state client
    liftIO $ putStrLn "fin"
  where
    onDisconnect :: SomeException -> IO ()
    onDisconnect _ = pure ()

--perform :: MVar [Client] -> Client -> IO ()
--perform state client@(query, conn) = handle onDisconnect $ do
--WS.sendTextData conn (T.pack $ show [1, 2, 3, 4, 5])
--WS.sendClose conn (T.pack "")
--removeClient' state client
--where
--onDisconnect :: SomeException -> IO ()
--onDisconnect _ = removeClient' state client
--removeClient' :: MVar [Client] -> Client -> IO ()
--removeClient' state client = liftIO $ modifyMVar_ state $ return . removeClient client

-- | The main entry point for the WS application
main :: IO ()
main = do
  putStrLn "Server is running"
  state <- newMVar []
  WS.runServer "0.0.0.0" 3000 $ application state

--
--
--
--
