{-# LANGUAGE OverloadedStrings #-}

module Server.Actions where

import CodeBlocks
import Container.Algebra
import Container.Eff
import Control.Concurrent (forkIO, killThread, threadWaitWrite)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Parser
import Server.Context
import Server.Types
import System.Posix.Files
import System.Posix.IO

sendResponse :: WS.Connection -> Response -> IO ()
sendResponse conn resp = WS.sendTextData conn $ Json.encode resp

runMdFile :: String -> Container -> IO (Either Error [CodeBlockResult])
runMdFile execId c = do
  mdTree <- mdToCodeBlocks . parseMarkdown <$> readFile "./examples/serial.md"
  moveScript
  removeSocketFile
  threadId <- forkIO . void $ runDaemon
  fd <- openFd socketFile ReadOnly (Just socketMode) defaultFileFlags
  threadWaitWrite fd
  result <- withLxc . evaluateBlocks execId c $ mdTree
  killThread threadId
  return result
  where
    socketFile = "/tmp/polykode-core-rpc.sock"
    env = ["NODE_PATH=./node_modules:/usr/local/lib/node_modules"]
    removeSocketFile :: IO (Either Error Result)
    removeSocketFile = withLxc $ exec c ["rm", socketFile]
    moveScript :: IO (Either Error ())
    moveScript = withLxc $ filePush c "/opt/code/scripts/rpc.js" "/opt/scripts/rpc.js"
    runDaemon :: IO (Either Error Result)
    runDaemon = withLxc $ exec c ["sh", "-c", unwords $ env ++ ["node", "/opt/scripts/rpc.js", "daemon"]]

executeMdAction :: String -> ServerContext -> IO [CodeBlockResult]
executeMdAction execId ctx = do
  result <- liftIO $ do
    container <- getCurrentContainer ctx
    putVariable execId "__exec_id__" (Json.String . T.pack $ execId) ctx
    nextContainer ctx
    putStrLn $ "Running on " ++ name container
    runMdFile execId container
  liftIO $ garbageCollect execId ctx
  case result of
    Right nodes -> return nodes
    Left _ -> return []

debugLog req msg = liftIO . putStrLn $ ":: [" ++ show req ++ "] :: " ++ msg

handleRequest :: ServerContext -> WS.Connection -> Request -> WS.ServerApp
handleRequest ctx conn req _pending = do
  let send r = sendResponse conn $ Response {rsId = rqId req, rsAction = r, rsError = Nothing}
  let request = rqAction req
  let log = debugLog request
  case request of
    RqMd -> do
      let execId = "foobar"
      log execId
      initClientState execId ctx
      result <- executeMdAction execId ctx
      garbageCollect execId ctx
      send $ RsMd result
    RqContext execId act -> do
      case act of
        CtxGet name -> do
          result <- liftIO $ readVariable execId name ctx
          case result of
            Just res -> do
              log "found"
              send $ RsContext res
            Nothing -> do
              log "variable not found"
              sendResponse conn $
                Response {rsId = rqId req, rsAction = RsNull, rsError = Just "Variable not found"}
        CtxPut name value -> do
          log "update"
          liftIO $ do
            putStrLn $ "Ctx put:" ++ name ++ " -> " ++ show value
            putVariable execId name value ctx
            send $ RsContext value
    RqCall _execId _mod _fn _params -> do
      log ""
      send $ RsReturn $ Json.String "this was the return value"
    RqReturn _execId result -> do
      log ""
      liftIO $ putStrLn $ "Return value:" ++ show result
    RqBadRequest e -> do
      log "bad request"
      sendResponse conn $ Response {rsId = rqId req, rsAction = RsNull, rsError = Just e}
