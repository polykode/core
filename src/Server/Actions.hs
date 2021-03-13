{-# LANGUAGE OverloadedStrings #-}

module Server.Actions where

import CodeBlocks
import Config
import Container.Algebra
import Container.Eff
import Container.SocketDaemon
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
import Utils

sendResponse :: WS.Connection -> Response -> IO ()
sendResponse conn resp = WS.sendTextData conn $ Json.encode resp

runMdFile :: String -> Container -> [CodeBlock] -> IO (Either Error [CodeBlockResult])
runMdFile execId c codeblocks = do
  withLxc $ removeSocketFile c :: IO (Either Error ())
  threadId <- forkIO . void $ (withLxc $ runDaemon c :: IO (Either Error ()))
  fd <- openFd daemonSocketFile ReadOnly (Just socketMode) defaultFileFlags
  threadWaitWrite fd
  result <- withLxc . evaluateBlocks execId c $ codeblocks
  killThread threadId
  return result

executeMdAction :: String -> ServerContext -> IO [CodeBlockResult]
executeMdAction execId ctx = do
  result <- do
    codeblocks <- mdToCodeBlocks . parseMarkdown <$> readFile "./examples/serial.md"
    container <- getCurrentContainer ctx
    putVariable execId "__exec_id__" (Json.String . T.pack $ execId) ctx
    nextContainer ctx
    runMdFile execId container codeblocks
  garbageCollect execId ctx
  return $ getEitherWithDef [] result

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
