module Server.Context where

import CodeBlocks
import CodeExecutor
import Container.Algebra
import Container.Eff
import Container.Pool
import Control.Concurrent.MVar
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Map as Map
import qualified Network.WebSockets as WS
import Utils

data CodeModule = CodeModule String Code ModuleExports
  deriving (Show)

type DocumentDataStore = Map.Map String Json.Value

-- clients ({ [execId] : last-rpc-id rpc-queue context modules  })

data ClientState = ClientState
  { csData :: DocumentDataStore,
    csModules :: Map.Map String CodeModule,
    csConnection :: [WS.Connection]
  }

emptyClientState = ClientState {csData = Map.empty, csModules = Map.empty, csConnection = []}

data ServerContext = ServerContext
  { ctxPool :: ContainerPool,
    ctxCurrent :: MVar Int,
    ctxClients :: MVar (Map.Map String ClientState)
  }

parseValue :: String -> Maybe Json.Value
parseValue = Json.decode . ByteString.pack

getClient :: String -> ServerContext -> IO (Maybe ClientState)
getClient execId ctx = do
  Map.lookup execId <$> (readMVar . ctxClients $ ctx)

initClientState :: String -> ServerContext -> IO ()
initClientState execId ctx = do
  let initClient = return . Map.insert execId emptyClientState
  modifyMVar_ (ctxClients ctx) initClient

updateDataStore :: String -> ServerContext -> (DocumentDataStore -> DocumentDataStore) -> IO ()
updateDataStore execId ctx fn = do
  client <- getClient execId ctx
  getMaybeWithDef (pure ()) $ do
    client <- client
    let newStore = fn . csData $ client
    let newClient = client {csData = newStore}
    return $ modifyMVar_ (ctxClients ctx) $ return . Map.insert execId newClient

readVariable :: String -> String -> ServerContext -> IO (Maybe Json.Value)
readVariable execId varname ctx = do
  client <- getClient execId ctx
  return $ client >>= Map.lookup varname . csData

putVariable :: String -> String -> Json.Value -> ServerContext -> IO ()
putVariable execId varname value ctx =
  updateDataStore execId ctx $ Map.insert varname value

garbageCollect :: String -> ServerContext -> IO ()
garbageCollect execId ctx =
  modifyMVar_ (ctxClients ctx) $ return . Map.delete execId

containerPool :: Int -> IO [Either Error Container]
containerPool = parallelIO . map withLxc . createContainerPool

createMdxContext :: Int -> IO ServerContext
createMdxContext poolSize = do
  -- TODO: Filter errors in container pool
  pool <- map (getEitherWithDef $ Container "x") <$> containerPool poolSize
  currentRef <- newMVar 0
  clients <- newMVar Map.empty
  return $
    ServerContext
      { ctxPool = pool,
        ctxCurrent = currentRef,
        ctxClients = clients
      }

cyclicIncrement max = (`mod` max) . (1 +)

nextContainer :: ServerContext -> IO ()
nextContainer ctx =
  modifyMVar_ (ctxCurrent ctx) $ return . cyclicIncrement (length . ctxPool $ ctx)

getCurrentContainer :: ServerContext -> IO Container
getCurrentContainer ctx = do
  cIndex <- readMVar . ctxCurrent $ ctx
  return $ ctxPool ctx !! cIndex
