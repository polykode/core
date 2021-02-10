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
import Utils

data CodeModule = CodeModule String Code ModuleExports
  deriving (Show)

type DocumentDataStore = Map.Map String Json.Value

data MdxContext = MdxContext
  { ctxPool :: ContainerPool,
    ctxCurrent :: MVar Int,
    ctxDataStore :: MVar (Map.Map String DocumentDataStore),
    ctxModules :: MVar (Map.Map String CodeModule)
  }

parseValue :: String -> Maybe Json.Value
parseValue = Json.decode . ByteString.pack

readVariable :: String -> String -> MdxContext -> IO (Maybe Json.Value)
readVariable execId varname ctx = do
  store <- readMVar $ ctxDataStore ctx
  return $ Map.lookup execId store >>= \store -> Map.lookup varname store

putVariable :: String -> String -> Json.Value -> MdxContext -> IO ()
putVariable execId varname value ctx = do
  store <- readMVar . ctxDataStore $ ctx
  let variables = getMaybeWithDef Map.empty . Map.lookup execId $ store
  let newVariables = Map.insert varname value variables
  modifyMVar_ (ctxDataStore ctx) $ return . Map.insert execId newVariables
  return ()

garbageCollect :: String -> MdxContext -> IO ()
garbageCollect execId ctx =
  modifyMVar_ (ctxDataStore ctx) $ return . Map.delete execId

containerPool :: Int -> IO (Either Error ContainerPool)
containerPool = withLxc . createContainerPool

createMdxContext :: Int -> IO MdxContext
createMdxContext poolSize = do
  pool <- getEitherWithDef [] <$> containerPool poolSize
  currentRef <- newMVar 0
  store <- newMVar Map.empty
  moduleMap <- newMVar Map.empty
  return $
    MdxContext
      { ctxPool = pool,
        ctxCurrent = currentRef,
        ctxDataStore = store,
        ctxModules = moduleMap
      }

cyclicIncrement max = (`mod` max) . (1 +)

nextContainer :: MdxContext -> IO ()
nextContainer ctx =
  modifyMVar_ (ctxCurrent ctx) $ return . cyclicIncrement (length . ctxPool $ ctx)

getCurrentContainer :: MdxContext -> IO Container
getCurrentContainer ctx = do
  cIndex <- readMVar . ctxCurrent $ ctx
  return $ ctxPool ctx !! cIndex
