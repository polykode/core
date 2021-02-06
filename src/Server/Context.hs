module Server.Context where

import Container.Algebra
import Container.Eff
import Container.Pool
import Control.Concurrent.MVar
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Map as Map
import Utils

type DocumentDataStore = Map.Map String Json.Value

data MdxContext = MdxContext
  { ctxPool :: ContainerPool,
    ctxCurrent :: MVar Int,
    ctxDataStore :: MVar (Map.Map String DocumentDataStore)
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

containerPool :: Int -> IO (Either Error ContainerPool)
containerPool = withLxc . createContainerPool

createMdxContext :: Int -> IO MdxContext
createMdxContext poolSize = do
  pool <- getEitherWithDef [] <$> containerPool poolSize
  currentRef <- newMVar 0
  store <- newMVar Map.empty
  return $
    MdxContext
      { ctxPool = pool,
        ctxCurrent = currentRef,
        ctxDataStore = store
      }

cyclicIncrement max = (`mod` max) . (1 +)

nextContainer :: MdxContext -> IO ()
nextContainer ctx =
  modifyMVar_ (ctxCurrent ctx) $ return . cyclicIncrement (length . ctxPool $ ctx)

getCurrentContainer :: MdxContext -> IO Container
getCurrentContainer ctx = do
  cIndex <- readMVar . ctxCurrent $ ctx
  return $ ctxPool ctx !! cIndex
