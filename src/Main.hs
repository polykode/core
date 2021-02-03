{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Container
import Control.Algebra
import Control.Carrier.Throw.Either
import Control.Effect.Throw
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Kind (Type)
import Debug.Trace
import GHC.IO.Exception
import System.Process
import Utils

-- Template container
template = Container "template-container"

createContainer :: Has (LxcEff :+: Throw Error) sig m => String -> m Container
createContainer name = Container name <$ (copy template name >>= start)

cleanupContainer :: Has (LxcEff :+: Throw Error) sig m => Container -> m ()
cleanupContainer c = stop c >> delete c

toContainerName n = "container--" ++ show n

createContainerPool :: Has (LxcEff :+: Throw Error) sig m => Int -> m [Container]
createContainerPool count = concatM . map (createContainer . toContainerName) $ [1 .. count - 1]

cleanContainerPool :: Has (LxcEff :+: Throw Error) sig m => [Container] -> m ()
cleanContainerPool = void . concatM . map cleanupContainer

sayHelloEff :: Has (LxcEff :+: Throw Error) sig m => m Result
sayHelloEff = do
  trace "Creating container pool" $ pure ()
  pool <- createContainerPool 4
  c <- pure . head $ pool

  -- Running
  trace "Executing" $ pure ()
  result <- exec c ["echo", "Hello", "world!"]

  -- Cleanup
  trace "Cleanup" $ pure ()
  cleanContainerPool pool

  return result

sayHello :: IO (Either Error Result)
sayHello = runLxcIO (runThrow sayHelloEff)

main = sayHello >>= print
