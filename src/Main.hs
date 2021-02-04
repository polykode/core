{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Container.Algebra
import Container.Eff
import Container.Pool
import Control.Algebra
import Control.Carrier.Throw.Either
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace
import Utils

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
