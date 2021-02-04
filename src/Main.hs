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
import MdEval
import Parser
import Utils

mdxEff :: Has (LxcEff :+: Throw Error) sig m => String -> m [ResultNode]
mdxEff mdStr = do
  trace "Creating container pool" $ pure ()
  pool <- createContainerPool 2

  -- Running
  trace "Executing" $ pure ()
  result <- evaluateMd (head pool) (parse mdStr)

  -- Cleanup
  trace "No cleanup" $ pure ()
  --cleanContainerPool pool

  return result

mdx :: String -> IO (Either Error [ResultNode])
mdx code = runLxcIO . runThrow $ mdxEff code

main = do
  code <- readFile "./examples/serial.md"
  d <- mdx code
  print d
