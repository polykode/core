{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Container where

import Control.Algebra
import Control.Effect.Throw
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Kind (Type)
import GHC.IO.Exception
import System.Process

type Result = (ExitCode, String, String)

newtype Container = Container
  { name :: String
  }
  deriving (Show)

data Error = ContainerErr Int String | RunErr String deriving (Show)

data LxcEff (m :: Type -> Type) k where
  Start :: Container -> LxcEff m (Either Error ())
  Stop :: Container -> LxcEff m (Either Error ())
  Delete :: Container -> LxcEff m (Either Error ())
  Copy :: Container -> String -> LxcEff m (Either Error Container)
  Exec :: Container -> [String] -> LxcEff m Result

type LxcIOErr = LxcEff :+: Throw Error

start :: Has LxcIOErr sig m => Container -> m ()
start c = send (Start c) >>= liftEither

stop :: Has LxcIOErr sig m => Container -> m ()
stop c = send (Stop c) >>= liftEither

delete :: Has LxcIOErr sig m => Container -> m ()
delete c = send (Delete c) >>= liftEither

copy :: Has LxcIOErr sig m => Container -> String -> m Container
copy c name = send (Copy c name) >>= liftEither

exec :: Has LxcIOErr sig m => Container -> [String] -> m Result
exec c = send . Exec c

runLxcCommand :: [String] -> IO Result
runLxcCommand args = readProcessWithExitCode "lxc" args ""

--- Algebra definitions below

handleResult :: (ExitCode, String, String) -> Either Error String
handleResult (status, stdout, stderr) = case status of
  ExitSuccess -> return stdout
  ExitFailure code -> Left $ ContainerErr code stderr

lxcStart :: Container -> IO (Either Error ())
lxcStart c =
  void . handleResult <$> runLxcCommand ["start", name c]

lxcCopy :: Container -> String -> IO (Either Error Container)
lxcCopy c targetContainer =
  fmap (const (Container targetContainer)) . handleResult <$> runLxcCommand ["copy", name c, targetContainer]

lxcStop :: Container -> IO (Either Error ())
lxcStop c =
  void . handleResult <$> runLxcCommand ["stop", name c]

lxcDelete :: Container -> IO (Either Error ())
lxcDelete c =
  void . handleResult <$> runLxcCommand ["delete", name c]

lxcExec :: Container -> [String] -> IO Result
lxcExec c command = runLxcCommand $ ["exec", name c, "--"] ++ command

newtype LxcIOC m a = LxcIOC {runLxcIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (LxcEff :+: sig) (LxcIOC m) where
  alg hdl sig ctx = case sig of
    L (Start c) -> (<$ ctx) <$> liftIO (lxcStart c)
    L (Stop c) -> (<$ ctx) <$> liftIO (lxcStop c)
    L (Delete c) -> (<$ ctx) <$> liftIO (lxcDelete c)
    L (Exec c cmd) -> (<$ ctx) <$> liftIO (lxcExec c cmd)
    L (Copy c name) -> (<$ ctx) <$> liftIO (lxcCopy c name)
    R other -> LxcIOC (alg (runLxcIO . hdl) other ctx)
