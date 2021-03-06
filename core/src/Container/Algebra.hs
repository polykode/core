{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Container.Algebra where

import Container.Eff
import Control.Algebra
import Control.Carrier.Throw.Either
import Control.Monad (void)
import Control.Monad.IO.Class
import Debug.Trace
import GHC.IO.Exception
import System.IO
import System.Process

runLxcCommand :: [String] -> IO Result
runLxcCommand args = readProcessWithExitCode "lxc" args ""

handleResult :: (ExitCode, String, String) -> Either Error String
handleResult (status, stdout, stderr) = case status of
  ExitSuccess -> return stdout
  ExitFailure code -> Left $ ContainerErr code stderr

lxcStart :: Container -> IO (Either Error ())
lxcStart c =
  trace ("Started " ++ name c)
    . void
    . handleResult
    <$> runLxcCommand ["start", name c]

lxcCopy :: Container -> String -> IO (Either Error Container)
lxcCopy c targetContainer =
  trace ("Created " ++ targetContainer)
    . fmap (const (Container targetContainer))
    . handleResult
    <$> runLxcCommand ["copy", name c, targetContainer]

lxcStop :: Container -> IO (Either Error ())
lxcStop c =
  trace ("Stopped " ++ name c)
    . void
    . handleResult
    <$> runLxcCommand ["stop", name c]

lxcDelete :: Container -> IO (Either Error ())
lxcDelete c =
  trace ("Deleted " ++ name c)
    . void
    . handleResult
    <$> runLxcCommand ["delete", name c]

lxcExec :: Container -> [String] -> IO Result
lxcExec c command = runLxcCommand $ ["exec", name c, "--"] ++ command

lxcInfo :: Container -> IO (Either Error String)
lxcInfo c = handleResult <$> runLxcCommand ["info", name c]

lxcFilePush :: Container -> String -> String -> IO ()
lxcFilePush c f1 f2 =
  trace ("Moved file from " ++ f1 ++ " to " ++ f2)
    . void
    $ runLxcCommand ["file", "push", f1, name c ++ f2, "-p"]

localWriteFile :: String -> String -> IO ()
localWriteFile f contents = do
  putStrLn ("Writing to file " ++ f)
  handle <- openFile f WriteMode
  hPutStr handle contents
  hClose handle
  return ()

newtype LxcIOC m a = LxcIOC {runLxcIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (LxcEff :+: sig) (LxcIOC m) where
  alg hdl sig ctx = case sig of
    L (Start c) -> (<$ ctx) <$> liftIO (lxcStart c)
    L (Stop c) -> (<$ ctx) <$> liftIO (lxcStop c)
    L (Delete c) -> (<$ ctx) <$> liftIO (lxcDelete c)
    L (Exec c cmd) -> (<$ ctx) <$> liftIO (lxcExec c cmd)
    L (Copy c name) -> (<$ ctx) <$> liftIO (lxcCopy c name)
    L (Info c) -> (<$ ctx) <$> liftIO (lxcInfo c)
    R other -> LxcIOC (alg (runLxcIO . hdl) other ctx)

newtype FileIOC m a = FileIOC {runFileIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (FileIOEff :+: sig) (FileIOC m) where
  alg hdl sig ctx = case sig of
    L (CreateLocalFile fp contents) -> (<$ ctx) <$> liftIO (localWriteFile fp contents)
    L (FilePush c f1 f2) -> (<$ ctx) <$> liftIO (lxcFilePush c f1 f2)
    R other -> FileIOC (alg (runFileIO . hdl) other ctx)

withLxc :: ThrowC e (LxcIOC (FileIOC m)) a -> m (Either e a)
withLxc = runFileIO . runLxcIO . runThrow
