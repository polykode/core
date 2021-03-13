{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TestUtils.EffectMock where

import Container.Eff
import Container.Pool
import Control.Algebra
import Control.Carrier.Throw.Either
import Control.Monad.IO.Class
import GHC.IO.Exception

newtype LxcIOC m a = LxcIOC {runMockLxcIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (LxcEff :+: sig) (LxcIOC m) where
  alg hdl sig ctx = case sig of
    L (Start c) -> (<$ ctx) <$> liftIO (pure $ Right ())
    L (Stop c) -> (<$ ctx) <$> liftIO (pure $ Right ())
    L (Delete c) -> (<$ ctx) <$> liftIO (pure $ Right ())
    L (Exec c cmd) -> (<$ ctx) <$> liftIO (pure (ExitSuccess, "Executing (" ++ name c ++ ")", unwords cmd))
    L (Copy c name) -> (<$ ctx) <$> liftIO (pure . Right . Container . ("copied:" ++) $ name)
    L (Info c) -> (<$ ctx) <$> liftIO (pure . Right $ "info")
    R other -> LxcIOC (alg (runMockLxcIO . hdl) other ctx)

newtype FileIOC m a = FileIOC {runMockFileIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (FileIOEff :+: sig) (FileIOC m) where
  alg hdl sig ctx = case sig of
    L (CreateLocalFile fp contents) -> return $ () <$ ctx
    L (FilePush c f1 f2) -> return $ () <$ ctx
    R other -> FileIOC (alg (runMockFileIO . hdl) other ctx)

runMock = runMockFileIO . runMockLxcIO . runThrow
