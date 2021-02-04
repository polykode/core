{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module EffectMock where

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
    L (Exec c cmd) -> (<$ ctx) <$> liftIO (pure (ExitSuccess, "Executing (" ++ name c ++ "): " ++ unwords cmd, ""))
    L (Copy c name) -> (<$ ctx) <$> liftIO (pure . Right . Container . ("copied:" ++) $ name)
    R other -> LxcIOC (alg (runMockLxcIO . hdl) other ctx)

runMock = runMockLxcIO . runThrow
