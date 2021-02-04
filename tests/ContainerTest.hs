{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ContainerTest where

import Container.Eff
import Control.Algebra
import Control.Carrier.Throw.Either
import Control.Monad.IO.Class
import GHC.IO.Exception
import Test.Hspec
import Text.RawString.QQ

newtype LxcIOC m a = LxcIOC {runMockLxcIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (LxcEff :+: sig) (LxcIOC m) where
  alg hdl sig ctx = case sig of
    L (Start c) -> (<$ ctx) <$> liftIO (pure $ Right ())
    L (Stop c) -> (<$ ctx) <$> liftIO (pure $ Right ())
    L (Delete c) -> (<$ ctx) <$> liftIO (pure $ Right ())
    L (Exec c cmd) -> (<$ ctx) <$> liftIO (pure (ExitSuccess, "Executing: " ++ unwords cmd, ""))
    L (Copy c name) -> (<$ ctx) <$> liftIO (pure . Right . Container . ("copied:" ++) $ name)
    R other -> LxcIOC (alg (runMockLxcIO . hdl) other ctx)

foldWithDefault def = \case
  Right d -> d
  Left e -> def e

runMock = runMockLxcIO . runThrow

tests = describe "Container" $ do
  describe "copy" $ do
    container <-
      let mockProgram :: IO (Either Error Container)
          mockProgram = runMock $ copy (Container "foobar") "test"
       in runIO mockProgram
    it "should return the executed command string with success" $ do
      container `shouldBe` Right (Container "copied:test")

  describe "exec" $ do
    result <-
      let mockProgram :: IO (Either Error Result)
          mockProgram = runMock $ exec (Container "foobar") ["echo", "1"]
       in runIO mockProgram
    it "should return the executed command string with success" $ do
      result `shouldBe` Right (ExitSuccess, "Executing: echo 1", "")
