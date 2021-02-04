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
import Container.Pool
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
    L (Exec c cmd) -> (<$ ctx) <$> liftIO (pure (ExitSuccess, "Executing (" ++ name c ++ "): " ++ unwords cmd, ""))
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
    it "should return the copied container" $ do
      container `shouldBe` Right (Container "copied:test")

  describe "exec" $ do
    result <-
      let mockProgram :: IO (Either Error Result)
          mockProgram = runMock $ exec (Container "foobar") ["echo", "1"]
       in runIO mockProgram
    it "should return the executed command string with success" $ do
      result `shouldBe` Right (ExitSuccess, "Executing (foobar): echo 1", "")

  describe "Pool" $ do
    describe "createContainerPool" $ do
      pool <-
        let mockProgram :: IO (Either Error ContainerPool)
            mockProgram = runMock $ createContainerPool 3
         in runIO mockProgram
      it "should return a pool of containers" $ do
        pool `shouldBe` Right [Container "container--1", Container "container--2", Container "container--3"]

    describe "executeCommand" $ do
      pool <-
        let mockProgram :: IO (Either Error Result)
            mockProgram = runMock $ do
              pool <- createContainerPool 10
              executeCommand pool ["echo", "1"]
         in runIO mockProgram
      it "should execute the command in the first container" $ do
        pool `shouldBe` Right (ExitSuccess, "Executing (container--1): echo 1", "")

--
--
--
--
--
