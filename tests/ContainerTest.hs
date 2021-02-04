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
import EffectMock
import GHC.IO.Exception
import Test.Hspec
import Text.RawString.QQ

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

--
--
--
--
--
