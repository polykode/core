{-# LANGUAGE LambdaCase #-}

module Main where

--import Evaluator

import Container
import Control.Monad.Trans.Except
import Evaluator

runIOErr :: IOErr () -> IO ()
runIOErr m =
  runExceptT m >>= \case
    Right _ -> pure ()
    Left e -> do
      putStrLn "Fuck"
      print e

main :: IO ()
main = runIOErr $ do
  liftIO . putStrLn $ "---------------------------------------"
  container <- createContext "fuckoff"
  launch container
  printContainerState container

  -- Wait for startup
  waitForStartup container

  result <- runCommand container "sh" ["-c", "echo 'Hello, world!'"]

  liftIO . putStrLn $ "Result?: " ++ show result

  cleanup container
  liftIO . putStrLn $ "---------------------------------------"
