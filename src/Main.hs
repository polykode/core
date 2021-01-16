{-# LANGUAGE LambdaCase #-}

module Main where

--import Evaluator

import Container
import Control.Applicative
import Control.Monad.Trans.Except
import Evaluator

runIOErr :: a -> IOErr a -> IO a
runIOErr def m = do
  runExceptT m >>= \case
    Right x -> pure x
    Left e -> do
      putStrLn "Fuck"
      print e
      pure def

executeCommand container = do
  liftIO . putStrLn $ "---------------------------------------"
  launch container
  printContainerState container

  -- Wait for startup
  waitForStartup container

  result <- runCommand container "sh" ["-c", "echo 'Hello, world!'"]

  liftIO . putStrLn $ "Result?: " ++ show result

  liftIO . putStrLn $ "---------------------------------------"

main :: IO ()
main = runIOErr () $ do
  container <- createContext "fuckoff"
  liftIO $ runIOErr () (executeCommand container) >> runIOErr () (cleanup container)
