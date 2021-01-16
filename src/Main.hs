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

runInContainer fn container =
  runIOErr () (fn container) >> runIOErr () (cleanup container)

loadMdFile container = do
  mdStr <- liftIO $ readFile "./examples/serial.md"
  result <- evaluate container mdStr
  liftIO $ print result
  return ()

main :: IO ()
main = runIOErr () $ do
  container <- createContext "fuckoff"
  liftIO $ runInContainer loadMdFile container
