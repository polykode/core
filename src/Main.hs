module Main where

--import Evaluator

import Container
import GHC.IO.Exception
import Parser
import System.LXC
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

main :: IO ()
main = do
  putStrLn "---------------------------------------"
  ctx <- createContext
  launch ctx
  printContainerState ctx

  -- Wait for startup
  waitForStartup ctx

  result <- runCommand ctx "sh" ["-c", "echo 'Hello, world!'"]

  putStrLn $ "Result?: " ++ show result

  cleanup ctx
  putStrLn "---------------------------------------"
