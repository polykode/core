module Main where

--import Evaluator
import Parser
import System.LXC

lxc = withContainer (Container "trusty" Nothing)

printLastError = lxc getLastError >>= putStrLn . ("Error: " ++) . show . fmap prettyLXCError

printContainerState = lxc state >>= putStrLn . ("State: " ++) . show

createC = do
  --exists <- lxc isDefined
  created <- lxc $ create "download" Nothing Nothing [] ["-d", "ubuntu", "-r", "trusty", "-a", "amd64"]
  putStrLn $ "Created?: " ++ show created

startC = do
  started <- lxc $ start False []
  putStrLn $ "Started?: " ++ show started

cleanup = lxc stop >> lxc destroy

attachOptions =
  defaultAttachOptions
    { attachExtraEnvVars = ["PATH=$PATH:/bin:/usr/bin:/usr/local/bin"]
    }

main :: IO ()
main = do
  putStrLn "---------------------------------------"
  createC
  startC
  printContainerState

  -- Wait for startup
  lxc $ wait ContainerRunning (-1)

  print attachOptions

  result <- lxc $ attachRunWait attachOptions "sh" ["sh", "-c", "echo 'Hello, world!'"]
  putStrLn $ "Result?: " ++ show result

  printLastError

  cleanup
  putStrLn "---------------------------------------"
