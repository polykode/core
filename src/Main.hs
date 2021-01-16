module Main where

--import Evaluator

import GHC.IO.Exception
import Parser
import System.LXC
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

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

data ContainerContext = ContainerContext
  { stdout :: Fd,
    stdin :: Fd
  }

runCommand :: ContainerContext -> String -> [String] -> IO (Maybe ExitCode)
runCommand context cmd args =
  lxc $ attachRunWait attachOptions cmd (cmd : args)
  where
    attachOptions =
      defaultAttachOptions
        { attachExtraEnvVars = ["PATH=$PATH:/bin:/usr/bin:/usr/local/bin"],
          attachStdoutFD = stdout context,
          attachStdinFD = stdin context
        }

main :: IO ()
main = do
  putStrLn "---------------------------------------"
  createC
  startC
  printContainerState

  -- Wait for startup
  lxc $ wait ContainerRunning (-1)

  stdout <- createFile "/tmp/polykode--tmp-stdout.000" stdFileMode
  stdin <- createFile "/tmp/polykode--tmp-stdin.000" stdFileMode

  result <- runCommand (ContainerContext {stdout = stdout, stdin = stdin}) "sh" ["-c", "echo 'Hello, world!'"]
  putStrLn $ "Result?: " ++ show result

  printLastError

  closeFd stdout
  closeFd stdin
  cleanup
  putStrLn "---------------------------------------"
