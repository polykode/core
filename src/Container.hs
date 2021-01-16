module Container where

import GHC.IO.Exception
import qualified System.LXC as LXC
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

data ContainerContext = ContainerContext
  { stdout :: Fd,
    stdin :: Fd,
    stderr :: Fd,
    container :: LXC.Container
  }

run ctx = LXC.withContainer (container ctx)

printLastError :: ContainerContext -> IO ()
printLastError c = run c LXC.getLastError >>= putStrLn . ("Error: " ++) . show . fmap LXC.prettyLXCError

printContainerState :: ContainerContext -> IO ()
printContainerState c = run c LXC.state >>= putStrLn . ("State: " ++) . show

waitForStartup :: ContainerContext -> IO Bool
waitForStartup c = run c $ LXC.wait LXC.ContainerRunning (-1)

launch ctx = do
  created <- run ctx $ LXC.create "download" Nothing Nothing [] ["-d", "ubuntu", "-r", "trusty", "-a", "amd64"]
  started <- run ctx $ LXC.start False []
  return ()

cleanup ctx = do
  run ctx LXC.stop
  run ctx LXC.destroy
  closeFd (stdout ctx)
  closeFd (stdin ctx)
  closeFd (stderr ctx)

runCommand :: ContainerContext -> String -> [String] -> IO (Maybe ExitCode)
runCommand ctx cmd args =
  run ctx $ LXC.attachRunWait attachOptions cmd (cmd : args)
  where
    attachOptions =
      LXC.defaultAttachOptions
        { LXC.attachExtraEnvVars =
            [ "PATH=$PATH:/bin:/usr/bin:/usr/local/bin"
            ],
          LXC.attachStdoutFD = stdout ctx,
          LXC.attachStdinFD = stdin ctx
        }

createContext :: IO ContainerContext
createContext = do
  stdout <- createFile "/tmp/polykode--tmp-stdout.000" stdFileMode
  stdin <- createFile "/tmp/polykode--tmp-stdin.000" stdFileMode
  stderr <- createFile "/tmp/polykode--tmp-stderr.000" stdFileMode
  return
    ContainerContext
      { stdout = stdout,
        stdin = stdin,
        stderr = stderr,
        container = LXC.Container "testicles" Nothing
      }
