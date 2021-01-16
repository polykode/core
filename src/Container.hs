module Container where

--import Control.Monad.Trans.State

import Control.Concurrent
import Control.Monad.Trans.Except
import qualified Data.Text as T
import GHC.IO.Exception
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import qualified System.LXC as LXC
import System.Posix.Files
import System.Posix.IO

data ContainerContext = ContainerContext
  { outputFile :: FilePath,
    inputFile :: FilePath,
    errorFile :: FilePath,
    container :: LXC.Container
  }

data Error = ContainerErr String | RunErr String deriving (Show)

type IOErr = ExceptT Error IO

type Result = IOErr (ExitCode, T.Text, T.Text)

liftIO :: IO a -> ExceptT e IO a
liftIO = ExceptT . fmap Right

run ctx = liftIO . LXC.withContainer (container ctx)

printLastError :: ContainerContext -> IOErr ()
printLastError c = run c LXC.getLastError >>= liftIO . putStrLn . ("Error: " ++) . show . fmap LXC.prettyLXCError

printContainerState :: ContainerContext -> IOErr ()
printContainerState c = run c LXC.state >>= liftIO . putStrLn . ("State: " ++) . show

waitForStartup :: ContainerContext -> IOErr Bool
waitForStartup c = run c $ LXC.wait LXC.ContainerRunning (-1)

launch ctx = do
  created <- run ctx $ LXC.create "download" Nothing Nothing [] ["-d", "ubuntu", "-r", "trusty", "-a", "amd64"]
  started <- run ctx $ LXC.start False []
  return ()

cleanup ctx = do
  run ctx $ LXC.stop >> LXC.destroy
  --liftIO $ do
  --  hClose (outputHandle ctx)
  --  hClose (inputHandle ctx)
  --  hClose (errorHandle ctx)
  return ()

runCommand :: ContainerContext -> String -> [String] -> Result
runCommand ctx cmd args = do
  options <- attachOptions
  exitState <- run ctx $ LXC.attachRunWait options cmd (cmd : args)
  case exitState of
    Nothing -> except . Left $ RunErr "Command not executed"
    Just code -> liftIO $ do
      --liftIO $ hShow ((`openFile` ReadWriteMode) . outputFile $ ctx) >>= putStrLn
      outF <- openFile (outputFile ctx) ReadWriteMode
      errF <- openFile (errorFile ctx) ReadWriteMode
      (outStr, errStr) <- readContents outF errF
      -- TODO: Handle ExitFailure
      hClose outF
      hClose errF
      return (code, outStr, errStr)
  where
    readContents outF errF = do
      outStr <- hGetContents outF
      errStr <- hGetContents errF
      pure (T.pack outStr, T.pack errStr)
    openFd f = createFile f (unionFileModes namedPipeMode stdFileMode)
    attachOptions = do
      outFd <- liftIO . openFd . outputFile $ ctx
      return $
        LXC.defaultAttachOptions
          { LXC.attachExtraEnvVars = ["PATH=$PATH:/bin:/usr/bin:/usr/local/bin"],
            LXC.attachStdoutFD = outFd
            --LXC.attachStdinFD = inputHandle ctx
          }

createContext :: String -> IOErr ContainerContext
createContext name =
  let mode = unionFileModes namedPipeMode stdFileMode
      filepath s = "/tmp/polykode--" ++ name ++ "--" ++ s ++ ".000"
   in --file s = liftIO $ openFile (filepath s) ReadWriteMode
      do
        --outputHandle <- file "stdout"
        --inputHandle <- file "stdin"
        --errorHandle <- file "stderr"
        return
          ContainerContext
            { outputFile = filepath "stdout",
              inputFile = filepath "stdin",
              errorFile = filepath "stderr",
              container = LXC.Container name Nothing
            }
