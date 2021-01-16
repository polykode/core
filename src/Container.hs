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

ifM :: (Monad m) => m Bool -> a -> m a -> m a
ifM predicate def m = do
  check <- predicate
  if check then pure def else m

failWith = except . Left

containerArgs = ["-d", "ubuntu", "-r", "trusty", "-a", "amd64"]

launch :: ContainerContext -> IOErr ()
launch ctx = do
  created <-
    run ctx $
      ifM LXC.isDefined True $
        LXC.create "download" Nothing Nothing [LXC.CreateQuiet] containerArgs
  started <- run ctx $ ifM LXC.isRunning True (LXC.start False [])
  if not created
    then failWith $ ContainerErr "Unable to create container"
    else
      if not started
        then failWith $ ContainerErr "Unable to start container"
        else pure ()

cleanup :: ContainerContext -> IOErr ()
cleanup ctx = do
  liftIO . putStrLn $ "Cleanup crew has arrived"
  run ctx $ LXC.stop >> LXC.destroy
  return ()

runCommand :: ContainerContext -> String -> [String] -> Result
runCommand ctx cmd args = do
  options <- attachOptions
  exitState <- run ctx $ LXC.attachRunWait options cmd (cmd : args)
  case exitState of
    Nothing -> except . Left $ RunErr "Command not executed"
    Just code -> liftIO $ do
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
      errFd <- liftIO . openFd . errorFile $ ctx
      return $
        LXC.defaultAttachOptions
          { LXC.attachStdoutFD = outFd,
            LXC.attachStderrFD = errFd,
            LXC.attachExtraEnvVars = ["PATH=$PATH:/bin:/usr/bin:/usr/local/bin"]
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
