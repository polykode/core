module Container.SocketDaemon where

import Config
import Container.Eff
import Control.Algebra
import Control.Monad (void)

daemonEnv = defaultExecEnv

removeSocketFile :: Has LxcIOErr sig m => Container -> m ()
removeSocketFile c = void $ exec c ["rm", daemonSocketFile]

runDaemon :: Has LxcIOErr sig m => Container -> m ()
runDaemon c = do
  filePush c rpcScriptPath rpcScriptTargetPath
  void $ exec c ["sh", "-c", unwords $ daemonEnv ++ ["node", rpcScriptTargetPath, "daemon"]]
