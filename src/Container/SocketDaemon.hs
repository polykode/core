module Container.SocketDaemon where

import Config
import Container.Eff
import Control.Algebra
import Control.Monad (void)

removeSocketFile :: Has LxcIOErr sig m => Container -> m ()
removeSocketFile c = void $ exec c ["rm", daemonSocketFile]

runDaemon :: Has LxcIOErr sig m => Container -> m ()
runDaemon c = do
  filePush c rpcScriptPath rpcScriptTargetPath
  void $ exec c ["sh", "-c", unwords $ defaultExecEnv ++ ["node", rpcScriptTargetPath, "daemon"]]
