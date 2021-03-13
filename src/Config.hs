module Config where

version = "0.0.0"

containerPoolSize = 3 :: Int

serverHost = "core.polykode.local"

serverPort = 3000 :: Int

serverBaseUrl = "ws://" ++ serverHost ++ ":" ++ show serverPort

defaultExecEnv = ["NODE_PATH=./node_modules:/usr/local/lib/node_modules"]

daemonSocketFile = "/tmp/polykode-core-rpc.sock"

rpcScriptPath = "/opt/code/scripts/rpc.js"

rpcScriptTargetPath = "/opt/scripts/rpc.js"
