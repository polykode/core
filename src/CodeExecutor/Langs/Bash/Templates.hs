{-# LANGUAGE QuasiQuotes #-}

module CodeExecutor.Langs.Bash.Templates where

import Config
import Text.RawString.QQ (r)

var k value = "local " ++ k ++ "=" ++ "'" ++ value ++ "'; "

contextTemplate :: String -> String -> String
contextTemplate execId code =
  [r|
__call() { node /opt/scripts/rpc.js send --data "$1" 2>/dev/null; }

context() {
  |]
    ++ var "execId" execId
    ++ var "baseUrl" serverBaseUrl
    ++ [r|
  case "$#" in
    1) __call "{ \"action\": \"ctx/get\", \"exec_id\": \"$execId\", \"name\": \"$1\" }" | jq -r '.value' ;;
    2) __call "{ \"action\": \"ctx/put\", \"exec_id\": \"$execId\", \"name\": \"$1\", \"value\": \"$2\" }" >/dev/null ;;
    *) echo "Invalid set of arguments passed to 'context'";
  esac
}
|]
    ++ code

importsTemplate :: String -> String -> String
importsTemplate execId code = code
