{-# LANGUAGE QuasiQuotes #-}

module CodeExecutor.Langs.Bash.Templates where

import Config
import Text.RawString.QQ (r)

var k value = "local " ++ k ++ "=" ++ "'" ++ value ++ "'; "

contextTemplate :: String -> String -> String
contextTemplate execId code =
  [r|
context() {
  |]
    ++ var "execId" execId
    ++ var "baseUrl" serverBaseUrl
    ++ [r|
  case "$#" in
    1) curl "${baseUrl}/uffi/var/$1?exec_id=${execId}" 2>/dev/null | jq -r ".value" ;;
    2) curl -X POST "${baseUrl}/uffi/var/$1?exec_id=${execId}" -d "value=$2" >/dev/null 2>&1 ;;
    *) echo "Invalid set of arguments passed to 'context'";
  esac
}
|]
    ++ code

importsTemplate :: String -> String -> String
importsTemplate execId code = code
