{-# LANGUAGE QuasiQuotes #-}

module CodeExecutor.Langs.Bash where

import CodeExecutor.Utils
import Container.Eff
import Control.Algebra
import Control.Monad.IO.Class
import GHC.IO.Exception
import Text.RawString.QQ

wrap execId code =
  [r|
context() {
  local execId="|]
    ++ execId
    ++ [r|";
  local baseUrl="http://10.118.192.132:3000";

  case "$#" in
    1) curl "$baseUrl/uffi/variable/$1?exec_id=$execId" 2>/dev/null | jq ".value" ;;
    2) curl -X POST "$baseUrl/uffi/variable/$1?exec_id=$execId" -d "value=$2" >/dev/null 2>&1 ;;
    *) echo "Invalid set of arguments passed to 'context'";
  esac
}
|]
    ++ code

run :: Has LxcIOErr sig m => String -> Container -> String -> m Result
run execId c code = exec c ["bash", "-c", wrap execId code]
