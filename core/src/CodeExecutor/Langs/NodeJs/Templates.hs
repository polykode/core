{-# LANGUAGE QuasiQuotes #-}

module CodeExecutor.Langs.NodeJs.Templates where

import Config
--import Data.String.Interpolate (i)
import Text.RawString.QQ (r)

var k value = "const " ++ k ++ " = " ++ "'" ++ value ++ "'; "

contextTemplate :: String -> String -> String
contextTemplate execId code =
  [r|
const { context } = (() => {
  const proc = require('child_process');
  |]
    ++ var "execId" execId
    ++ var "baseUrl" serverBaseUrl
    ++ [r|

  const call = (action, d) => {
    const data = { ...d, action, exec_id: execId };
    const { stdout, stderr } = proc.spawnSync('node', [
      '/opt/scripts/rpc.js',
      'send',
      '--data', JSON.stringify(data),
    ]);

    const output = stdout.toString();
    return output ? JSON.parse(output) : {};
  };

  const context = new Proxy({}, {
    get: (_, key) => call('ctx/get', { name: key }).value,
    set: (_, key, value) => void call('ctx/put', { name: key, value }),
  });

  return { context };
})();
|]
    ++ code

importsTemplate :: String -> String -> String
importsTemplate _execId code = code
