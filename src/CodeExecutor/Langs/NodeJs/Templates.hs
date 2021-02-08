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
  const fetch = require('node-fetch');
  const qs = require('querystring');

  |]
    ++ var "execId" execId
    ++ var "baseUrl" serverBaseUrl
    ++ [r|
  const readValue = key =>
    fetch(`${baseUrl}/uffi/variable/${key}?exec_id=${execId}`)
      .then(r => r.json()).then(data => data.value);

  const setValue = (key, value) => {
    const valueString = typeof value === 'string' ? value : JSON.stringify(value);
    return fetch(`${baseUrl}/uffi/variable/${key}?exec_id=${execId}`, {
      method: 'POST',
      headers: {
        'X-Exec-Id': execId,
        'Content-Type': 'application/x-www-form-urlencoded',
      },
      body: qs.stringify({ value: valueString }),
    });
  };

  const context = new Proxy({}, {
    get: (_, k) => readValue(k),
    set: (_, k, value) => setValue(k, value),
  });

  return { context };
})();
|]
    ++ code

importsTemplate :: String -> String -> String
importsTemplate execId code = code
