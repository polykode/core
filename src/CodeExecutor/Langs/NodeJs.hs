{-# LANGUAGE QuasiQuotes #-}

module CodeExecutor.Langs.NodeJs where

import CodeExecutor.Utils
import Container.Eff
import Control.Algebra
import Control.Monad.IO.Class
import GHC.IO.Exception
import Text.RawString.QQ

-- Add host pointing to this in hostfile of the template container
wrap execId code =
  [r|
const { context } = (() => {
  const fetch = require('node-fetch');
  const qs = require('querystring');

  const execId = "|]
    ++ execId
    ++ [r|";
  const baseUrl = "http://10.118.192.132:3000";

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

run :: Has LxcIOErr sig m => String -> Container -> String -> m Result
run execId c code = exec c ["node", "-e", wrap execId code]
