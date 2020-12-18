const R = require('ramda');
const { spawn } = require('child_process');

const then = fn => p => p.then(fn);

const spawnProcess = (cmd, args = [], input) => {
  const ps = spawn(cmd, args);

  ps.stdout.pipe(process.stdout);
  ps.stderr.pipe(process.stderr);
  if (input) {
    ps.stdin.write(input);
    ps.stdin.end();
  }

  return new Promise((res, rej) => {
    ps.on('close', status => status !== 1 ? res() : rej());
    ps.on('error', err => rej(err));
  });
};

const Adapter = ({ evaluate, wrap }) => ({
  execute: R.compose(then(evaluate), wrap),
});

const NodeAdapter = ({ config, getEnv }) => Adapter({
  evaluate: code => spawnProcess('node', ['-'], code),
  wrap: async code => {
    const env = await getEnv();
    return `
      const { args, context } = ${JSON.stringify(env)};
      const codeBlock = () => {
        ${code}
      };
      const result = codeBlock() || {};
      require('node-fetch')('http://127.0.0.1:${config.port}/?lang=javascript', {
        method: 'POST',
        body: JSON.stringify(result),
      })
    `.trim();
  },
});

const PythonAdapter = ({ config, getEnv }) => Adapter({
  evaluate: code => spawnProcess('python', ['-c', code]),
  wrap: async code => {
    const env = await getEnv();
    const envString = JSON.stringify(env);
    return `
import json
import requests

__env = json.loads('${envString}')
args = __env['args']
context = __env['context']

def codeBlock():
  ${code.split('\n').join('\n  ')}

result = codeBlock()
requests.post('http://127.0.0.1:${config.port}/?lang=python', data=json.dumps(result))
`.trim()
  },
});

const BashAdapter = ({ config, getEnv }) => Adapter({
  evaluate: code => spawnProcess('bash', ['-c', code]),
  wrap: async code => {
    const env = await getEnv();
    return `
      args_json='${JSON.stringify(env.args)}';
      context_json='${JSON.stringify(env.context)}';

      get_arg() { echo "$args_json" | jq "$1" 2>/dev/null; }
      get_ctx() { echo "$context_json" | jq "$1" 2>/dev/null; }

      return_ctx() {
        local json="{ ";
        while [[ $# -gt 0 ]]; do
          key="$1"; value="$2";
          shift 2;
          json="$json\\"$key\\": \\"$value\\"";

          [[ $# -gt 0 ]] && json="$json,";
        done;
        json="$json }";

        curl 'http://127.0.0.1:${config.port}/?lang=javascript' -X POST -d "$json" >/dev/null 2>&1;
      }

      ${code}
    `.trim();
  },
});

module.exports = {
  javascript: NodeAdapter,
  js: NodeAdapter,
  python: PythonAdapter,
  bash: BashAdapter,
};
