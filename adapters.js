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


module.exports = {
  javascript: NodeAdapter,
  js: NodeAdapter,
  python: PythonAdapter,
};
