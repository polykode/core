const R = require('ramda');
const { spawn } = require('child_process');

// TODO
const spawnProcess = (cmd, args = [], input) => {
  const ps = spawn(cmd, args);

  ps.stdout.pipe(process.stdout);
  ps.stderr.pipe(process.stderr);
  if (input) {
    ps.stdin.write(input);
    ps.stdin.end();
  }

  return new Promise(res => {
    ps.on('close', status => status !== 1 ? res() : rej());
    ps.on('error', err => rej(err));
  });
};

const Adapter = ({ evaluate, wrap }) => ({
  execute: R.compose(evaluate, wrap),
});

const NodeAdapter = env => Adapter({
  evaluate: code => spawnProcess('node', ['-'], code),
  wrap: code => `
    const { args, context } = ${JSON.stringify(env)};
    const codeBlock = () => {
      ${code}
    };
    const result = codeBlock() || {};
    require('node-fetch')('http://127.0.0.1:${env.port}/?lang=javascript', {
      method: 'POST',
      body: JSON.stringify(result),
    })
    // Push to server
  `,
});

const PythonAdapter = env => Adapter({
  evaluate: code => spawnProcess('python', ['-c', code]),
  wrap: code => `
args = {}
context = {}

def codeBlock():
  ${code.split('\n').join('\n  ')}

result = codeBlock()
`.trim(),
});


module.exports = {
  javascript: NodeAdapter,
  js: NodeAdapter,
  python: PythonAdapter,
};
