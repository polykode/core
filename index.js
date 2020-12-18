const R = require('ramda');
const marked = require('marked');
const { readFileSync } = require('fs');
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

const NodeAdapter = Adapter({
  evaluate: code => spawnProcess('node', ['-'], code),
  wrap: code => `
    const args = {}; // TODO
    const context = {}; // TODO
    const codeBlock = () => {
      ${code}
    };
    const result = codeBlock();
    // Push to server
  `,
});

const PythonAdapter = Adapter({
  evaluate: code => spawnProcess('python', ['-c', code]),
  wrap: code => `
args = {}
context = {}

def codeBlock():
  ${code.split('\n').join('\n  ')}

result = codeBlock()
`.trim(),
});


const languages = {
  javascript: NodeAdapter,
  js: NodeAdapter,
  python: PythonAdapter,
};

const runCodeBlock = ({ lang, text }) => {
  const adapter = languages[lang];
  // NoAdapter error here
  return adapter.execute(text);
};

const serial = fn => xs => xs.reduce((acc, x) => acc.then(_ => fn(x)), Promise.resolve())

const run = R.compose(serial(runCodeBlock), R.filter(R.propEq('type', 'code')), marked.lexer);

const md = readFileSync("./examples/simple.md", "utf-8");
run(md)
  .catch(_ => process.exit(1));

