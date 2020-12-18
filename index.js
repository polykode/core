const R = require('ramda');
const net = require('net');
const http = require('http');
const marked = require('marked');
const { readFileSync } = require('fs');

const serial = fn => xs => xs.reduce((acc, x) => acc.then(_ => fn(x)), Promise.resolve())

const environment = {
  port: 44931,
  context: {},
  args: {}, // TODO
};

const languages = require('./adapters');
const runCodeBlock = ({ lang, text }) => {
  const adapter = languages[lang];
  // NoAdapter error here
  return adapter(environment).execute(text);
};

// Use tcp socket instead?
const server = http.createServer((req, res) => {
  if (req.method !== 'POST') return res.end();

  let data = '';
  req.on('data', chunk => (data += chunk.toString()));
  req.on('end', () => {
    const json = JSON.parse(data);
    Object.assign(environment.context, json);
  });
  res.end('wow');
});

const run = R.compose(
  serial(runCodeBlock),
  R.filter(R.propEq('type', 'code')),
  marked.lexer,
);

server.listen(environment.port, () => {
  const md = readFileSync('./examples/simple.md', 'utf-8');

  run(md)
    .then(() => server.close())
    .catch(_ => {
      server.close();
      process.exit(1);
    });
});

