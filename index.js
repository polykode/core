const R = require('ramda');
const net = require('net');
const http = require('http');
const marked = require('marked');
const { readFileSync } = require('fs');

const serial = fn => xs => xs.reduce((acc, x) => acc.then(_ => fn(x)), Promise.resolve())

const config = { port: 44931 };

const environment = {
  context: {},
  args: {}, // TODO
};

// Use tcp socket instead?
const server = http.createServer((req, res) => {
  if (req.method !== 'POST') return res.end();

  let data = '';
  req.on('data', chunk => (data += chunk.toString()));
  req.on('end', () => {
    try {
      const json = JSON.parse(data);
      Object.assign(environment.context, json);
    } catch (e) {}
  });
  res.end('wow');
});

const languages = require('./adapters');
const runCodeBlock = ({ lang, text }) => {
  const adapter = languages[lang];
  // NoAdapter error here
  const options = {
    config,
    getEnv: async () => environment,
  };
  return adapter(options).execute(text);
};

const runAction = R.cond([
  [R.propEq('type', 'code'), async block => {
    console.log('```', block.lang);
    await runCodeBlock(block);
    console.log('```');
  }],
  [R.propEq('type', 'heading'), async ({ raw }) => console.log(`\n${raw}`)],
  [R.T, async () => {}],
])

const run = serial(runAction);

server.listen(config.port, () => {
  const content = readFileSync('./examples/simple.md', 'utf-8');
  const md = marked.lexer(content);

  run(md)
    .then(() => server.close())
    .catch(_ => {
      server.close();
      process.exit(1);
    });
});

