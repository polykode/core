const fs = require('fs');
const net = require('net');
const Websocket = require('ws');

const main = () => {
  const channel = Channel('/tmp/polykode-core-rpc.sock');

  Cli(process.argv.slice(2), {
    daemon: {
      action: () => {
        const ws = new Websocket('ws://core.polykode.local:3000');

        const fserver = channel.startServer(data => ws.send(data));
        ws.on('message', fserver.sendData);
        ws.on('close', fserver.close);
      },
    },
    send: {
      options: ['data'],
      action: ({ data }) => {
        const fconn = channel.connect(d => client.onData(JSON.parse(d)));
        const client = RpcClient({ send: m => fconn.sendData(JSON.stringify(m)) });
        client.sendData(JSON.parse(data))
          .then(JSON.stringify)
          .then(console.log)
          .then(d => (fconn.close(), d))
          .catch(e => console.error(e)); // TODO: Handle errors better
      },
    },
  });
};

const genId = () => {
  let newid = () => Math.round(Math.random() * 1e8).toString(16).padEnd(8, 'f');
  return newid() + newid() + newid() + newid();
};

const RpcClient = ({ send: sendReq }) => {
  let listeners = {};

  const onData = m => {
    if (listeners[m.id]) {
      listeners[m.id].forEach(fn => fn(m));
      listeners[m.id] = undefined;
    }
  };

  const addListener = (id, f) => (listeners[id] = (listeners[id] || []).concat([f]));

  return {
    onData: onData,
    sendData: (reqData = {}) => {
      const id = genId();
      return new Promise((res, _rej) => {
        reqData.beacon ? res({ id }) : addListener(id, res);
        sendReq({ id, ...reqData });
      });
    },
  };
};

const Channel = (socketPath) => {
  const isRunning = () => fs.existsSync(socketPath);
  const deleteServer = () => fs.rmSync(socketPath);

  const startServer = (onData) => {
    if (isRunning())
      deleteServer();

    let clients = [];
    const server = net.createServer(stream => {
      clients = clients.concat([stream]);
      stream.on('data', d => onData(d.toString()));
    });
    server.listen(socketPath);

    const broadcast = data => clients.forEach(c => c.write(data));

    return { sendData: broadcast, close: () => server.stop() };
  };

  const connect = (onData) => {
    const client = net.connect(socketPath);
    client.on('data', d => onData(d.toString()));
    return { sendData: d => client.write(d), close: () => client.destroy() };
  };

  return { isRunning, startServer, connect };
};

const Cli = ([command, ...args], pattern) => {
  const argsPattern = pattern[command];
  if (!argsPattern)
    throw new Error (`Invalid command: ${command}`);

  const reducer = (acc, [key, ...rest]) => {
    if (rest.length === 0) return acc;

    if (key.startsWith('--')) {
      const [value, ...args] = rest;
      return reducer({ ...acc, [key.replace(/^--/, '')]: value }, args);
    } else {
      throw new Error('Invalid set of arguments');
    }
  };

  const parsedArgs = reducer({}, args);
  const isValid = (argsPattern.options || []).every(k => !!parsedArgs[k]);

  if (!isValid) throw new Error('Invalid set of args');

  if (argsPattern.action)
    return argsPattern.action(parsedArgs);
};

Function.prototype._ = function(g) { return x => this(g(x)); };

main();

