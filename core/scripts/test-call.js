const Websocket = require('ws');
const ws = new Websocket('ws://127.0.0.1:3005');

ws.on('message', data => {
  console.log(data);
  ws.close();
});

ws.on('open', () => {
  ws.send(JSON.stringify({
    action: 'blocks/execute',
    id: 'test',
    blocks: [
      {
        name: '1',
        lang: 'js',
        code: 'console.log("hello world", 30 ** 3);',
      },
      {
        name: '2',
        lang: 'bash',
        code: 'echo goobar 200;',
      },
    ],
  }));
});

