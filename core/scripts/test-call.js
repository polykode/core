const Websocket = require('ws');
const ws = new Websocket('ws://127.0.0.1:3000');

ws.on('message', data => {
  console.log(data);
  ws.close();
});

ws.on('open', () => {
  ws.send(JSON.stringify({ action: 'md/execute', id: 'test' }));
});

