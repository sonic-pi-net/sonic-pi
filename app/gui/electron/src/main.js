let app = require('app');
let BrowserWindow = require('browser-window');

import Receiver from './backend/receiver';
import Sender   from './backend/sender';
import Server   from './backend/server';

let mainWindow = null;
let server = new Server();

app.on("ready", () => {
  let receiver = new Receiver();
  server.spawn();

  mainWindow = new BrowserWindow({width: 800, height: 600});
  mainWindow.loadUrl("file://" + __dirname + "/index.html");
  mainWindow.openDevTools();

  mainWindow.on("closed", () => {
    mainWindow = null;
  });
});

app.on("quit", () => {
  console.log("Killing the child process");
  server.kill();
});
