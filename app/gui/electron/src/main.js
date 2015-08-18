let app = require('app');
let BrowserWindow = require('browser-window');
let spawn = require('child_process').spawn;
let server = spawn('./../../../app/server/native/osx/ruby/bin/ruby', ['./../../../app/server/bin/sonic-pi-server.rb']);

import Receiver from './backend/receiver';
import Sender   from './backend/sender';

let mainWindow = null;

server.stdout.on('data', (data) => {
  console.log(data.toString());
});

server.stderr.on('data', (data) => {
  console.log(data.toString());
});

app.on("ready", () => {
  let receiver = new Receiver();

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
