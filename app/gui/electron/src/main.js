let app = require('app');
let BrowserWindow = require('browser-window');

import Receiver from './backend/receiver';
import Sender   from './backend/sender';
import Server   from './backend/server';

let mainWindow = null;
let server = new Server();
let splashScreenWindow = null;


app.on("ready", () => {
  let receiver = new Receiver();
  // splash size = splash image size
  splashScreen = new BrowserWindow({ width: 923, height: 606, frame: false });
  splashScreen.loadUrl("file://" + __dirname + "/splash.html");

  mainWindow = new BrowserWindow({ width: 800, height: 600, show: false });
  mainWindow.loadUrl("file://" + __dirname + "/index.html");

  server.spawn().then(() => {
    receiver.initialize();
    splashScreen.close();
    mainWindow.show();
  });

  mainWindow.on("closed", () => {
    mainWindow = null;
  });
});

app.on("quit", () => {
  console.log("Killing the child process");
  server.kill();
});
