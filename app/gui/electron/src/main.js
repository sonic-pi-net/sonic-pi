var app = require('app');
var BrowserWindow = require('browser-window');
import Receiver from './backend/receiver';
import Sender from './backend/sender';

var exec = require('child_process').exec;
var mainWindow = null;

app.on('ready', function() {
  exec('./server.sh', function (error, stdout, stderr) {
    console.log('stdout: ' + stdout);
    console.log('stderr: ' + stderr);
    if (error !== null) {
      console.log('exec error: ' + error);
    }
  });

  mainWindow = new BrowserWindow({width: 800, height: 600});
  mainWindow.loadUrl('file://' + __dirname + '/index.html');

  let receiver = new Receiver();
  let sender = new Sender();

  sender.runCode("play 70; sleep 1; play 80");

  mainWindow.on('closed', function() {
    mainWindow = null;
  });
});
