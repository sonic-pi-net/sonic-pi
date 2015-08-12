var osc = require('osc-min');
var dgram = require('dgram');

let instance = null;

class Receiver {
  constructor(options = {}) {
    if(!instance) {
      instance = this;
      instance.initialize(options);
      this.default_port = 4558;
      this.port = options.port || this.default_port;
    }
    return instance;
  }

  initialize(options) {
    this.sock = dgram.createSocket("udp4", function(msg, rinfo) {
      var error;
      try {
        return console.log(osc.fromBuffer(msg));
      } catch (_error) {
        error = _error;
        return console.log("invalid OSC packet");
      }
    });
    this.sock.bind(this.port);
  }
}

export default Receiver;
