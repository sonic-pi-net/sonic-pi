var osc = require('osc-min');
var dgram = require('dgram');

class Sender {
  constructor (options = {}) {
    this.default_port = 4557;
    this.port = options.port || this.default_port;
    this.host = "localhost";
    this.socket = dgram.createSocket("udp4");
    this.id = options.id;
    return this;
  }

  runCode(code) {
    let buf = osc.toBuffer({
      address: "/run-code",
      args: [this.id, code]
    });
    this.socket.send(buf, 0, buf.length, this.port, this.host);
  }

  stopAllJobs() {
    let buf = osc.toBuffer({
      address: "/stop-all-jobs",
      args: [this.id]
    });
    this.socket.send(buf, 0, buf.length, this.port, this.host);
  }
}

export default Sender;