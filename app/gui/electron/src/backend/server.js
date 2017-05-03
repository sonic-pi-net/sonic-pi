let spawn = require('child_process').spawn;
let fs = require('fs');

class Server {
  constructor(options = {}) {
    this.stdoutHandler = options.stdoutHandler || ((data) => console.log(data.toString()));
    this.stderrHandler = options.stderrHandler || ((data) => console.log(data.toString()));
    this.booted = false;
    return this;
  }

  spawn() {
    let platform = process.platform;
    let ruby_path = null;
    let prefix = "../../..";
    let server_path = prefix + "/app/server/bin/sonic-pi-server.rb";

    if(platform == "darwin") {
      ruby_path = prefix + "/app/server/native/osx/ruby/bin/ruby";
    } else if (platform == "win32") {
      ruby_path = prefix + "/app/server/native/windows/bin/ruby.exe";
    } else {
      ruby_path = prefix + "/app/server/native/raspberry/ruby/bin/ruby";
    }

    if(! fs.existsSync(ruby_path))
      ruby_path = "ruby";

    this.server = spawn(ruby_path, [server_path]);

    // this promise never gets rejected, that's bad
    return new Promise((resolve, reject) => {
      this.server.stderr.on("data", this.stderrHandler);
      this.server.stdout.on("data", (data) => {
        if(this.isServerBooted(data))
          resolve();
        this.stdoutHandler(data);
      })
    });
  }

  // check whether the SC server has been properly started
  isServerBooted(data) {
    let matchString = /SuperCollider 3 server ready/;
    return matchString.test(data.toString());
  }

  kill() {
    this.server.kill();
  }
}

export default Server;