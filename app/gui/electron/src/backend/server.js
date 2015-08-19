let spawn = require('child_process').spawn;
let fs = require('fs');

class Server {
  constructor(options = {}) {
    this.stdoutHandler = options.stdoutHandler || ((data) => console.log(data.toString()));
    this.stderrHandler = options.stderrHandler || ((data) => console.log(data.toString()));
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
    
    if(! fs.existsSync(ruby_path) {
      ruby_path = "ruby";
    });

    if(! fs.existsSync(ruby_path))
      ruby_path = "ruby";

    this.server = spawn(ruby_path, [server_path]);
    this.server.stdout.on("data", this.stdoutHandler);
    this.server.stderr.on("data", this.stderrHandler);
    return this;
  }

  kill() {
    this.server.kill();
  }
}

export default Server;