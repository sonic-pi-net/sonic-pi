fs = require('fs');

class Settings {
  constructor(options={}) {
    this.path = options.path || "./settings.json";
    return this;
  }
  load(options = {}) {
    let that = this;

    return new Promise((resolve, reject) => {
      fs.readFile(that.path,(err, data) => {
        if(err)
          reject("Could not find the settings file");
        // TODO: use Object.assign
        that.editor = JSON.parse(data).editor;
        that.ui = JSON.parse(data).ui;
        resolve(that);
      });
    });
  }

  save() {
    return new Promise((resolve, reject) => {
      let settings = { editor: this.editor, ui: this.ui };

      fs.writeFile(this.path, JSON.stringify(settings), function(err) {
        if(err)
          return console.log(err);
      })
    })
  }
}

export default Settings;