var instance = null;

class Editor {
  constructor(options = {}) {
    if(!instance) {
      var selector = options.selector || "editor";
      this.editor = ace.edit(selector);
      this.editor.getSession().setMode("ace/mode/ruby");
      this.editor.getSession().setUseSoftTabs(true);
      this.editor.getSession().setTabSize(2);
      this.editor.focus();
      instance = this;
      if(options.onSave) {
        this.onSave(options.onSave);
      }
    }
    return instance;
  }

  get showGutter() {
    this.editor.renderer.getShowGutter();
  }

  set showGutter(newVal) {
    this.editor.renderer.setShowGutter(newVal);
  }

  get theme() {
    this.editor.getTheme();
  }
  set theme(themeName) {
    this.editor.setTheme(themeName);
  }

  get keyBinding() {
    this.editor.getKeyboardHandler();
  }

  set keyBinding(bindingName) {
    this.editor.setKeyboardHandler(bindingName);
  }

  value() {
    return this.editor.getValue();
  }

  static instance() {
    return instance;
  }

  onSave(fun) {
    this.editor.commands.addCommand({
      name: 'saveFile',
      bindKey: {
        win: 'Ctrl-S',
        mac: 'Command-S',
        sender: 'editor|cli'
      },
      exec: function(env, args, request) {
        fun.call();
      }
    });
  }

  focus() {
    this.editor.focus();
  }
}
