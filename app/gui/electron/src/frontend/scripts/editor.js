var instance = null;

class Editor {
  constructor(options = {}) {
    // singleton for now. Can be extended to accomodate multiple editors.
    if(!instance) {
      var selector = options.selector || "editor";
      this.editor = ace.edit(selector);
      if(options.settings) this.applySettings(options.settings)
      if(options.onCmdR)   this.onCmdR(options.onCmdR);
      if(options.onCmdS)   this.onCmdS(options.onCmdS);
      this.editor.focus();
      instance = this;
    }
    return instance;
  }

  applySettings(settings) {
    this.theme          = settings.theme;
    this.keyBinding     = settings.keyBinding;
    this.showGutter     = settings.showGutter;
    this.highlightOnRun = settings.highlightOnRun;
    var rubyMode = ace.require("ace/mode/ruby").Mode;
    this.editor.getSession().setMode(new rubyMode());
    this.editor.getSession().setUseSoftTabs(true);
    this.editor.getSession().setTabSize(2);
  }

  get availableThemes() {
    let themelist = ace.require("ace/ext/themelist");
    var result = [];
    for (var theme of themelist.themes) {
      result.push({ name: theme.caption, value: theme.theme, isDark: theme.isDark });
    }
    return result;
  }

  get showGutter() {
    return this.editor.renderer.getShowGutter();
  }

  set showGutter(newVal) {
    this.editor.renderer.setShowGutter(newVal);
  }

  get theme() {
    return this.editor.getTheme();
  }
  set theme(themeName) {
    this.editor.setTheme(themeName);
  }

  get keyBinding() {
    return this.editor.getKeyboardHandler().$id;
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

  highlight() {
    if(this.highlightOnRun) {
      this.editor.container.classList.remove("run-animation")
      this.editor.container.offsetWidth = this.editor.container.offsetWidth;
      this.editor.container.classList.add("run-animation")
    }
  }

  onCmdR(fun) {
    this.editor.commands.addCommand({
      name: 'Run',
      bindKey: {
        win: 'Ctrl-R',
        mac: 'Command-R',
        sender: 'editor|cli'
      },
      exec: function(env, args, request) {
        fun.call();
      }
    });
  }

  onCmdS(fun) {
    this.editor.commands.addCommand({
      name: 'Stop',
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

export default Editor;