var instance = null;

class Editor {
  constructor(options = {}) {
    if(!instance) {
      var selector = options.selector || "editor";
      this.editor = ace.edit(selector);
      if(options.settings) this.applySettings(options.settings)
      if(options.onSave)   this.onSave(options.onSave);
      this.editor.focus();
      instance = this;
    }
    return instance;
  }

  applySettings(settings) {
    this.theme      = settings.theme;
    this.keyBinding = settings.keyBinding;
    this.showGutter = settings.showGutter;
    this.editor.getSession().setMode("ace/mode/ruby");
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

export default Editor;