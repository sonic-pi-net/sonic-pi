class Editor {
  constructor(options = {}) {
    var selector = options.selector || "editor";
    this.editor = ace.edit(selector);
    this.editor.setTheme("ace/theme/tomorrow_night_eighties");
    this.editor.getSession().setMode("ace/mode/ruby");
    this.editor.setKeyboardHandler("ace/keyboard/vim");
    this.editor.focus();
    if(options.onSave) {
      this.onSave(options.onSave);
    }
  }

  // TODO: replace with getter
  value() {
    return this.editor.getValue();
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
}
