import Sender from './backend/sender';

var editor = ace.edit("editor");
editor.setTheme("ace/theme/tomorrow_night_eighties");
editor.getSession().setMode("ace/mode/ruby");
editor.setKeyboardHandler("ace/keyboard/vim");
editor.focus();

var runCurrentBuffer = function() {
  var sender = new Sender();
  sender.runCode(editor.getValue());
}

editor.commands.addCommand({
  name: 'saveFile',
  bindKey: {
    win: 'Ctrl-S',
    mac: 'Command-S',
    sender: 'editor|cli'
  },
  exec: function(env, args, request) {
    runCurrentBuffer();
  }
});
