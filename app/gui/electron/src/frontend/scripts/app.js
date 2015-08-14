import Sender from './backend/sender';

var runCurrentBuffer = () => {
  var sender = new Sender();
  sender.runCode(editor.value());
}

var editor = new Editor({
  selector: "editor",
  onSave: () => { runCurrentBuffer() }
});


