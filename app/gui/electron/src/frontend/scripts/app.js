import Sender from './backend/sender';

var runCurrentBuffer = () => {
  var sender = new Sender();
  sender.runCode(editor.value());
}

var editor = new Editor({
  selector: "editor",
  onSave: () => { runCurrentBuffer() }
});

// TODO: Discuss if need to replace with a proper class
var runButton = document.getElementById("run-button");
runButton.addEventListener("click", function(event, target) {
  runCurrentBuffer();
  editor.focus();
  event.preventDefault();
})

var settingsButton = document.getElementById("settings-button");
settingsButton.addEventListener("click", function(event, target) {
  this.classList.toggle("active");
  var settingsPane = document.getElementById("settings-pane");
  settingsPane.classList.toggle("hidden");
  event.preventDefault();
})
