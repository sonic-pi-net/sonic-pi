import Sender from './backend/sender';
import Settings from './frontend/scripts/settings';
import Editor from './frontend/scripts/editor';
import * as utils from './frontend/scripts/ui-utils'

var runCurrentBuffer = () => {
  var sender = new Sender();
  sender.runCode(editor.value());
}

let editor = null;
let settings = new Settings();

settings.load().then( (settings) => {
  editor = new Editor({
    selector: "editor",
    settings: settings.editor,
    onSave: () => { runCurrentBuffer() }
  });

  utils.bindThemeSelect(editor, settings, "aside#settings-pane select#theme");
  utils.bindKeysSelect(editor, settings, "aside#settings-pane select#key-binding");
  utils.bindLineNumbersCheckbox(editor,settings, "aside#settings-pane input#show-numbers");
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
