import Sender from './backend/sender';
import Settings from './frontend/scripts/settings';
import Editor from './frontend/scripts/editor';
import * as utils from './frontend/scripts/ui-utils';
let uuid = require('node-uuid');
// For now, generate one id for the whole app.
const ID = uuid.v1();

let runCurrentBuffer = () => {
  new Sender({ id: ID }).runCode(editor.value());
  editor.highlight();
}

let stopCurrentBuffer = () => {
  new Sender({ id: ID }).stopAllJobs();
}

let editor = null;
let settings = new Settings();

settings.load().then( (settings) => {
  editor = new Editor({
    selector: "editor",
    settings: settings.editor,
    onCmdR: () => { runCurrentBuffer() },
    onCmdS: () => { stopCurrentBuffer() }
  });

  utils.populateThemeSelect(editor, "aside#settings-pane select#theme");
  utils.bindThemeSelect(editor, settings, "aside#settings-pane select#theme");
  utils.bindKeysSelect(editor, settings, "aside#settings-pane select#key-binding");
  utils.bindLineNumbersCheckbox(editor,settings, "aside#settings-pane input#show-numbers");
  utils.bindHighlightCheckbox(editor,settings, "aside#settings-pane input#highlight-on-run");

  utils.bindUiThemeSelect(settings, "aside#settings-pane select#ui-theme");
});

// TODO: Discuss if need to replace with a proper class
let runButton = document.getElementById("run-button");
runButton.addEventListener("click", (event, target) => {
  runCurrentBuffer();
  editor.focus();
  event.preventDefault();
})

let stopButton = document.getElementById("stop-button");
stopButton.addEventListener("click", (event, target) => {
  stopCurrentBuffer();
  editor.focus();
  event.preventDefault();
});

let settingsButton = document.getElementById("settings-button");
settingsButton.addEventListener("click", (event, target) => {
  let settingsPane = document.getElementById("settings-pane");
  let editorContainer= document.getElementById("editor");
  settingsPane.classList.toggle("hidden");
  editorContainer.classList.toggle("compressed");
  event.preventDefault();
})
