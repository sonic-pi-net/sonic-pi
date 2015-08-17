var themeSelector = document.querySelector("aside#settings-pane select#theme")
themeSelector.value = Editor.instance().theme;
themeSelector.addEventListener("change", function(event, target){
  Editor.instance().theme = this.value;
})

var keysSelector = document.querySelector("aside#settings-pane select#key-binding")
keysSelector.value = Editor.instance().keyBinding;
keysSelector.addEventListener("change", function(event, target){
  Editor.instance().keyBinding = `ace/keyboard/${this.value}`;
})

var lineNumbersCheckBox = document.querySelector("aside#settings-pane input#show-numbers")
lineNumbersCheckBox.checked = Editor.instance().showGutter;
lineNumbersCheckBox.addEventListener("change", function(event, target){
  Editor.instance().showGutter = this.checked;
})
