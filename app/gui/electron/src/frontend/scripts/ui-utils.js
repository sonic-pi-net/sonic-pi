export function bindThemeSelect(editor, settings, selector) {
  // TODO: ohmy, this is dirty.
  let str = "";
  str += '<optgroup label="Bright">';
  let brightThemes = editor.availableThemes.filter((elem) => { return !elem.isDark });
  let darkThemes   = editor.availableThemes.filter((elem) => { return elem.isDark });

  for(theme of brightThemes)
    str += `<option value="${theme.value}">${theme.name}</option>`;
  str += '</optgroup><optgroup label="Dark">';

  for(theme of darkThemes)
    str += `<option value="${theme.value}">${theme.name}</option>`;
  str += '</optgroup>';

  let themeSelector = document.querySelector(selector);
  themeSelector.innerHTML = str;
  themeSelector.value = editor.theme;
  themeSelector.addEventListener("change", function(event, target){
    editor.theme = this.value;
    settings.editor.theme = this.value;
    settings.save();
  })
}

export function bindKeysSelect(editor, settings, selector) {
  let keysSelect = document.querySelector(selector);
  keysSelect.value = settings.editor.keyBinding;
  keysSelect.addEventListener("change", function(event, target){
    editor.keyBinding = this.value;
    settings.editor.keyBinding = this.value;
    settings.save();
  });
}

export function bindLineNumbersCheckbox(editor, settings, selector) {
  let lineNumbersCheckBox = document.querySelector(selector);
  lineNumbersCheckBox.checked = editor.showGutter;
  lineNumbersCheckBox.addEventListener("change", function(event, target){
    editor.showGutter = this.checked;
    settings.editor.showGutter = this.checked;
    settings.save();
  });
}
