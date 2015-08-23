export function populateThemeSelect(editor, selector) {
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

  let themeSelect = document.querySelector(selector);
  themeSelect.innerHTML = str;
}

export function bindThemeSelect(editor, settings, selector) {
  let themeSelect = document.querySelector(selector);
  themeSelect.value = settings.editor.theme;
  themeSelect.addEventListener("change", function(event, target){
    editor.theme = this.value;
    settings.editor.theme = this.value;
    settings.save();
  });
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
  lineNumbersCheckBox.checked = settings.editor.showGutter;
  lineNumbersCheckBox.addEventListener("change", function(event, target){
    editor.showGutter = this.checked;
    settings.editor.showGutter = this.checked;
    settings.save();
  });
}

export function bindHighlightCheckbox(editor, settings, selector) {
  let highlightCheckbox = document.querySelector(selector);
  highlightCheckbox.checked = settings.editor.highlightOnRun;
  highlightCheckbox.addEventListener("change", function(event, target){
    editor.highlightOnRun = this.checked;
    settings.editor.highlightOnRun = this.checked;
    settings.save();
  });
}

export function bindUiThemeSelect(settings, selector) {
  let uiThemeSelect = document.querySelector(selector);
  uiThemeSelect.value = settings.ui.theme;
  document.body.className = uiThemeSelect.value;

  uiThemeSelect.addEventListener("change", function(event, target){
    document.body.className = this.value;
    settings.ui.theme = this.value;
    settings.save();
  });
}
