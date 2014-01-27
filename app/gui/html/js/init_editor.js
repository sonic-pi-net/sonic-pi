var editor = CodeMirror.fromTextArea(document.getElementById("code"), {
    mode: "text/x-ruby",
    tabMode: "indent",
    matchBrackets: true,
    indentUnit: 2,
    lineNumbers: true,
    value: "#foo"
});
