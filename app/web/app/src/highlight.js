// SPDX-License-Identifier: AGPL-3.0-or-later
// Highlighting as native Sonic Pi does it: Sonic Pi is standard Ruby, so the
// tokens come from CodeMirror's Ruby mode, and the colours from the theme
// keys native's lexer (app/gui/widgets/sonicpilexer.cpp) paints each Ruby
// style with. Classes, not inline colours, so a theme change repaints
// everything and a static snippet (docs, cards, the completion popup)
// highlights exactly like the editor.
import { StreamLanguage, syntaxHighlighting } from "@codemirror/language";
import { ruby } from "@codemirror/legacy-modes/mode/ruby";
import { tags as t, tagHighlighter, highlightTree } from "@lezer/highlight";

// QsciLexerRuby's keyword set: what native colours as Keyword. CodeMirror's
// Ruby mode counts more words (loop, lambda, raise ...) that native leaves
// plain, and counts true/false/nil as atoms, which native calls keywords.
export const NATIVE_KEYWORDS = new Set(
  "__FILE__ and def end in or self unless __LINE__ begin defined? ensure module redo super until BEGIN break do false next rescue then when END case else for nil retry true while alias class elsif if not return undef yield".split(" "),
);

const PLAIN_STYLES = new Set(["keyword", "atom", "variable", "variableName", "builtin", "variableName.standard", "property", "propertyName", "meta"]);

const sonicPiMode = {
  ...ruby,
  name: "sonic-pi",
  token(stream, state) {
    const style = ruby.token(stream, state);
    if (style === null || PLAIN_STYLES.has(style)) {
      const word = stream.current();
      if (NATIVE_KEYWORDS.has(word)) return "keyword";
      if (style === "keyword") return "variable";
      if (word.startsWith("@")) return "variableName.special"; // InstanceVariable, ClassVariable
      if (style === "atom" && /^[A-Z]/.test(word)) return "typeName"; // Foo::Bar is a constant, not a symbol
      if (style === "property" || style === "propertyName") return "variable";
    }
    return style;
  },
};

export const sonicPiLanguage = StreamLanguage.define(sonicPiMode);

// Ruby style → the native theme key it is painted with (see style.css).
export const spHighlighter = tagHighlighter([
  { tag: t.keyword, class: "sp-keyword" },            // KeywordForeground
  { tag: t.atom, class: "sp-symbol" },                // SymbolForeground: :sym and opt keys
  { tag: t.number, class: "sp-number" },              // NumberForeground
  { tag: t.string, class: "sp-string" },              // DoubleQuotedStringForeground
  { tag: [t.special(t.string), t.regexp], class: "sp-regex" }, // RegexForeground
  { tag: t.comment, class: "sp-comment" },            // CommentForeground
  { tag: t.definition(t.variableName), class: "sp-def" }, // FunctionMethodNameForeground
  { tag: t.special(t.variableName), class: "sp-ivar" },   // InstanceVariableForeground
  { tag: [t.tagName, t.typeName, t.className], class: "sp-const" }, // ClassNameForeground
]);

export const highlighting = [sonicPiLanguage, syntaxHighlighting(spHighlighter)];

/** The pieces of `code`, highlighted: [text, class] in order (class "" for plain text). */
function pieces(code) {
  const tree = sonicPiLanguage.parser.parse(code);
  const out = [];
  let pos = 0;
  highlightTree(tree, spHighlighter, (from, to, cls) => {
    if (from > pos) out.push([code.slice(pos, from), ""]);
    out.push([code.slice(from, to), cls]);
    pos = to;
  });
  if (pos < code.length) out.push([code.slice(pos), ""]);
  return out;
}

const ESC = { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" };
/** `code` highlighted as HTML text: its pieces escaped, each styled one in a span of its class. No document needed:
 *  the site's build (scripts/build-site.mjs) colours the tutorial's inline code with it, as the editor would. */
export function highlightHTML(code) {
  const esc = (x) => x.replace(/[&<>"]/g, (c) => ESC[c]);
  return pieces(code).map(([text, cls]) => (cls ? `<span class="${cls}">${esc(text)}</span>` : esc(text))).join("");
}

/** `code` highlighted as HTML, a line at a time: a token that runs over a line's end (a string, a comment) is closed at
 *  it and opened again on the next, so each line stands alone as the editor's .cm-line does (ui/card-html.js). */
export function highlightLinesHTML(code) {
  const esc = (x) => x.replace(/[&<>"]/g, (c) => ESC[c]);
  const lines = [""];
  for (const [text, cls] of pieces(code)) {
    text.split("\n").forEach((part, i) => {
      if (i) lines.push("");
      if (part) lines[lines.length - 1] += cls ? `<span class="${cls}">${esc(part)}</span>` : esc(part);
    });
  }
  return lines;
}

/**
 * Highlights `code` as a still copy of the editor: the same tree of elements CodeMirror draws (.cm-editor >
 * .cm-scroller > .cm-content > .cm-line, an empty line holding a <br>) with the same token classes, so the same
 * rules lay both out and a block and an editor of the same code sit pixel for pixel alike (ui/card.js).
 */
export function renderLines(code, el = document.createElement("div")) {
  el.classList.add("cm-editor", "sp-code");
  el.textContent = "";
  const scroller = document.createElement("div"); scroller.className = "cm-scroller";
  const content = document.createElement("div"); content.className = "cm-content";
  let line = document.createElement("div"); line.className = "cm-line";
  const lines = [line];
  for (const [text, cls] of pieces(code)) {
    const parts = text.split("\n");
    parts.forEach((part, i) => {
      if (i) { line = document.createElement("div"); line.className = "cm-line"; lines.push(line); }
      if (!part) return;
      if (!cls) return void line.appendChild(document.createTextNode(part));
      const span = document.createElement("span"); span.className = cls; span.textContent = part;
      line.appendChild(span);
    });
  }
  for (const l of lines) { if (!l.firstChild) l.appendChild(document.createElement("br")); content.appendChild(l); }
  scroller.appendChild(content); el.appendChild(scroller);
  return el;
}

/** Highlights `code` into `el` (a new <pre> by default), as the editor would. */
export function renderCode(code, el = document.createElement("pre")) {
  el.classList.add("sp-code");
  el.textContent = "";
  const tree = sonicPiLanguage.parser.parse(code);
  let pos = 0;
  const put = (text, cls) => {
    if (!text) return;
    if (!cls) return void el.appendChild(document.createTextNode(text));
    const span = document.createElement("span");
    span.className = cls;
    span.textContent = text;
    el.appendChild(span);
  };
  highlightTree(tree, spHighlighter, (from, to, cls) => {
    put(code.slice(pos, from));
    put(code.slice(from, to), cls);
    pos = to;
  });
  put(code.slice(pos));
  return el;
}
