// SPDX-License-Identifier: AGPL-3.0-or-later
// The editor: CodeMirror 6, buffers kept in the browser, Tab accepting a
// completion — with native Sonic Pi's ten buffers, its
// highlighting (./highlight.js), its autocomplete (./completion), its Alt+M
// re-indent and its editing commands, which native's shortcuts run in
// whichever keymap the player picked (./shortcuts.js; main.js presses them
// through command()).
import { EditorState, StateField, StateEffect, RangeSet } from "@codemirror/state";
import { icon } from "./icons.js";
import { EditorView, ViewPlugin, keymap, lineNumbers, highlightActiveLine, highlightActiveLineGutter, drawSelection, Decoration, WidgetType, GutterMarker, gutterLineClass } from "@codemirror/view";
import {
  history, historyKeymap, defaultKeymap, indentMore, indentLess, undo, redo, selectAll, transposeChars, moveLineUp, moveLineDown,
  cursorCharForward, cursorCharBackward, cursorLineDown, cursorLineUp, cursorLineBoundaryForward, cursorLineBoundaryBackward, cursorDocStart, cursorDocEnd, cursorGroupForward, cursorGroupBackward,
  selectCharForward, selectCharBackward, selectLineDown, selectLineUp, selectLineBoundaryForward, selectLineBoundaryBackward, selectDocStart, selectDocEnd, selectGroupForward, selectGroupBackward,
  deleteCharForward, deleteCharBackward, deleteGroupForward, deleteGroupBackward,
} from "@codemirror/commands";
import { bracketMatching, indentUnit } from "@codemirror/language";
import { search, searchKeymap, highlightSelectionMatches, openSearchPanel, closeSearchPanel, findNext, findPrevious, SearchQuery, setSearchQuery, getSearchQuery } from "@codemirror/search";
import { acceptCompletion, closeCompletion, completionKeymap, completionStatus, startCompletion, selectedCompletion } from "@codemirror/autocomplete";
import { highlighting } from "./highlight.js";
import { completionExtensions, completionDocText, hideSlider, moveCompletion } from "./completion/cm.js";
import { announce, Announcement } from "./announce.js";
import { LoopScopeState, drawLoopScope, SWEEP_WINDOW, SWEEP_SEARCH, SCROLL_WINDOW } from "./loopscope.js";
import { animateWhileShown } from "./ui/shown.js";
import { shadowFor } from "./shadow.js";

export const NUM_BUFFERS = 10;   // the most a set can have (workspace.js MAX_BUFFERS); each set its own number
// the first buffer on a first visit (workspace.js puts it there): notes to hear, change and hear again — short lines,
// few words, the same on every screen (the toolbar's buttons, not their keys)
export const STARTER = `# Welcome to Sonic Pi!
#
# Press ▶ to run and hear this code.
#
# Try changing the notes
# and press ▶ again.
#
# Stuck? Press Δ for help.
# Have fun!

use_synth :pluck
play 48
play 52
play 72
`;
const FONT_KEY = "sp-font-size";
/** Whether a buffer's text is free for code arriving: nothing in it, or the first visit's starter as it came. */
// (and a starter an earlier version gave, still untouched in a buffer: as free as today's)
export const FORMER_STARTERS = [`# Welcome to Sonic Pi!
#
# Press ▶ to run and hear this code.
#
# Try changing cutoff to 70
# and press ▶ again.
#
# Stuck? Press Δ for help.
# Have fun!

use_synth :prophet
play 40, release: 8, cutoff: 95
`];
export const isBlankBuffer = (text) => !text.trim() || text === STARTER || FORMER_STARTERS.includes(text);


const store = {
  get: (k) => { try { return localStorage.getItem(k); } catch { return null; } },
  set: (k, v) => { try { localStorage.setItem(k, v); } catch {} },
};

// ── Ruby block structure, for Enter and Alt+M ──────────────────────────────

const stripLine = (text) => text
  .replace(/"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'/g, '""')
  .replace(/#.*$/, "")
  .trim();

const OPENS_AT_START = /^(if|unless|while|until|case|def|begin|class|module|for)\b/;
const OPENS_AT_END = /(\bdo\s*(\|[^|]*\|)?|\{\s*(\|[^|]*\|)?|=\s*(if|unless|case|begin)\b.*)$/;
const DEDENTS = /^(end\b|\}|else\b|elsif\b|when\b|in\b|rescue\b|ensure\b)/;
const CLOSES = /^(end\b|\})/;

function opens(s) {
  if (/^def\b.*\)\s*=\s*\S/.test(s) || /^def\s+\w+[?!]?\s*=\s*\S/.test(s)) return false; // endless def
  const open = OPENS_AT_START.test(s) || OPENS_AT_END.test(s);
  return open && !/\bend\s*$/.test(s.replace(/^end\b/, ""));
}

/** Block depth after each line; `indents[i]` is line i's own indent level. */
export function blockIndents(lines) {
  const indents = [];
  let depth = 0;
  for (const raw of lines) {
    const s = stripLine(raw);
    indents.push(Math.max(0, DEDENTS.test(s) ? depth - 1 : depth));
    if (CLOSES.test(s)) depth = Math.max(0, depth - 1);
    if (opens(s)) depth++;
  }
  return { indents, depth };
}

export function newlineAndIndent(view) {
  const { state } = view;
  const pos = state.selection.main.head;
  const line = state.doc.lineAt(pos);
  const lines = [];
  for (let i = 1; i < line.number; i++) lines.push(state.doc.line(i).text);
  lines.push(line.text.slice(0, pos - line.from));
  const { depth } = blockIndents(lines);
  const after = line.text.slice(pos - line.from);
  const level = DEDENTS.test(stripLine(after)) ? Math.max(0, depth - 1) : depth;
  const indent = "  ".repeat(level);
  view.dispatch({
    changes: { from: pos, to: pos + (after.match(/^\s*/)[0].length), insert: "\n" + indent },
    selection: { anchor: pos + 1 + indent.length },
    scrollIntoView: true,
    userEvent: "input",
  });
  return true;
}

/** Native's Alt+M: every line re-indented by its block depth. */
function reindentBuffer(view) {
  const { doc } = view.state;
  const lines = doc.toString().split("\n");
  const { indents } = blockIndents(lines);
  const changes = [];
  lines.forEach((text, i) => {
    const line = doc.line(i + 1);
    const lead = text.match(/^\s*/)[0].length;
    const want = text.trim() === "" ? "" : "  ".repeat(indents[i]);
    if (text.slice(0, lead) !== want) changes.push({ from: line.from, to: line.from + lead, insert: want });
  });
  if (changes.length) view.dispatch({ changes, userEvent: "input.indent" });
  return true;
}

/** Tab, as native's: the current line (or every line the selection touches) put at its block depth, not a tab added.
 * A caret in the line's leading space goes to the start of its code; a line already right opens the completions. */
function indentLinesToDepth(view) {
  const { state } = view, { doc } = state;
  const { indents } = blockIndents(doc.toString().split("\n"));
  const changes = [];
  const seen = new Set();
  for (const r of state.selection.ranges) {
    for (let n = doc.lineAt(r.from).number; n <= doc.lineAt(r.to).number; n++) {
      if (seen.has(n)) continue;
      seen.add(n);
      const line = doc.line(n), lead = line.text.match(/^\s*/)[0].length;
      const want = "  ".repeat(indents[n - 1]);
      if (line.text.slice(0, lead) !== want) changes.push({ from: line.from, to: line.from + lead, insert: want });
    }
  }
  // nothing to put right (the line already at its depth), a caret and no selection: Tab asks for the completions
  // instead, as Ctrl+Space does — a key no OS or input method takes for itself
  if (!changes.length && state.selection.ranges.length === 1 && state.selection.main.empty) {
    const line = doc.lineAt(state.selection.main.head), lead = line.text.match(/^\s*/)[0].length;
    if (state.selection.main.head - line.from >= lead) return startCompletion(view);
  }
  // the caret, if it sat in the leading space of its line, to the start of the line's code (in the re-indented doc)
  const after = state.update({ changes }).state, sel = after.selection.main;
  let selection;
  if (sel.empty) {
    const line = after.doc.lineAt(sel.head), lead = line.text.match(/^\s*/)[0].length;
    if (sel.head - line.from <= lead) selection = { anchor: line.from + lead };
  }
  view.dispatch({ changes, selection, userEvent: "input.indent" });
  return true;
}

export function toggleComment(view) {
  const { state } = view;
  const sel = state.selection.main;
  const first = state.doc.lineAt(sel.from).number, last = state.doc.lineAt(sel.to).number;
  const lines = [];
  for (let i = first; i <= last; i++) lines.push(state.doc.line(i));
  const all = lines.every((l) => l.text.trim() === "" || /^\s*#/.test(l.text));
  const changes = [];
  for (const l of lines) {
    if (l.text.trim() === "") continue;
    const lead = l.text.match(/^\s*/)[0].length;
    if (all) {
      const m = l.text.slice(lead).match(/^#\s?/);
      changes.push({ from: l.from + lead, to: l.from + lead + m[0].length });
    } else {
      changes.push({ from: l.from + lead, insert: "# " });
    }
  }
  if (changes.length) view.dispatch({ changes, userEvent: "input" });
  return true;
}

// ── Errors: the line an error names ────────────────────────────────────────

const setErrorLine = StateEffect.define();
class ErrorCaret extends WidgetType {
  toDOM() { return Object.assign(document.createElement("span"), { className: "cm-sp-error-caret", ariaHidden: "true" }); }
  eq() { return true; }
}
const errorLineField = StateField.define({
  create: () => Decoration.none,
  update(deco, tr) {
    deco = deco.map(tr.changes);
    for (const e of tr.effects) {
      if (!e.is(setErrorLine)) continue;
      if (e.value == null) { deco = Decoration.none; continue; }
      // a line, or { line, from, to }: the words the error is about (columns) get a wavy underline, a point a caret
      const { line: at, from, to } = typeof e.value === "number" ? { line: e.value } : e.value;
      const l = tr.state.doc.line(Math.min(Math.max(1, at), tr.state.doc.lines));
      const marks = [Decoration.line({ class: "cm-sp-error-line" }).range(l.from)];
      if (from != null) {
        const a = l.from + Math.min(from, l.length), b = l.from + Math.min(Math.max(from, to ?? from), l.length);
        // as native marks it: a word or more gets the zigzag under it, a single character (or a point) a ^ beneath
        marks.push(b - a > 1 ? Decoration.mark({ class: "cm-sp-error-span" }).range(a, b)
          : b > a ? Decoration.mark({ class: "cm-sp-error-point" }).range(a, b)
          : Decoration.widget({ widget: new ErrorCaret(), side: 1 }).range(a));
      }
      deco = Decoration.set(marks, true);
    }
    return deco;
  },
  provide: (f) => [
    EditorView.decorations.from(f),
    // the error's line number too, in the error's colour with an arrow into the line (native's marker)
    gutterLineClass.compute([f], (state) => {
      const lines = [];
      state.field(f).between(0, state.doc.length, (from, _to, d) => { if (d.spec.class === "cm-sp-error-line") lines.push(errorGutter.range(from)); });
      return RangeSet.of(lines);
    }),
  ],
});
const errorGutter = new (class extends GutterMarker { elementClass = "cm-sp-error-gutter"; })();

// ── Live: the line a sound came from ─────────────────────────────────────

export const addFlash = StateEffect.define();
export const dropFlash = StateEffect.define();

export const flashField = StateField.define({
  create: () => ({ lines: new Map(), deco: Decoration.none }),
  update(value, tr) {
    let lines = value.lines;
    let changed = false;
    const edit = () => { if (!changed) { lines = new Map(lines); changed = true; } };
    if (tr.docChanged && lines.size) { lines = new Map(); changed = true; }   // an edit moves lines: what flashed no longer lines up
    for (const e of tr.effects) {
      if (e.is(addFlash)) {
        edit();
        const prev = lines.get(e.value.line);
        lines.set(e.value.line, { flip: prev ? !prev.flip : false, id: e.value.id });
      } else if (e.is(dropFlash)) {
        const cur = lines.get(e.value.line);
        if (cur && cur.id === e.value.id) { edit(); lines.delete(e.value.line); }
      }
    }
    if (!changed) return value;
    const doc = tr.state.doc;
    const ranges = [];
    for (const [n, f] of lines) {
      if (n < 1 || n > doc.lines) continue;
      const line = doc.line(n);
      ranges.push(Decoration.line({ class: f.flip ? "sp-flash-a" : "sp-flash-b" }).range(line.from));
    }
    return { lines, deco: Decoration.set(ranges, true) };
  },
  provide: (f) => EditorView.decorations.from(f, (value) => value.deco),
});

// ── Run: the code lit behind its letters, as a highlighter pen would ──────
// Each line's code, from its first character to its last (no indent, no empty line, nothing past the end), marked
// a moment when Run is pressed (editor.js flash, style.css sp-kick-a/b); the class alternates, so a Run straight after
// another starts it again rather than carrying on the one before.
const kickCode = StateEffect.define();   // { id, flip }
const dropKick = StateEffect.define();   // id
const MAX_KICK_LINES = 2000;             // a buffer longer than this lights its first lines only
const kickField = StateField.define({
  create: () => ({ id: 0, deco: Decoration.none }),
  update(value, tr) {
    for (const e of tr.effects) {
      if (e.is(kickCode)) {
        const doc = tr.state.doc, mark = Decoration.mark({ class: e.value.flip ? "sp-kick-a" : "sp-kick-b" }), ranges = [];
        for (let n = 1; n <= Math.min(doc.lines, MAX_KICK_LINES); n++) {
          const line = doc.line(n), text = line.text, start = text.search(/\S/);
          if (start < 0) continue;
          ranges.push(mark.range(line.from + start, line.from + text.trimEnd().length));
        }
        value = { id: e.value.id, deco: Decoration.set(ranges) };
      } else if (e.is(dropKick) && e.value === value.id) value = { id: value.id, deco: Decoration.none };
    }
    if (tr.docChanged && value.deco !== Decoration.none) value = { id: value.id, deco: Decoration.none };   // an edit: it lets go
    return value;
  },
  provide: (f) => EditorView.decorations.from(f, (value) => value.deco),
});

// ── Waits: a thread held on a sync, marked at the line it waits on ────────
// main.js says which lines of the buffer have a thread parked on a sync
// ({line, key, who}); a small "waiting" after the line says so, its tooltip
// which threads and on what. The mark goes with the cue.
const setWaits = StateEffect.define();

/** A sync key as the code spells it: the Time State path /cue/tick is :tick; any other path is itself. */
const syncKey = (k) => (k.startsWith("/cue/") ? `:${k.slice(5)}` : /^[A-Za-z_]\w*$/.test(k) ? `:${k}` : k);

class WaitWidget extends WidgetType {
  constructor(key, who) { super(); this.key = key; this.who = who; }
  eq(other) { return other.key === this.key && other.who === this.who; }
  toDOM() {
    const el = document.createElement("span");
    el.className = "sp-wait";
    el.title = `${this.who}: waiting on sync ${this.key}`;
    el.append(document.createElement("i"), document.createTextNode("waiting"));
    return el;
  }
  ignoreEvent() { return true; }
}

// The mark belongs to the line of code a thread is parked on, so it is held by a position inside that line and
// drawn at whatever the end of that line is now. Holding the end itself would not do: type Return there and the
// mark goes with the new line, leaving the code it belongs to behind.
const waitField = StateField.define({
  create: () => ({ at: [], deco: Decoration.none }),
  update(value, tr) {
    let at = value.at;
    for (const e of tr.effects) {
      if (!e.is(setWaits)) continue;
      at = [...e.value]
        .sort((a, b) => a.line - b.line)
        .filter(({ line }) => line >= 1 && line <= tr.state.doc.lines)
        .map(({ line, key, who }) => ({ pos: tr.state.doc.line(line).from, key, who }));   // the line's start, which
    }                                                                                      // typing at its end cannot move
    if (tr.docChanged) at = at.map((w) => ({ ...w, pos: tr.changes.mapPos(w.pos, -1) }));
    if (at === value.at && !tr.docChanged) return value;
    const doc = tr.state.doc;
    const ranges = at
      .filter((w) => w.pos <= doc.length)
      .map((w) => Decoration.widget({ widget: new WaitWidget(syncKey(w.key), w.who), side: 1 }).range(doc.lineAt(w.pos).to));
    return { at, deco: Decoration.set(ranges, true) };
  },
  provide: (f) => EditorView.decorations.from(f, (v) => v.deco),
});

// ── Live loop scopes: native's inline scope after each live_loop's header line ──
// main.js says which loops have a scope stream ({name, slot}); the widget sits
// after the loop's header line, and the plugin reads and draws every one each
// frame through hooks.scopeFrame (the engine's getScope) — see ./loopscope.js.

export const setLoopScopes = StateEffect.define();

/** A loop's scope canvas: the editor's widget draws one; a card's still block puts one on its loop's line (ui/card.js). */
export function loopScopeCanvas(name, slot) {
  const c = document.createElement("canvas");
  c.className = "sp-loop-scope";
  c.dataset.slot = String(slot);
  c.title = name + "'s scope";
  c.setAttribute("aria-hidden", "true");
  return c;
}
class LoopScopeWidget extends WidgetType {
  constructor(name, slot) { super(); this.name = name; this.slot = slot; }
  eq(other) { return other.name === this.name && other.slot === this.slot; }
  toDOM() { return loopScopeCanvas(this.name, this.slot); }
  ignoreEvent() { return true; }
}

// the loop's header: its live_loop :name line
function loopHeaderLine(doc, name) {
  const re = new RegExp("^\\s*live_loop\\s+:" + name.replace(/[^\w]/g, "\\$&") + "\\b");
  for (let i = 1; i <= doc.lines; i++) if (re.test(doc.line(i).text)) return doc.line(i);
  return null;
}

export const loopScopeField = StateField.define({
  create: () => Decoration.none,
  update(deco, tr) {
    deco = deco.map(tr.changes);
    for (const e of tr.effects) {
      if (!e.is(setLoopScopes)) continue;
      const ranges = [];
      for (const { name, slot } of e.value) {
        const line = loopHeaderLine(tr.state.doc, name);
        if (line) ranges.push(Decoration.widget({ widget: new LoopScopeWidget(name, slot), side: 1 }).range(line.to));
      }
      deco = Decoration.set(ranges, true);
    }
    return deco;
  },
  provide: (f) => EditorView.decorations.from(f),
});

function loopScopePlugin(hooks) {
  return ViewPlugin.fromClass(class {
    constructor(view) {
      this.view = view;
      this.states = new Map();      // slot → LoopScopeState
      // frames only while there are loops' scopes to draw and the editor is on screen: none behind the site's pages
      this.loop = animateWhileShown(view.scrollDOM, () => this.frame());
    }
    update(u) { if (u.state.field(loopScopeField, false)?.size) this.loop.wake(); }
    frame() {
      const canvases = this.view.dom.querySelectorAll(".sp-loop-scope");
      if (!canvases.length) { if (this.states.size) this.states.clear(); this.loop.rest(); return; }
      const scroll = !!hooks.loopScopeScroll?.();
      const seen = new Set();
      for (const c of canvases) {
        const slot = Number(c.dataset.slot);
        seen.add(slot);
        let st = this.states.get(slot);
        if (!st) this.states.set(slot, (st = new LoopScopeState()));
        const frame = hooks.scopeFrame?.(slot, scroll ? SCROLL_WINDOW : SWEEP_WINDOW + SWEEP_SEARCH) ?? null;
        if (st.feed(frame, scroll) || !c.dataset.painted) { drawLoopScope(c, st, { scroll }); c.dataset.painted ||= "1"; }   // said once: a write is a change to the page, even of the same value
      }
      for (const k of this.states.keys()) if (!seen.has(k)) this.states.delete(k);
    }
    destroy() { this.loop.stop(); }
  });
}

// ── Indentation guides: native's QScintilla draws one at each indent level
// inside a block (never at column 0), and a blank line takes the deeper of
// its neighbours' ──

const leadingSpaces = (text) => /^ */.exec(text)[0].length;
function guideLevels(doc, n) {
  let indent = leadingSpaces(doc.line(n).text);
  if (doc.line(n).text.trim() === "") {
    const near = (step) => {
      for (let i = n + step, k = 0; i >= 1 && i <= doc.lines && k < 200; i += step, k++) {
        const text = doc.line(i).text;
        if (text.trim() !== "") return leadingSpaces(text);
      }
      return 0;
    };
    indent = Math.max(near(-1), near(1));
  }
  return Math.max(0, Math.floor((indent - 1) / 2));
}

const guideDecos = new Map();
function guideDeco(levels) {
  if (!guideDecos.has(levels)) {
    // one 1px gradient per level, each at its column: 6px is the line's own left padding
    const images = Array(levels).fill("linear-gradient(var(--sp-guide), var(--sp-guide))").join(",");
    const positions = Array.from({ length: levels }, (_, i) => `calc(6px + ${2 * (i + 1)}ch + 0.3ch) 0`).join(",");
    guideDecos.set(levels, Decoration.line({ attributes: { style: `background-image:${images};background-position:${positions};background-size:1px 100%;background-repeat:no-repeat` } }));
  }
  return guideDecos.get(levels);
}

const indentGuides = ViewPlugin.fromClass(class {
  constructor(view) { this.decorations = this.build(view); }
  update(u) { if (u.docChanged || u.viewportChanged) this.decorations = this.build(u.view); }
  build(view) {
    const { doc } = view.state;
    const ranges = [];
    let last = 0;
    for (const { from, to } of view.visibleRanges) {
      for (let n = Math.max(last + 1, doc.lineAt(from).number), end = doc.lineAt(to).number; n <= end; n++) {
        const levels = guideLevels(doc, n);
        if (levels > 0) ranges.push(guideDeco(Math.min(levels, 24)).range(doc.line(n).from));
        last = n;
      }
    }
    return Decoration.set(ranges);
  }
}, { decorations: (p) => p.decorations });

// ── The editor's own toolbar, floating over the code as native's does ─────

// native's editor toolbar glyphs (editortoolbar.cpp, tablericons.h): Tabler's arrow-back-up, arrow-forward-up,
// scissors, copy, clipboard and search, on their 24-unit grid
const EDIT_ICONS = {
  undo: '<path d="M9 14l-4 -4l4 -4"/><path d="M5 10h11a4 4 0 1 1 0 8h-1"/>',
  redo: '<path d="M15 14l4 -4l-4 -4"/><path d="M19 10h-11a4 4 0 1 0 0 8h1"/>',
  cut: '<path d="M3 7a3 3 0 1 0 6 0a3 3 0 1 0 -6 0"/><path d="M3 17a3 3 0 1 0 6 0a3 3 0 1 0 -6 0"/><path d="M8.6 8.6l10.4 10.4"/><path d="M8.6 15.4l10.4 -10.4"/>',
  copy: '<path d="M7 9.667a2.667 2.667 0 0 1 2.667 -2.667h8.666a2.667 2.667 0 0 1 2.667 2.667v8.666a2.667 2.667 0 0 1 -2.667 2.667h-8.666a2.667 2.667 0 0 1 -2.667 -2.667l0 -8.666"/><path d="M4.012 16.737a2.005 2.005 0 0 1 -1.012 -1.737v-10c0 -1.1 .9 -2 2 -2h10c.75 0 1.158 .385 1.5 1"/>',
  paste: '<path d="M9 5h-2a2 2 0 0 0 -2 2v12a2 2 0 0 0 2 2h10a2 2 0 0 0 2 -2v-12a2 2 0 0 0 -2 -2h-2"/><path d="M9 5a2 2 0 0 1 2 -2h2a2 2 0 0 1 2 2a2 2 0 0 1 -2 2h-2a2 2 0 0 1 -2 -2z"/>',
  search: '<path d="M3 10a7 7 0 1 0 14 0a7 7 0 1 0 -14 0"/><path d="M21 21l-6 -6"/>',
};

function editToolbar(view) {
  const bar = document.createElement("div");
  bar.className = "sp-edit-toolbar";
  const selected = () => { const { from, to } = view.state.selection.main; return view.state.sliceDoc(from, to); };
  const write = (text) => { try { navigator.clipboard.writeText(text).catch(() => {}); } catch {} };
  const actions = [
    ["undo", "Undo (Cmd/Ctrl+Z)", () => undo(view)],
    ["redo", "Redo (Cmd/Ctrl+Shift+Z)", () => redo(view)],
    null,
    ["cut", "Cut", () => { const t = selected(); if (!t) return; write(t); view.dispatch({ ...view.state.replaceSelection(""), userEvent: "delete.cut" }); }],
    ["copy", "Copy", () => { const t = selected(); if (t) write(t); }],
    ["paste", "Paste", async () => {
      try {
        const t = await navigator.clipboard.readText();
        if (t) view.dispatch({ ...view.state.replaceSelection(t), userEvent: "input.paste", scrollIntoView: true });
      } catch { /* the browser said no: Cmd/Ctrl+V still pastes */ }
    }],
    ["search", "Find (Cmd/Ctrl+F)", () => openSearchPanel(view)],
  ];
  for (const action of actions) {
    if (!action) { bar.appendChild(Object.assign(document.createElement("span"), { className: "sp-edit-gap" })); continue; }
    const [id, title, run] = action;
    const b = document.createElement("button");
    b.type = "button";
    b.className = `sp-edit-btn sp-edit-${id}`;
    b.title = title;
    b.setAttribute("aria-label", title);
    // what titleKeys() puts the shortcut after: the button's words and native's command
    b.dataset.text = title.replace(/ \(.*\)$/, "");
    b.dataset.command = { undo: "Undo", redo: "Redo", cut: "Cut", copy: "Copy", paste: "Paste", search: "Find" }[id];
    b.innerHTML = `<svg viewBox="0 0 24 24" aria-hidden="true">${EDIT_ICONS[id]}</svg>`;
    b.addEventListener("mousedown", (e) => e.preventDefault());   // the editor keeps its selection
    b.addEventListener("click", async () => { await run(); if (id !== "search") view.focus(); });
    bar.appendChild(b);
  }
  return bar;
}

// Native's ghosting: while a visible line of code reaches under the toolbar it fades right back and lets the pointer
// through to the code, so a big font or a long line can still be read and clicked. Resting the pointer on it a moment
// (native's 450 ms dwell) brings it back until the pointer leaves.
function ghostToolbar(view, bar) {
  let occluded = false, awake = false, dwell = null, queued = false;
  const set = () => {
    const ghost = occluded && !awake;
    bar.classList.toggle("ghost", ghost);
    if (!ghost) clearTimeout(dwell), dwell = null;
  };
  const measure = () => {
    queued = false;
    if (!bar.isConnected || !bar.offsetParent) return;
    const r = bar.getBoundingClientRect(), left = r.left - 8;   // a line that merely kisses the corner counts
    let hit = false;
    for (let y = r.top; y <= r.bottom && !hit; ) {
      const block = view.lineBlockAtHeight(y - view.documentTop);
      if (block.top + view.documentTop > r.bottom) break;
      const end = view.coordsAtPos(block.to, -1);
      if (end && end.right >= left && end.bottom > r.top && end.top < r.bottom) hit = true;
      if (block.to >= view.state.doc.length) break;
      y = view.documentTop + block.bottom + 1;
    }
    if (hit !== occluded) {
      occluded = hit;
      // text arriving under a pointer already resting on the toolbar leaves it awake, as native does
      awake = hit && bar.matches(":hover");
      set();
    }
  };
  const check = () => { if (!queued) { queued = true; requestAnimationFrame(measure); } };
  view.scrollDOM.addEventListener("scroll", check, { passive: true });
  new ResizeObserver(check).observe(view.scrollDOM);
  // a ghosted toolbar is skipped by the pointer, so the dwell is watched from the page (the pointer may leave the
  // editor, not only the toolbar)
  document.addEventListener("pointermove", (e) => {
    if (!occluded) return;
    const r = bar.getBoundingClientRect();
    const over = e.clientX >= r.left && e.clientX <= r.right && e.clientY >= r.top && e.clientY <= r.bottom;
    // woken, it ghosts again once the pointer leaves it (the toolbar may never have seen it arrive)
    if (awake) { if (!over) { awake = false; set(); } return; }
    if (!over) { clearTimeout(dwell); dwell = null; return; }
    dwell ??= setTimeout(() => { awake = true; set(); }, 450);
  });
  check();
  return check;
}

// ── Find: native's FindPopup, a pill at the editor's top right ─────────────
// A Find field with a current/total counter, an Aa toggle (off: smart case, a
// lowercase query matches any case and a capital makes it exact), Enter and
// Shift+Enter for the next and previous match, Escape to close. No replace
// row, as native has none. CodeMirror's search state does the matching.
let forceCase = false;   // the Aa toggle, shared across buffers and openings as native's is
// what a screen reader hears after a find: native's "3 of 12", else how many, else none
function findSpeech(state) {
  const q = getSearchQuery(state);
  if (!q.valid) return "";
  const sel = state.selection.main;
  let total = 0, current = 0;
  for (let c = q.getCursor(state); !c.next().done;) { total++; if (c.value.from === sel.from && c.value.to === sel.to) current = total; }
  return current ? `${current} of ${total}` : total ? `${total} matches` : "No results";
}
const sayFind = (view) => announce(findSpeech(view.state), true, Announcement.Navigation);
const findNextSaid = (view) => { const r = findNext(view); sayFind(view); return r; };
const findPreviousSaid = (view) => { const r = findPrevious(view); sayFind(view); return r; };
function findPanel(view) {
  const make = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };
  const dom = make("div", "cm-search sp-find");
  const glass = make("span", "sp-find-icon");
  glass.innerHTML = `<svg viewBox="0 0 24 24" aria-hidden="true">${EDIT_ICONS.search}</svg>`;
  const input = make("input", "sp-find-input");
  Object.assign(input, { type: "text", placeholder: "Find", spellcheck: false, autocomplete: "off" });
  input.setAttribute("aria-label", "Find");
  const count = make("span", "sp-find-count");
  const aa = make("button", "sp-find-aa", "Aa");
  aa.type = "button";
  aa.title = "Match case exactly (otherwise a lowercase search matches any case)";
  aa.setAttribute("aria-pressed", String(forceCase));
  const prev = make("button", "sp-find-nav"), next = make("button", "sp-find-nav"), close = make("button", "sp-find-close");
  prev.innerHTML = icon("chevron-up"); next.innerHTML = icon("chevron-down"); close.innerHTML = icon("x");   // native's: up for the previous match, down for the next
  for (const [b, t] of [[prev, "Previous match (Shift+Enter)"], [next, "Next match (Enter)"], [close, "Close (Escape)"]]) { b.type = "button"; b.title = t; b.setAttribute("aria-label", t); }
  dom.append(glass, input, count, aa, prev, next, close);

  const query = () => new SearchQuery({ search: input.value, caseSensitive: forceCase || /[A-Z]/.test(input.value) });
  // the match at or after the selection's start, or the first, so the query selects as it is typed
  const select = () => {
    const q = query();
    view.dispatch({ effects: setSearchQuery.of(q) });
    if (!q.valid) return;
    const { state } = view;
    let m = q.getCursor(state, state.selection.main.from).next();
    if (m.done) m = q.getCursor(state, 0).next();
    if (!m.done) view.dispatch({ selection: { anchor: m.value.from, head: m.value.to }, scrollIntoView: true, userEvent: "select.search" });
  };
  const paintCount = () => {
    const q = query();
    if (!q.valid) { count.textContent = ""; return; }
    const { state } = view, sel = state.selection.main;
    let total = 0, current = 0;
    for (let c = q.getCursor(state); !c.next().done;) { total++; if (c.value.from === sel.from && c.value.to === sel.to) current = total; }
    count.textContent = total ? (current ? `${current}/${total}` : String(total)) : "No results";
    count.classList.toggle("none", !total);   // native's no-results state: the count in the accent
  };
  input.addEventListener("input", () => { select(); paintCount(); sayFind(view); });
  input.addEventListener("keydown", (e) => {
    if (e.key === "Enter") { e.preventDefault(); (e.shiftKey ? findPreviousSaid : findNextSaid)(view); }
    else if (e.key === "Escape") { e.preventDefault(); closeSearchPanel(view); view.focus(); }
  });
  aa.addEventListener("click", () => { forceCase = !forceCase; aa.setAttribute("aria-pressed", String(forceCase)); aa.classList.toggle("on", forceCase); select(); paintCount(); input.focus(); });
  prev.addEventListener("click", () => { findPreviousSaid(view); input.focus(); });
  next.addEventListener("click", () => { findNextSaid(view); input.focus(); });
  close.addEventListener("click", () => { closeSearchPanel(view); view.focus(); });
  aa.classList.toggle("on", forceCase);
  return {
    dom,
    top: true,
    mount() {
      // seeded from the selection by openSearchPanel; otherwise the last query, as native keeps it
      input.value = getSearchQuery(view.state).search;
      input.focus();
      input.select();
      paintCount();
    },
    update(u) { if (u.docChanged || u.selectionSet) paintCount(); },
  };
}

const v = (k) => `var(--${k})`;
const editorTheme = EditorView.theme({
  "&": { color: v("DefaultForeground"), backgroundColor: v("Background"), height: "100%", fontSize: "var(--editor-font-size, 18px)" },
  ".cm-scroller": { fontFamily: "var(--code-font)", lineHeight: "1.55" },
  ".cm-content": { caretColor: v("CaretForeground"), paddingBottom: "40vh", paddingTop: "8px" },
  ".cm-cursor, .cm-dropCursor": { borderLeft: `3px solid ${v("CaretForeground")}` },
  "&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground, .cm-selectionBackground": { backgroundColor: v("selectionWash") },
  ".cm-activeLine": { backgroundColor: v("CaretLineBackground") },
  // the caret line's wash paints over the selection layer: while there is a selection it goes, so the selection shows on that line too
  "&.sp-has-selection .cm-activeLine": { backgroundColor: "transparent" },
  ".cm-gutters": { backgroundColor: v("MarginBackground"), color: v("gutterText"), border: "none" },   // native's MarginForeground, legible (theme.js)
  ".cm-lineNumbers .cm-gutterElement": { padding: "0 0.9em 0 1.2em", fontStyle: "italic" },
  ".cm-activeLineGutter": { backgroundColor: v("CaretLineBackground"), color: v("WindowForeground") },
  "&.cm-focused .cm-matchingBracket": { color: v("MatchedBraceForeground"), backgroundColor: v("MatchedBraceBackground") },
  ".cm-searchMatch": { backgroundColor: v("accentTintStrong"), outline: `1px solid ${v("FindMatchBackground")}` },
  // the match the count says (3/5), where Enter goes on from: filled in the accent, the others only outlined
  ".cm-searchMatch.cm-searchMatch-selected": { backgroundColor: v("HighlightedBackground"), outline: `2px solid ${v("HighlightedBackground")}`, borderRadius: "2px" },
  ".cm-searchMatch.cm-searchMatch-selected, .cm-searchMatch.cm-searchMatch-selected *": { color: `${v("accentContrastText")} !important` },
  ".cm-selectionMatch": { backgroundColor: v("accentTint") },
  ".cm-panels": { backgroundColor: v("PaneBackground"), color: v("WindowForeground") },
});

// ── The editor ────────────────────────────────────────────────────────────

/**
 * @param mount element to mount in
 * @param api CompletionAPI
 * @param hooks { run, stop, showDocs(word), onBuffer(i), onCaret(line, position), playNote, playChord, playSample }
 * @param workspace the buffers (workspace.js): the editor shows the one it says, and hands its edits back to it
 */
export function createEditor(mount, { api, hooks, workspace }) {
  let toolbarCheck = null;   // the toolbar's ghosting re-measured (ghostToolbar), once it is mounted
  // each buffer's editor state (its undo with it), by set and buffer: the workspace has the text, this how it was edited
  const states = new Map();
  const keyOf = () => `${workspace.set().id}:${workspace.active}`;
  let shownKey = keyOf(), shownBuffer = workspace.active;
  let killRing = "";
  let flashSeq = 0, flashTimer = 0, kickSeq = 0;

  // native's "Line: 20,  Position: 11": the caret's line, and its column from 1
  const reportCaret = (state) => {
    const head = state.selection.main.head, line = state.doc.lineAt(head);
    hooks.onCaret?.(line.number, head - line.from + 1);
  };

  const wordAtCaret = (state) => {
    const pos = state.selection.main.head;
    const line = state.doc.lineAt(pos);
    const col = pos - line.from;
    const left = /[\w?!]*$/.exec(line.text.slice(0, col))[0];
    const right = /^[\w?!]*/.exec(line.text.slice(col))[0];
    return left + right;
  };

  let nudgeSlider = () => false;   // slider mode's keyboard step (completion/cm.js), once the completion is set up
  const extensions = [
    // Native's shortcuts, in whichever keymap the player picked, are main.js's
    // (shortcuts.js), and run the editor's commands through command() below.
    // These are the keys every keymap leaves to the editor.
    keymap.of([
      // Tab accepts the completion. CodeMirror ignores an accept within 75ms
      // of the list changing, so a fast Tab while the popup is open or on its
      // way retries the accept rather than falling through to indent.
      { key: "Tab", run: (view) => {
        if (acceptCompletion(view)) return true;
        if (completionStatus(view.state)) {
          const retry = (tries) => {
            if (acceptCompletion(view) || !completionStatus(view.state) || tries === 0) return;
            setTimeout(() => retry(tries - 1), 50);
          };
          setTimeout(() => retry(6), 50);
          return true;
        }
        return indentLinesToDepth(view);   // native's: the line to its right depth, never a tab inserted
      } },
      { key: "Shift-Tab", run: indentLess },
      { key: "Enter", run: newlineAndIndent },
      // the list's moves are completion/cm.js's (held at the ends, as native's, and spoken): CodeMirror's wrap
      ...completionKeymap.filter((k) => !["ArrowUp", "ArrowDown", "PageUp", "PageDown"].includes(k.key)),
      // Escape puts away the value's slider, or lets go of the mark (native's SetMark), once the completion popup has
      // had it; Ctrl-G, Emacs's "get out", puts away whatever is up: the completion, the search, the slider, the mark
      { key: "Escape", run: (v) => hideSlider(v) || (marking ? (collapse(), true) : false) },
      { key: "Ctrl-g", run: (v) => { closeCompletion(v); closeSearchPanel(v); hideSlider(v); if (marking) collapse(); return true; } },
      ...searchKeymap,
      ...historyKeymap,
      ...defaultKeymap,
    ]),
    history(),
    drawSelection(),
    loopScopeField,
    loopScopePlugin(hooks),
    waitField,
    lineNumbers(),
    highlightActiveLine(),
    highlightActiveLineGutter(),
    search({ createPanel: findPanel, top: true }),
    highlightSelectionMatches(),
    bracketMatching(),
    indentUnit.of("  "),
    EditorState.tabSize.of(2),
    highlighting,
    indentGuides,
    editorTheme,
    errorLineField,
    flashField,
    kickField,
    completionExtensions(api, { ...hooks, registerNudge: (f) => { nudgeSlider = f; } }),
    EditorView.updateListener.of((u) => {
      if (u.docChanged || u.viewportChanged || u.geometryChanged) toolbarCheck?.();
      if (u.docChanged || u.transactions.some((t) => t.isUserEvent("select.pointer"))) marking = false;   // an edit or a click lets go of the mark
      if (u.selectionSet || u.docChanged) {
        reportCaret(u.state);
        u.view.dom.classList.toggle("sp-has-selection", !u.state.selection.main.empty);
      }
      if (u.docChanged) workspace.edit(shownBuffer, u.state.doc.toString());   // the workspace keeps it
    }),
  ];

  // the editor's name for a screen reader, as native's "Code Editor Buffer 0", and how to leave it (Tab indents here, as in native)
  const named = (i) => EditorView.contentAttributes.of({ "aria-label": `Code Editor Buffer ${i}`, "aria-description": "Tab indents. F6 moves to the next pane, Shift+F6 to the previous." });
  const makeState = (doc, i) => EditorState.create({ doc, extensions: [extensions, named(i)] });
  // the editor lives in a shadow root on its mount (./shadow.js): its flashes and scopes, changing as a program
  // plays, are out of sight of a page-wide watcher (an ad blocker's), whose scans of the page otherwise freeze it
  const root = shadowFor(mount);
  const view = new EditorView({ state: makeState(workspace.text(shownBuffer), shownBuffer), parent: root, root });
  // the line numbers are laid out from line heights the editor measured: whatever changes them without the editor
  // hearing of it (a font arriving late, the page's zoom, a text size set while the editor was out of sight) would
  // leave the numbers drifting from their lines, so any change in the code's own size has it measure again
  new ResizeObserver(() => view.requestMeasure()).observe(view.contentDOM);
  document.fonts?.addEventListener?.("loadingdone", () => view.requestMeasure());
  const toolbar = editToolbar(view);
  root.appendChild(toolbar);
  toolbarCheck = ghostToolbar(view, toolbar);
  reportCaret(view.state);

  // the buffer the workspace says, shown: its own state if it has been shown before (its undo kept), and its text as
  // the workspace has it; code put in the buffer showing from outside (a link, a file) is one undoable change
  function show() {
    const key = keyOf(), i = workspace.active, text = workspace.text(i);
    if (key !== shownKey) {
      states.set(shownKey, view.state);
      view.setState(states.get(key) ?? makeState(text, i));
      const moved = i !== shownBuffer;
      shownKey = key; shownBuffer = i;
      hooks.onBuffer?.(i);
      if (moved) announce(`Buffer ${i}`, false, Announcement.Navigation);
      reportCaret(view.state);
      view.focus();
    }
    if (view.state.doc.toString() !== text) view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: text }, userEvent: "input.replace" });
  }
  workspace.subscribe((e) => { if (e.kind === "buffer" || e.kind === "set") show(); });
  const switchBuffer = (i) => workspace.showBuffer(i);

  const fontSize = () => parseInt(getComputedStyle(document.documentElement).getPropertyValue("--editor-font-size"), 10) || 18;
  function setFontSize(px) {
    px = Math.max(8, Math.min(40, Math.round(px)));
    document.documentElement.style.setProperty("--editor-font-size", `${px}px`);
    store.set(FONT_KEY, String(px));
    view.requestMeasure();
  }
  const saved = parseInt(store.get(FONT_KEY) ?? "", 10);
  if (saved) setFontSize(saved);

  // ── Native's editing commands, by their shortcut ids (./shortcuts.js) ──

  // Emacs's mark, as native's SetMark: the caret's moves select from where it
  // was set, until an edit, a click, Escape, Copy or Cut lets it go
  let marking = false;
  const collapse = () => {
    marking = false;
    view.dispatch({ selection: { anchor: view.state.selection.main.head } });
  };
  const moving = (plain, extend) => (v) => (marking ? extend : plain)(v);
  const lines = (n, plain, extend) => (v) => {
    for (let i = 0; i < n; i++) (marking ? extend : plain)(v);
    return true;
  };
  const selectedText = () => { const { from, to } = view.state.selection.main; return view.state.sliceDoc(from, to); };
  // Cut and Copy go to the clipboard and the kill ring, which Paste falls back on when the browser keeps the clipboard to itself
  const toClipboard = (text) => {
    killRing = text;
    try { navigator.clipboard.writeText(text).catch(() => {}); } catch {}
  };
  const docsWord = () => selectedText().trim() || wordAtCaret(view.state);

  // native's cutLineFromPoint: to the end of the line, or with only space left, the line break too
  function cutToEnd(v) {
    const pos = v.state.selection.main.head, line = v.state.doc.lineAt(pos);
    const to = /^\s*$/.test(v.state.sliceDoc(pos, line.to)) && line.to < v.state.doc.length ? line.to + 1 : line.to;
    if (to > pos) {
      toClipboard(v.state.sliceDoc(pos, to));
      v.dispatch({ changes: { from: pos, to }, userEvent: "delete.cut" });
    }
    return true;
  }

  // native's upcaseWordOrSelection and downcaseWordOrSelection: the selection, or to the end of the word with the caret after it
  function changeCase(v, upper) {
    const range = v.state.selection.main;
    const from = range.from, to = range.empty ? v.moveByGroup(range, true).head : range.to;
    const text = v.state.sliceDoc(from, to);
    const insert = upper ? text.toUpperCase() : text.toLowerCase();
    v.dispatch({ changes: { from, to, insert }, ...(range.empty ? { selection: { anchor: from + insert.length } } : {}), userEvent: "input" });
    marking = false;
    return true;
  }

  async function paste(v) {
    let text = "";
    try { text = await navigator.clipboard.readText(); } catch { /* the browser said no */ }
    text ||= killRing;
    if (text) v.dispatch({ ...v.state.replaceSelection(text), userEvent: "input.paste", scrollIntoView: true });
    marking = false;
  }

  const COMMANDS = {
    Align: reindentBuffer,
    Comment: toggleComment,
    // in the find bar Find searches again, as native's emacs Ctrl+S does there
    Find: (v, { inSearch }) => (inSearch ? findNextSaid(v) : openSearchPanel(v)),
    FindNext: findNextSaid,
    FindPrev: findPreviousSaid,
    Transpose: transposeChars,
    ShiftUp: moveLineUp,
    ShiftDown: moveLineDown,
    SetMark: (v) => {
      marking = true;
      v.dispatch({ selection: { anchor: v.state.selection.main.head } });
      return true;
    },
    TriggerAutocomplete: startCompletion,
    // native's showCompletionDocs: the docs for the completion chosen, or else for the word;
    // a screen reader hears the chosen row's doc (native's announceCompletionDetails)
    // With no list up it is opened at the caret first, as native's does, so the doc read is the one for here.
    ReadCompletionDetails: (v) => {
      const read = () => {
        const c = selectedCompletion(v.state);
        hooks.showDocs?.(c ? c.label.replace(/:$/, "") : docsWord());
        const said = c && completionDocText(c);
        if (said) announce(said, true, Announcement.Navigation);
      };
      if (completionStatus(v.state) === "active") { read(); return true; }
      startCompletion(v);
      // the list comes a moment later (its source runs): read once it is there, or the word's docs if none comes
      let tries = 0;
      const wait = () => (completionStatus(v.state) === "active" || ++tries > 20 ? read() : requestAnimationFrame(wait));
      requestAnimationFrame(wait);
      return true;
    },
    ContextualDocs: () => (hooks.showDocs?.(docsWord()), true),
    // while the completion popup is up, Down and Up move through it
    // (a slider over the value: Down and Up step it, as the arrows do)
    Down: (v) => nudgeSlider(v, -1) || moveCompletion(v, 1) || moving(cursorLineDown, selectLineDown)(v),
    Up: (v) => nudgeSlider(v, 1) || moveCompletion(v, -1) || moving(cursorLineUp, selectLineUp)(v),
    DownTen: lines(10, cursorLineDown, selectLineDown),
    UpTen: lines(10, cursorLineUp, selectLineUp),
    CutToEnd: cutToEnd,
    Copy: () => {
      const t = selectedText();
      if (t) toClipboard(t);
      collapse();
      return true;
    },
    Cut: (v) => {
      const t = selectedText();
      if (t) {
        toClipboard(t);
        v.dispatch({ ...v.state.replaceSelection(""), userEvent: "delete.cut" });
      }
      marking = false;
      return true;
    },
    Paste: (v) => (paste(v), true),
    Right: moving(cursorCharForward, selectCharForward),
    Left: moving(cursorCharBackward, selectCharBackward),
    DeleteForward: deleteCharForward,
    DeleteBackward: deleteCharBackward,
    LineStart: moving(cursorLineBoundaryBackward, selectLineBoundaryBackward),
    LineEnd: moving(cursorLineBoundaryForward, selectLineBoundaryForward),
    DocStart: moving(cursorDocStart, selectDocStart),
    DocEnd: moving(cursorDocEnd, selectDocEnd),
    WordRight: moving(cursorGroupForward, selectGroupForward),
    WordLeft: moving(cursorGroupBackward, selectGroupBackward),
    SelectLineStart: selectLineBoundaryBackward,
    SelectLineEnd: selectLineBoundaryForward,
    SelectWordRight: selectGroupForward,
    SelectWordLeft: selectGroupBackward,
    SelectDocStart: selectDocStart,
    SelectDocEnd: selectDocEnd,
    CenterVertically: (v) => (v.dispatch({ effects: EditorView.scrollIntoView(v.state.selection.main.head, { y: "center" }) }), true),
    Undo: undo,
    Redo: redo,
    SelectAll: selectAll,
    DeleteWordRight: deleteGroupForward,
    DeleteWordLeft: deleteGroupBackward,
    UpcaseWord: (v) => changeCase(v, true),
    DowncaseWord: (v) => changeCase(v, false),
  };

  return {
    view,
    /** The editor steps aside (a page over it): its completion and its focus go, so nothing of it floats over what covers it. */
    aside() { closeCompletion(view); view.contentDOM.blur(); },
    /** Runs one of native's editing commands by its shortcut id (./shortcuts.js); false when it is not the editor's. */
    command(id, context = {}) {
      const run = COMMANDS[id];
      if (!run) return false;
      run(view, context);
      return true;
    },
    /** The edit toolbar's titles with their shortcuts: title(text, command) gives each one. */
    titleKeys(title) {
      for (const b of root.querySelectorAll(".sp-edit-btn")) {
        b.title = title(b.dataset.text, b.dataset.command);
        b.setAttribute("aria-label", b.title);
      }
    },
    get active() { return workspace.active; },
    get size() { return workspace.size; },
    switchBuffer,
    /** Buffer i's text, the current one's live, the others' as last left. */
    bufferText: (i) => workspace.text(i),
    getCode: () => view.state.doc.toString(),
    /** Replaces the buffer's text as one undoable change. */
    setCode(code) {
      view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: code }, userEvent: "input.replace" });
    },
    insertAtCursor(code) {
      const { state } = view;
      const pos = state.selection.main.head;
      const line = state.doc.lineAt(pos);
      const atLineStart = pos === line.from;
      const text = (atLineStart ? "" : "\n") + code.replace(/\n?$/, "\n");
      view.dispatch({ changes: { from: pos, insert: text }, selection: { anchor: pos + text.length }, scrollIntoView: true, userEvent: "input" });
      view.focus();
    },
    /** The error's line lit, and where on it (from, to: columns) underlined; the view goes to it. */
    markError(line, from, to) {
      view.dispatch({ effects: setErrorLine.of(from != null ? { line, from, to } : line) });
      if (line) view.dispatch({ effects: EditorView.scrollIntoView(view.state.doc.line(Math.min(Math.max(1, line), view.state.doc.lines)).from, { y: "center" }) });
    },
    /** The caret to line, column (1-based line, 0-based column), the editor focused. */
    goTo(line, col = 0) {
      const l = view.state.doc.line(Math.min(Math.max(1, line), view.state.doc.lines));
      view.dispatch({ selection: { anchor: l.from + Math.min(col, l.length) }, scrollIntoView: true });
      view.focus();
    },
    /** A line's text as it is now (1-based), or null past the end. */
    lineText: (line) => (line >= 1 && line <= view.state.doc.lines ? view.state.doc.line(line).text : null),
    /** An error card's fix: { line, from, to, insert } (columns in the line) as one undoable edit, the caret after it. */
    applyFix({ line, from, to, insert }) {
      const l = view.state.doc.line(line);
      const a = l.from + from, b = l.from + to;
      view.dispatch({ changes: { from: a, to: b, insert }, selection: { anchor: a + insert.length }, scrollIntoView: true, userEvent: "input" });
      view.focus();
    },
    clearError() { view.dispatch({ effects: setErrorLine.of(null) }); },
    /** The run's kick: the line numbers flare and thump like a beat (style.css), the code left as it is. Each Run
     * starts it again, however fast they come. */
    flash() {
      clearTimeout(flashTimer);
      view.dom.classList.remove("sp-run-flash");
      void view.dom.offsetWidth;
      view.dom.classList.add("sp-run-flash");
      const id = ++kickSeq;
      view.dispatch({ effects: kickCode.of({ id, flip: id % 2 === 0 }) });
      flashTimer = setTimeout(() => { view.dom.classList.remove("sp-run-flash"); view.dispatch({ effects: dropKick.of(id) }); }, 320);
    },
    /** A line flashes, as native's code flash. */
    flashLine(line) {
      const id = ++flashSeq;
      view.dispatch({ effects: addFlash.of({ line, id }) });
      setTimeout(() => view.dispatch({ effects: dropFlash.of({ line, id }) }), 500);
    },
    /** The live loops with a scope stream, drawn after their header lines: [{ name, slot }] (./loopscope.js). */
    setLoopScopes(list) { view.dispatch({ effects: setLoopScopes.of(list) }); },
    /** The buffer's lines with a thread held on a sync: [{line, key, who}]. */
    setWaits(list) { view.dispatch({ effects: setWaits.of(list) }); },
    goToLine(line) {
      const n = Math.min(Math.max(1, line), view.state.doc.lines);
      const l = view.state.doc.line(n);
      view.dispatch({ selection: { anchor: l.from, head: l.to }, effects: EditorView.scrollIntoView(l.from, { y: "center" }) });
      view.focus();
    },
    fontSize,
    setFontSize,
    focus: () => view.focus(),
  };
}
