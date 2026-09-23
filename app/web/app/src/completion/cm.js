// SPDX-License-Identifier: AGPL-3.0-or-later
// Native Sonic Pi's completion popup (app/gui/widgets/completion_popup.cpp),
// on CodeMirror's autocomplete: what to offer comes from ./api.js; the popup
// ranks it with native's fuzzy tiers and shows native's detail pane — the
// summary, a usage card, the doc — with its modes: notes on a piano, chords
// and scales lit on it, synths and FX to audition, samples to play. Native's
// slider mode is a slider over the value under the caret, which edits the
// buffer as it moves.
import { autocompletion, startCompletion, completionStatus, currentCompletions, selectedCompletion, selectedCompletionIndex, setSelectedCompletion, acceptCompletion, closeCompletion } from "@codemirror/autocomplete";
import { announce, Announcement } from "../announce.js";
import { EditorView, ViewPlugin, showTooltip, tooltips, keymap } from "@codemirror/view";
import { StateField, StateEffect, Prec } from "@codemirror/state";
import { scanLineToCaret, caretAfterClosedValue, fuzzyMatch, tokenEndAtCaret, lineToContext } from "./context.js";
import { synthAt } from "./api.js";
import { renderCode } from "../highlight.js";

import { piano, noteName, BLACK } from "../ui/piano.js";   // the one keyboard, swept as well as pressed
export { piano, noteName };

// Slider mode's slider put away (Escape, Ctrl-G): gone while the caret stays on the value it was over
const dismissSlider = StateEffect.define();
// Tab takes hold of the slider (the keys step it) and lets go again: until it is held it rests faded, and the arrows
// are the code's
const holdSlider = StateEffect.define();
/** The slider put away, if one is up: true when it was (the key is taken), false when there was none. */
export function hideSlider(view) {
  if (!view.dom.querySelector(".sp-slider")) return false;
  view.dispatch({ effects: dismissSlider.of(null) });
  return true;
}

// A phone (style.css's one-column layout): the detail pane cannot sit beside
// the list, and CodeMirror's fallback tucks it under the selected row, over the
// rest of the list. Here it goes under the whole list — or over it when there
// is more room there — the width of the screen, as tall as the room allows.
// Tooltips are kept to the editor's own box on a phone, so neither the list nor
// the pane runs under the docked keyboard (keyboard.js) or the panel below.
const PHONE = () => window.matchMedia("(max-width: 760px)").matches;
const INFO_MARGIN = 8, INFO_GAP = 4;

function tooltipSpace(view) {
  const doc = view.dom.ownerDocument.documentElement;
  if (!PHONE()) return { top: 0, left: 0, bottom: doc.clientHeight, right: doc.clientWidth };
  const r = view.scrollDOM.getBoundingClientRect();
  return { top: r.top, left: 0, bottom: r.bottom, right: doc.clientWidth };
}

function positionInfo(view, list, option, info, space, tooltip) {
  const listH = list.bottom - list.top, listW = list.right - list.left;
  const scaleY = listH / tooltip.offsetHeight, scaleX = listW / tooltip.offsetWidth;
  const infoW = info.right - info.left, infoH = info.bottom - info.top;
  if (PHONE()) {
    // the band the pane keeps clear of: the list and the line being typed on (the list
    // hangs off that line, so a pane stopping at the list's edge would cover it)
    const caret = view.coordsAtPos(view.state.selection.main.head);
    const bandTop = Math.min(list.top, caret?.top ?? list.top), bandBottom = Math.max(list.bottom, caret?.bottom ?? list.bottom);
    const below = space.bottom - bandBottom, above = bandTop - space.top;
    const under = below >= Math.min(infoH, 120) || below >= above;
    const height = Math.max(72, (under ? below : above) - INFO_MARGIN - INFO_GAP);
    const offset = under ? bandBottom - list.top : list.bottom - bandTop;
    const left = space.left + INFO_MARGIN - list.left, width = space.right - space.left - 2 * INFO_MARGIN;
    return {
      style: `${under ? "top" : "bottom"}: ${(offset + INFO_GAP) / scaleY}px; left: ${left / scaleX}px; width: ${width / scaleX}px; max-width: none; max-height: ${height / scaleY}px`,
      class: under ? "sp-cinfo-below" : "sp-cinfo-above",
    };
  }
  // CodeMirror's own placement (autocomplete's defaultPositionInfo, which it does not export):
  // beside the list on whichever side has the room, else narrow, under or over the selected row
  let left = false, narrow = false, side = "top", offset, maxWidth;
  const spaceLeft = list.left - space.left, spaceRight = space.right - list.right;
  if (spaceRight < Math.min(infoW, spaceLeft)) left = true;
  if (infoW <= (left ? spaceLeft : spaceRight)) {
    offset = -2;   // native's: one box with the list — the pane's top on the list's top border, its height the list's (below), so the two tops are one line
    maxWidth = Math.min(400, left ? spaceLeft : spaceRight);
  } else {
    narrow = true;
    maxWidth = Math.min(400, space.right - list.left - 30);
    const spaceBelow = space.bottom - list.bottom;
    if (spaceBelow >= infoH || spaceBelow > list.top) offset = option.bottom - list.top;
    else { side = "bottom"; offset = list.bottom - option.top; }
  }
  // the pane's own height from its content (not its box, which the last row's height set)
  const pane = tooltip.querySelector(".cm-completionInfo"), words = pane?.querySelector(".sp-cinfo-scroll"), acts = pane?.querySelector(".sp-cinfo-actions");
  const cs = pane && getComputedStyle(pane);
  const natural = words ? words.scrollHeight + (acts?.offsetHeight ?? 0) + parseFloat(cs.borderTopWidth) + parseFloat(cs.borderBottomWidth) : infoH;
  // a list of a row or two beside a tall pane would be stretched to an empty column: the pane goes under the list
  // instead, the two the same width, one box (over it when there is more room there)
  const rows = tooltip.querySelectorAll(":scope > ul > li").length;
  if (rows <= 2 && natural > listH * 1.5) {
    // the box widened to the pane's width first (its inside: min-width), then measured as it now is — list, the
    // box's outer edge from before, may be the last placement's width
    const bw = parseFloat(getComputedStyle(tooltip).borderLeftWidth) || 0;
    tooltip.style.minHeight = "";
    tooltip.style.minWidth = `${(Math.min(440, space.right - list.left - INFO_MARGIN) - 2 * bw) / scaleX}px`;
    const box = tooltip.getBoundingClientRect();
    const below = space.bottom - box.bottom, above = box.top - space.top;
    const under = below >= Math.min(natural, 240) || below >= above;
    const h = Math.min(natural, 470, (under ? below : above) - INFO_MARGIN);
    // placed from the box's inside: its sides on the box's sides, its top border lying on the box's bottom one
    return {
      style: `${under ? "top" : "bottom"}: ${(box.height - 2 * bw) / scaleY}px; left: ${-bw / scaleX}px; width: ${box.width / scaleX}px; max-width: none; height: ${h / scaleY}px; max-height: none`,
      class: under ? "sp-cinfo-under" : "sp-cinfo-over",
    };
  }
  tooltip.style.minWidth = "";
  // beside the list, as native's one box: the two the same height — the taller of the list and the pane (the pane's
  // own capped, then scrolling) — the list stretched to it when the pane is the taller
  if (!narrow) {
    const h = Math.max(listH, Math.min(natural, 470));
    tooltip.style.minHeight = `${h / scaleY}px`;
    return { style: `${side}: ${offset / scaleY}px; max-width: ${maxWidth / scaleX}px; height: ${h / scaleY}px; max-height: none`, class: "cm-completionInfo-" + (left ? "left" : "right") };
  }
  tooltip.style.minHeight = "";
  return {
    style: `${side}: ${offset / scaleY}px; max-width: ${maxWidth / scaleX}px`,
    class: "cm-completionInfo-" + (narrow ? "right-narrow" : left ? "left" : "right"),
  };
}

/** Native's slider grid: two decimal places finer than the range's magnitude. */
export const gridFor = (lo, hi) => Math.pow(10, Math.floor(Math.log10(hi - lo)) - 2);
export function formatValue(value, lo, hi) {
  const decimals = Math.max(0, -(Math.floor(Math.log10(hi - lo)) - 2));
  return String(Number(Number(value).toFixed(decimals)));
}

const el = (tag, cls, text) => {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text != null) e.textContent = text;
  return e;
};

const miniButton = (label, onClick) => {
  const b = el("button", "sp-mini-btn", label);
  b.type = "button";
  b.addEventListener("mousedown", (e) => { e.preventDefault(); onClick(); });
  return b;
};

/**
 * Native's detail pane for one row. CodeMirror names it as the row's
 * description, so everything in it is hidden from a screen reader (native
 * prunes its pane from the accessibility tree too): the row itself says the
 * name, kind and summary, and Ctrl+I reads the doc (completionDocText).
 */
function detailFor(it, hooks) {
  // A cue has no documentation: its name is already the row, and repeating it in the panel says nothing. What is
  // worth knowing is what it last carried, so that is the panel — and where there is nothing to say, there is no
  // panel at all.
  if (it.kind === "cue") {
    const last = hooks.cueValue?.(it.text);
    if (last == null || last === "") return null;
    const root = el("div", "sp-cinfo");
    root.appendChild(el("div", "sp-cinfo-title", "last carried"));
    root.appendChild(el("div", "sp-cinfo-cue-val", String(last)));
    return root;
  }
  const root = el("div", "sp-cinfo");
  // an opt's summary is its doc's first sentence (api.js optSummary), which the doc below says: its title is its name
  const title = it.kind === "note" ? `${it.text}  ·  ${it.summary}` : it.kind === "opt" ? it.text : (it.summary || it.text);
  root.appendChild(el("div", "sp-cinfo-title", title));
  const web = hooks.webSupport?.(it.text);   // a function the web build does not have: said before anything else
  if (web) root.appendChild(el("p", "doc-unsupported sp-cinfo-web", `${web.kind === "never" ? "Never on the web" : "Not on the web yet"}: ${web.reason}.`));
  if (it.usage) {
    const card = el("div", "sp-cinfo-usage");
    card.style.setProperty("--chars", Math.max(...String(it.usage).split("\n").map((l) => l.length), 1));   // its longest line: the code sized to fit it (style.css)
    card.appendChild(renderCode(it.usage));
    root.appendChild(card);
  }
  const actions = el("div", "sp-cinfo-actions");
  if (it.kind === "note" && it.intervals) {
    const tonic = it.note;
    const notes = it.intervals.map((i) => tonic + i);
    root.appendChild(piano({ from: tonic - (tonic % 12), to: Math.max(tonic - (tonic % 12) + 23, Math.max(...notes) + (11 - (Math.max(...notes) % 12))), lit: new Set(notes), root: tonic, onKey: (n) => hooks.playNote?.(n) }));
  } else if (it.kind === "note") {
    const base = it.note - (it.note % 12);
    root.appendChild(piano({ from: base - 12, to: base + 23, lit: new Set([it.note]), root: it.note, onKey: (n) => hooks.playNote?.(n) }));
  } else if ((it.kind === "chord" || it.kind === "scale") && it.intervals) {
    const tonic = it.note ?? 60;
    const notes = it.intervals.map((i) => tonic + i);
    const top = Math.max(...notes);
    root.appendChild(piano({ from: tonic - (tonic % 12), to: Math.max(tonic - (tonic % 12) + 23, top + (11 - (top % 12))), lit: new Set(notes), root: tonic, onKey: (n) => hooks.playNote?.(n) }));
    actions.appendChild(miniButton(it.kind === "chord" ? "▶ Play chord" : "▶ Play scale", () => (it.kind === "chord" ? hooks.playChord : hooks.playScale)?.(notes)));
  } else if (it.kind === "synth") {
    // its keys are how it is heard: pressed, or swept across
    const name = it.text.replace(/^:/, "");
    root.appendChild(piano({ from: 48, to: 71, onKey: (n) => hooks.playNote?.(n, { synth: name }) }));
    actions.appendChild(miniButton("Docs", () => hooks.showDocs?.(it.text, "synths")));
  } else if (it.kind === "fx") {
    // and an FX's, a note played through it
    const name = it.text.replace(/^:/, "");
    root.appendChild(piano({ from: 48, to: 71, onKey: (n) => hooks.playNote?.(n, { fx: name }) }));
    actions.appendChild(miniButton("Docs", () => hooks.showDocs?.(it.text, "fx")));
  } else if (it.kind === "sample") {
    actions.appendChild(miniButton("▶ Play", () => hooks.playSample?.(it.text.replace(/^:/, ""))));
  } else if (it.kind === "range") {
    const bar = el("div", "sp-rangebar");
    const mark = el("i");
    mark.style.left = `${((it.rdefault - it.rmin) / (it.rmax - it.rmin)) * 100}%`;
    bar.appendChild(mark);
    root.appendChild(el("div", "sp-cinfo-range", `${formatValue(it.rmin, it.rmin, it.rmax)} … ${formatValue(it.rmax, it.rmin, it.rmax)}`));
    root.appendChild(bar);
  } else if (it.kind === "fn") {
    actions.appendChild(miniButton("Docs ↗", () => hooks.showDocs?.(it.text, "lang")));
  }
  if (it.doc) {
    const doc = el("div", "sp-doc sp-cinfo-doc");
    doc.innerHTML = it.doc;
    root.appendChild(doc);
  }
  // after the doc, as native's pane ends with its Docs button
  // the words scroll on their own; the actions stand in a row of their own under them, never over them (native's)
  const scroll = el("div", "sp-cinfo-scroll");
  scroll.append(...root.childNodes);
  root.appendChild(scroll);
  if (actions.childElementCount) root.appendChild(actions);
  for (const c of root.children) c.setAttribute("aria-hidden", "true");
  for (const b of root.querySelectorAll("button")) b.tabIndex = -1;
  return root;
}

/**
 * The doc behind one row as plain text, for a screen reader on demand (native's
 * CompletionPopup::currentDoc): the usage line first, then the doc's words.
 */
export function completionDocText(completion) {
  const it = completion?.data;
  if (!it) return "";
  let text = it.usage ? `Usage: ${it.usage}. ` : "";
  if (it.doc) {
    const div = document.createElement("div");
    // a pause where the doc breaks: its paragraphs, rows and headings, and each opt's name from its doc
    div.innerHTML = it.doc.replace(/<\/(p|li|tr|div|h[1-6]|dt|dd|pre)>/gi, ". $&").replace(/<\/(td|th)>|<br\s*\/?>/gi, ", $&");
    text += div.textContent.replace(/\s+/g, " ").replace(/(\s*[.,])+\s*\./g, ".").replace(/,\s*\./g, ".").replace(/^[\s.,]+/, "");
  }
  return text.trim();
}

/** The list's selection moved by `delta` rows, as native's moveSelection: held at the ends (it does not wrap), and
 * nothing changes there, so nothing is said. True when a list is up (the key is taken either way). */
export function moveCompletion(view, delta) {
  if (completionStatus(view.state) !== "active") return false;
  const n = currentCompletions(view.state).length, at = selectedCompletionIndex(view.state) ?? 0;
  const to = Math.max(0, Math.min(n - 1, at + delta));
  if (to !== at) view.dispatch({ effects: setSelectedCompletion(to) });
  return true;
}

/** What a screen reader hears for a row, as native's CompletionPopup::currentAnnouncement: "prophet, synth, The
 * Prophet, 1 of 5" — its name, kind, summary (the detail it cannot see in the docs pane) and its place in the list. */
export function rowAnnouncement(completion, at, total) {
  const it = completion.data;
  const parts = [completion.label, KIND_LABEL[completion.type] ?? ""];
  if (it?.summary && it.summary !== completion.label) parts.push(it.summary);
  parts.push(`${at + 1} of ${total}`);
  return parts.filter(Boolean).join(", ");
}

// Each row's kind as native's icon: λ for a function, and one for each other kind
const KIND_ICON = { fn: "λ", opt: ":", optval: "=", range: "↔", note: "♪", synth: "∿", fx: "≈", sample: "▸", chord: "♫", scale: "♫", tuning: "♮", example: "✎", port: "⇄", cue: "⚑" };
const KIND_LABEL = { fn: "function", opt: "opt", optval: "value", range: "range", note: "note", synth: "synth", fx: "fx", sample: "sample", chord: "chord", scale: "scale", tuning: "tuning", example: "example", port: "MIDI port", cue: "cue" };

// Native's note list: a keyboard under the rows, in the popup itself (completionpopup.cpp NotePiano). Two octaves
// from a C, labelled at each C, the selected note lit and the notes the list holds washed; ‹ › and the wheel move it
// an octave; a key hovered selects its row, a key pressed puts its note in (a note the list has not got as well —
// the keyboard covers more than a filtered list). Only while the list is notes.
const PIANO_SPAN = 24;
const notePiano = ViewPlugin.fromClass(class {
  constructor(view) { this.view = view; this.start = null; this.key = ""; this.foot = null; }
  update(u) { if (u.docChanged || u.selectionSet || u.transactions.length) this.view.requestMeasure({ read: () => null, write: () => this.sync() }); }
  destroy() { this.foot?.remove(); }
  notes() {
    const opts = currentCompletions(this.view.state);
    return opts.every((o) => o.type === "note" && Number.isFinite(o.data?.note) && !o.data?.intervals) && opts.length > 1 ? opts : null;
  }
  sync() {
    const view = this.view, tip = view.dom.querySelector(".cm-tooltip-autocomplete");
    const opts = tip && completionStatus(view.state) === "active" ? this.notes() : null;
    if (!opts) { this.foot?.remove(); this.foot = null; this.start = null; this.key = ""; return; }
    const sel = selectedCompletion(view.state)?.data?.note ?? 60;
    // the window keeps still while the lit note is in it, and moves by octaves to reach it
    if (this.start == null || sel < this.start || sel > this.start + PIANO_SPAN) this.start = Math.max(0, Math.min(127 - PIANO_SPAN, sel - (sel % 12) - 12));
    const inList = new Set(opts.map((o) => o.data.note));
    const key = `${this.start}|${inList.size}|${[...inList][0]}|${tip.clientWidth}`;
    if (this.foot?.isConnected && key === this.key) return this.light(sel);   // the same keys: only the lit one moves, so the key under the pointer stays itself
    this.key = key;
    if (!this.foot || this.foot.parentNode !== tip) { this.foot?.remove(); this.foot = el("div", "sp-cpiano"); tip.appendChild(this.foot); }
    this.render(opts, sel, inList);
  }
  render(opts, sel, inList) {
    const view = this.view, foot = this.foot, from = this.start, to = from + PIANO_SPAN;
    foot.textContent = "";
    const shift = (by) => { this.start = Math.max(0, Math.min(127 - PIANO_SPAN, this.start + by)); this.key = ""; this.sync(); };
    const arrow = (dir) => {
      const b = el("button", "sp-cpiano-nav", dir < 0 ? "◀" : "▶");
      b.type = "button"; b.tabIndex = -1; b.title = dir < 0 ? "An octave down" : "An octave up";
      b.disabled = dir < 0 ? from === 0 : to >= 127;
      b.addEventListener("mousedown", (e) => { e.preventDefault(); shift(12 * dir); });
      return b;
    };
    // the keys as wide as the popup allows: two octaves are fifteen white keys, the arrows either side
    const tipW = foot.parentNode.clientWidth || 300, W = Math.max(14, Math.min(30, Math.floor((tipW - 64) / 15)));
    const keys = piano({ from, to, lit: new Set([sel]), root: sel, onKey: (n) => this.press(n, opts), keyWidth: W });
    // the list's notes washed — unless every key is one of them, when the wash says nothing (native skips it too)
    const all = [...keys.children].every((k) => inList.has(Number(k.dataset.note)));
    if (!all) for (const k of keys.children) if (inList.has(Number(k.dataset.note))) k.classList.add("in-list");
    keys.addEventListener("mouseover", (e) => {
      const n = Number(e.target.closest?.(".sp-key")?.dataset.note);
      const i = opts.findIndex((o) => o.data.note === n);
      if (i >= 0 && completionStatus(view.state) === "active" && selectedCompletion(view.state) !== opts[i]) view.dispatch({ effects: setSelectedCompletion(i) });   // never a list that has closed
    });
    const board = el("div", "sp-cpiano-board");
    const labels = el("div", "sp-cpiano-labels");
    let white = 0;
    for (let n = from; n <= to; n++) {
      if (BLACK.has(n % 12)) continue;
      if (n % 12 === 0) { const l = el("span", n === sel - (sel % 12) ? "here" : "", `C${n / 12 - 1}`); l.style.left = `${white * W + W / 2}px`; labels.appendChild(l); }
      white++;
    }
    labels.style.width = keys.style.width;   // the labels' row the keys' own width, so each C's label sits under its key
    board.append(keys, labels);
    foot.append(arrow(-1), board, arrow(1));
    foot.onwheel = (e) => { if (Math.abs(e.deltaY) < 4 && Math.abs(e.deltaX) < 4) return; e.preventDefault(); shift((e.deltaY || e.deltaX) > 0 ? 12 : -12); };
    foot.onmousedown = (e) => { e.preventDefault(); e.stopPropagation(); };   // the editor keeps its focus and its caret; the list's own mousedown (which reads a list a key may just have closed) never hears it
  }
  light(sel) {
    for (const k of this.foot.querySelectorAll(".sp-key")) { const on = Number(k.dataset.note) === sel; k.classList.toggle("lit", on); k.classList.toggle("root", on); }
    const c = sel - (sel % 12);
    for (const l of this.foot.querySelectorAll(".sp-cpiano-labels span")) l.classList.toggle("here", l.textContent === `C${c / 12 - 1}`);
  }
  // a key pressed: its row accepted, or — a note the list has not got — its number put in place of the word typed
  press(n, opts) {
    const view = this.view, i = opts.findIndex((o) => o.data.note === n);
    if (completionStatus(view.state) !== "active") return;
    if (i >= 0) { if (selectedCompletion(view.state) !== opts[i]) view.dispatch({ effects: setSelectedCompletion(i) }); acceptCompletion(view); return; }
    const head = view.state.selection.main.head, line = view.state.doc.lineAt(head);
    const word = /[\w:]*$/.exec(line.text.slice(0, head - line.from))[0];
    const insert = String(n);
    view.dispatch({ changes: { from: head - word.length, to: head, insert }, selection: { anchor: head - word.length + insert.length }, userEvent: "input.complete" });
    closeCompletion(view);
  }
});

export function completionExtensions(api, hooks = {}) {
  const resolveSynthAt = (state, pos) => api.setSynthResolver(() => synthAt(state.sliceDoc(0, pos), pos));

  const toOption = (it) => ({
    label: it.text,
    detail: it.kind === "note" || it.kind === "optval" ? it.summary : "",
    type: it.kind,
    data: it,
    info: it.kind === "note" && !it.intervals ? undefined : () => detailFor(it, hooks),   // a plain note's keyboard is the list's own (notePiano)
    apply: (view, _completion, from, to) => {
      const chain = it.kind === "opt" || (it.kind === "fn" && it.text.endsWith(":"));
      const insert = chain ? `${it.text} ` : it.text;
      view.dispatch({ changes: { from, to, insert }, selection: { anchor: from + insert.length }, userEvent: "input.complete" });
      if (chain) setTimeout(() => startCompletion(view), 0);
    },
  });

  const source = (ctx) => {
    const { state, pos } = ctx;
    const line = state.doc.lineAt(pos);
    const col = pos - line.from, text = line.text;
    const scan = scanLineToCaret(text, col);
    if (scan.inComment || caretAfterClosedValue(text, col)) return null;
    if (!ctx.explicit && text[col - 1] === ",") return null;   // right after a comma nothing is offered yet: the space comes first, as native waits for a word to begin
    resolveSynthAt(state, pos);
    const { context, items } = api.completionsAt(text, col);
    if (!items.length) return null;
    const kind = items[0].kind;
    if (scan.inString && !["cue", "port", "sample"].includes(kind)) return null;
    let partial = context[context.length - 1] ?? "";
    const end = tokenEndAtCaret(text, col);
    let start = end - partial.length;
    if (start < 0 || start > col) start = col;
    // inside a string the partial carries the quote that opened it (`sync "/ao` gives `"/ao`), and nothing on
    // offer begins with a quote: the word being matched, and replaced, is what follows it
    if (scan.inString && (partial.startsWith('"') || partial.startsWith("'"))) { start += 1; partial = partial.slice(1); }
    const typed = text.slice(start, col);
    if (!ctx.explicit && kind === "fn" && typed.length < 2) return null;
    if (kind === "range") {
      // only when asked for (Tab): a value already written needs no completing, and a popup that arrives by itself
      // over the number being typed is in the way — on a phone especially, where there is no Escape to send it away
      if (!ctx.explicit) return null;
      const it = items[0];
      const value = formatValue(it.rdefault, it.rmin, it.rmax);
      return {
        from: line.from + start, to: line.from + end, filter: false,
        options: [{ label: value, detail: `${it.text} ${formatValue(it.rmin, it.rmin, it.rmax)} … ${formatValue(it.rmax, it.rmin, it.rmax)}`, type: "range", data: it, info: () => detailFor(it, hooks), apply: value }],
      };
    }
    const scored = [];
    const notes = new Set();   // a note matched by its number and by its name (53 and :f3) is offered once
    for (const it of items) {
      const score = typed ? fuzzyMatch(typed, it.text) : 0;
      if (score === null) continue;
      if (it.kind === "note") {
        const n = /^\d+$/.test(it.text) ? it.text : it.summary;
        if (notes.has(n)) continue;
        notes.add(n);
      }
      scored.push([score, it]);
    }
    if (!scored.length) return null;
    // the one word offered is the one already written (play 42, only 42): taking it would change nothing, so the popup
    // is only in the way. Asked for (Tab), it still shows, for the name beside it.
    if (!ctx.explicit && scored.length === 1 && scored[0][1].text === text.slice(start, end)) return null;
    if (typed) scored.sort((a, b) => b[0] - a[0]);
    return { from: line.from + start, to: line.from + end, filter: false, options: scored.slice(0, 400).map(([, it]) => toOption(it)) };
  };

  // Native opens the popup as soon as a slot has something to offer: after
  // `sample `, `use_synth `, `play `, an opt key. Function names wait for typing.
  const trigger = EditorView.updateListener.of((u) => {
    if (!u.docChanged || !u.transactions.some((tr) => tr.isUserEvent("input.type"))) return;
    const pos = u.state.selection.main.head;
    const ch = u.state.sliceDoc(pos - 1, pos);
    if (ch !== " " && ch !== "," && ch !== "(") return;
    if (completionStatus(u.state)) return;
    const line = u.state.doc.lineAt(pos);
    const scan = scanLineToCaret(line.text, pos - line.from);
    if (scan.inComment || scan.inString) return;
    resolveSynthAt(u.state, pos);
    const { items } = api.completionsAt(line.text, pos - line.from);
    if (items.length && items[0].kind !== "fn") setTimeout(() => startCompletion(u.view), 0);
  });

  // ── A screen reader, as native has it (completionpopup.cpp) ──
  // The popup is not in the accessibility tree: focus, and the reader's attention, stay in the code, so every
  // character typed is still echoed, and the list never takes over as a "listbox" that re-reads itself on each
  // letter. What the reader hears instead is said, assertively, each time the selection is moved (Up, Down, Page
  // Up, Page Down, Ctrl-N, Ctrl-P): the row's name, kind, summary and place, and nothing at the ends of the list,
  // where the selection does not move. The list opening, or narrowing as you type, says nothing: the typing is what
  // is heard. Ctrl-I reads the chosen row's usage and doc (editor.js ReadCompletionDetails).
  // CodeMirror's own is the ARIA combobox (aria-activedescendant on the code), which a reader follows, or not,
  // as it will: Safari's VoiceOver on an editable area barely does.
  const COMBOBOX = ["aria-activedescendant", "aria-controls", "aria-haspopup", "aria-autocomplete", "aria-expanded"];
  const unlist = ViewPlugin.define((view) => {
    const strip = () => { for (const a of COMBOBOX) if (view.contentDOM.hasAttribute(a)) view.contentDOM.removeAttribute(a); };
    const mo = new MutationObserver(strip);
    mo.observe(view.contentDOM, { attributes: true, attributeFilter: COMBOBOX });
    strip();
    return { destroy: () => mo.disconnect() };
  });
  const speakSelection = EditorView.updateListener.of((u) => {
    const now = selectedCompletionIndex(u.state);
    if (now == null || completionStatus(u.state) !== "active") return;
    const list = currentCompletions(u.state);
    if (list !== currentCompletions(u.startState) || now === selectedCompletionIndex(u.startState)) return;   // narrowed by typing, or not moved
    announce(rowAnnouncement(list[now], now, list.length), true, Announcement.Navigation);
  });
  const hideList = EditorView.updateListener.of((u) => {
    if (!completionStatus(u.state)) return;
    u.view.requestMeasure({
      read: () => u.view.dom.querySelector(".cm-tooltip-autocomplete"),
      write: (t) => { if (t && t.getAttribute("aria-hidden") !== "true") t.setAttribute("aria-hidden", "true"); },
    });
  });

  /** Slider mode's value, a step at a time from the keyboard (Up/Right and Down/Left, Page Up and Page Down ten),
   * spoken as it moves as native's is — once Tab has taken hold of the slider; false otherwise, so the key does what
   * it would (the arrows move through the code while the slider only rests there, faded). */
  function nudge(view, steps) {
    const { slot, held } = view.state.field(sliderField, false) ?? {};
    if (!slot || !held) return false;
    const { lo, hi } = slot.range;
    const step = gridFor(lo, hi);
    const v = Math.min(hi, Math.max(lo, Math.round((slot.value + steps * step) / step) * step));
    const txt = formatValue(v, lo, hi);
    if (txt === formatValue(slot.value, lo, hi)) return true;   // at an end: taken, and nothing said
    view.dispatch({ changes: { from: slot.from, to: slot.to, insert: txt }, selection: { anchor: slot.from + txt.length }, userEvent: "input.slide" });
    announce(txt, true, Announcement.Navigation);
    return true;
  }
  hooks.registerNudge?.(nudge);   // Ctrl-N and Ctrl-P, the editor's own Down and Up (editor.js)
  // Tab on a slider's value takes hold of it, and Tab again lets go (a completion list up has Tab first: it accepts)
  function holdToggle(view) {
    const { slot, held } = view.state.field(sliderField, false) ?? {};
    if (!slot || completionStatus(view.state) === "active") return false;
    view.dispatch({ effects: holdSlider.of(!held) });
    const { lo, hi } = slot.range;
    announce(held ? `${slot.opt.replace(/:$/, "")} slider released` : `${slot.opt.replace(/:$/, "")} slider, ${formatValue(slot.value, lo, hi)}. Up and Down change it, Tab lets go`, true, Announcement.Navigation);
    return true;
  }
  const sliderKeys = Prec.highest(keymap.of([
    { key: "Tab", run: holdToggle },
    { key: "ArrowUp", run: (v) => nudge(v, 1) || moveCompletion(v, -1) }, { key: "ArrowDown", run: (v) => nudge(v, -1) || moveCompletion(v, 1) },
    { key: "ArrowRight", run: (v) => nudge(v, 1) }, { key: "ArrowLeft", run: (v) => nudge(v, -1) },
    { key: "PageUp", run: (v) => nudge(v, 10) || moveCompletion(v, -10) }, { key: "PageDown", run: (v) => nudge(v, -10) || moveCompletion(v, 10) },
  ]));

  // ── Slider mode: a ranged opt's number under the caret gets a slider ──

  function sliderSlot(state) {
    const sel = state.selection.main;
    if (!sel.empty) return null;
    const line = state.doc.lineAt(sel.head), col = sel.head - line.from;
    const scan = scanLineToCaret(line.text, col);
    if (scan.inString || scan.inComment) return null;
    const context = lineToContext(line.text, col);
    if (context.length < 3) return null;
    const partial = context[context.length - 1];
    // a value typed, or none yet: the caret just after the opt and a space ("release: "), nothing after it on the line
    // but a comma, a bracket or the end — the slider opens at the opt's default, and a slide types the value in
    const empty = partial === "" && /\s/.test(line.text[col - 1] ?? "") && /^\s*($|[,)\]}])/.test(line.text.slice(col));
    if (!empty && !/^-?(\d+\.?\d*|\.\d+)$/.test(partial)) return null;
    let opt = "";
    for (let i = context.length - 2; i >= 0; --i) if (context[i]) { opt = context[i]; break; }
    if (!opt.endsWith(":")) return null;
    resolveSynthAt(state, sel.head);
    const owner = api.ownerForContext(context);
    const range = api.ownerRange(owner, opt) || api.optRanges[opt];
    if (!range) return null;
    const end = empty ? col : tokenEndAtCaret(line.text, col), start = end - partial.length;
    return { key: `${line.number}:${start}:${opt}:${owner}`, from: line.from + start, to: line.from + end, value: empty ? range.def : Number(partial), opt, owner, range };
  }

  // put away, it stays away while the caret stays on the value it was over (its key: the line, where the number
  // starts, the opt), typed into or not
  const sliderField = StateField.define({
    create: (state) => withTooltip(sliderSlot(state)),
    update: (prev, tr) => {
      if (tr.effects.some((e) => e.is(dismissSlider))) return withTooltip(prev.slot, prev.slot?.key ?? null);
      const hold = tr.effects.find((e) => e.is(holdSlider));
      if (hold) return { ...prev, held: !!prev.slot && hold.value };
      if (!(tr.docChanged || tr.selection)) return prev;
      const next = withTooltip(sliderSlot(tr.state), prev.dismissed);
      // held while it is the same value being stepped (its own slides, the caret on it); anything else typed lets go
      next.held = prev.held && !!next.slot && next.slot.key === prev.slot?.key && (!tr.docChanged || tr.isUserEvent("input.slide"));
      return next;
    },
    provide: (f) => showTooltip.from(f, (v) => v.tooltip),
  });

  function withTooltip(slot, dismissed = null) {
    const away = !!slot && slot.key === dismissed;
    return { slot: away ? null : slot, dismissed: away ? dismissed : null, held: false, tooltip: slot && !away ? { pos: slot.from, above: true, create: sliderView } : null };   // the caret gone from the value: it has its slider again next time
  }

  function sliderView(view) {
    const dom = el("div", "sp-slider");
    const label = el("span", "sp-slider-opt");
    const input = el("input");
    input.type = "range";
    const out = el("span", "sp-slider-val");
    dom.append(label, input, out);
    let key = null, dragging = false;
    const sync = (state) => {
      const { slot, held } = state.field(sliderField);
      if (!slot) return;
      dom.classList.toggle("held", !!held);
      const { lo, hi } = slot.range;
      if (slot.key !== key) {
        key = slot.key;
        input.min = lo;
        input.max = hi;
        input.step = gridFor(lo, hi);
        label.textContent = slot.opt;
        input.setAttribute("aria-label", `${slot.opt} ${slot.owner ? `of ${slot.owner}` : ""}`.trim());
      }
      if (!dragging) input.value = slot.value;
      out.textContent = formatValue(slot.value, lo, hi);
    };
    input.addEventListener("pointerdown", () => { dragging = true; });
    input.addEventListener("pointerup", () => { dragging = false; view.focus(); });
    input.addEventListener("input", () => {
      const { slot } = view.state.field(sliderField);
      if (!slot) return;
      const txt = formatValue(Number(input.value), slot.range.lo, slot.range.hi);
      // an edit and nothing more: Run applies it, as with a value typed in
      view.dispatch({ changes: { from: slot.from, to: slot.to, insert: txt }, selection: { anchor: slot.from + txt.length }, userEvent: "input.slide" });
    });
    dom.addEventListener("mousedown", (e) => { if (e.target !== input) e.preventDefault(); });
    sync(view.state);
    return { dom, update: (u) => sync(u.state) };
  }

  return [
    sliderKeys,   // ahead of autocompletion's own keys (which wrap at the ends): the same precedence, first wins
    autocompletion({
      override: [source],
      activateOnTyping: true,
      icons: false,
      closeOnBlur: false,
      maxRenderedOptions: 150,
      // after the label in the row, so its text still starts with the name; drawn first
      addToOptions: [
        { render: (c) => (c.type === "note" ? null : Object.assign(el("span", `sp-kind-icon k-${c.type}`, KIND_ICON[c.type] ?? "·"), { title: KIND_LABEL[c.type] ?? "", ariaHidden: "true" })), position: 90 },   // a note's row is its number and its name: the keyboard under the list says "note"
      ],
      optionClass: (c) => `sp-opt-${c.type}${c.type !== "note" ? "" : c.label.startsWith(":") ? " sp-by-name" : /^\d+$/.test(c.label) && c.label % 12 === 0 ? " sp-octave" : ""}`,
      positionInfo,
    }),
    tooltips({ tooltipSpace }),
    trigger,
    sliderField,
    unlist,
    speakSelection,
    hideList,
    notePiano,
  ];
}
