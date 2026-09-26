// SPDX-License-Identifier: AGPL-3.0-or-later
// The code card: one component for every short program the GUI shows — the quickstart pane's cards, the docs'
// and the app tutorial's snippets, the site's examples and hero, the web tutorial's blocks. Native's quickstart
// card (widgets/quickstart) and sonic-pi.net's: a title bar with its actions, the code, then the blurb beside
// the scope that is the play control (the transport disc with the stereo rings drawn round it).
//
//   const card = createCard({ title, code, blurb, key, actions: ["copy", "insert", "drag"], hooks, root });
//   deck.add(card);            // a deck (./deck.js) plays it: one card at a time, on a scope slot of its own
//   parent.append(card.el);
//
// The code shows as a still block until Edit is pressed, when an editor takes its place (a page of hundreds of
// cards stays quick): edit a number, play it again. The block is the editor's own tree of elements and token
// classes (highlight.js renderLines), so the two lay out alike and nothing moves on the toggle. With `remember`,
// an edit outlives a reload until Reset. Play runs the code as a job of the card's; pressed again it runs it
// again, so a live loop takes the new code; Stop ends every job the card started and nothing else (./deck.js).
// The line a sound came from flashes, as the editor's do. An error in the program shows in the footer, as the
// app's error strip. Every colour is the theme's; the sizes take the --qs-code-size and --qs-title-size tokens,
// so a pane sets its own (style.css, site.css).
import { EditorView, keymap } from "@codemirror/view";
import { EditorState } from "@codemirror/state";
import { defaultKeymap, history, historyKeymap, indentWithTab } from "@codemirror/commands";
import { indentUnit } from "@codemirror/language";
import { highlighting } from "../highlight.js";
import { renderLines } from "../highlight.js";
import { loopScopeField, setLoopScopes, loopScopeCanvas } from "../editor.js";
import { LoopScopeState, drawLoopScope, SCROLL_WINDOW, SWEEP_WINDOW, SWEEP_SEARCH } from "../loopscope.js";
import { css, colour, blend, cssColour } from "../theme.js";
import { createEventStrip } from "./strip.js";
import { TRANSPORT, TRANSPORT_STOP } from "./card-html.js";

// The classes CodeMirror puts on an editor in a root (its base theme's, the highlighter's): a still block wears
// them too, so CodeMirror's own rules lay both out alike. Asked once per root, of a throwaway editor, which
// also mounts CodeMirror's styles there before any block shows.
const cmClasses = new WeakMap();
function editorClasses(root) {
  let classes = cmClasses.get(root);
  if (!classes) {
    const probe = new EditorView({ state: EditorState.create({ doc: "", extensions: [highlighting] }), root });
    classes = [...probe.dom.classList].filter((c) => c !== "cm-editor");
    probe.destroy();
    cmClasses.set(root, classes);
  }
  return classes;
}

const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };
const store = {
  get: (k) => { try { return localStorage.getItem(k); } catch { return null; } },
  set: (k, v) => { try { v == null ? localStorage.removeItem(k) : localStorage.setItem(k, v); } catch {} },
};

import { icon, paths } from "../icons.js";   // the cards' action glyphs, from the one registry

// native's transportRing: the disc, and the glyph cut out of it
export { TRANSPORT, TRANSPORT_STOP } from "./card-html.js";   // the one Play and Stop, the build's cards' too

// native's CardScope (widgets/cardscope.h), to the same proportions, on a scope slot of the card's own as
// native's is; gain is its setDrive's default
const RING = { radius: 0.42, step: 0.12, swing: 0.04, stroke: 0.028, idle: 0.22, gain: 1, points: 256 };   // step: a second ring's way in (native: 0.42 and 0.3)

/**
 * The card's rings: the waveform wrapped around a circle on each transport control — the left channel round
 * Play, the right round Stop — each radius swinging with the signal. Idle rings settle back to faint circles,
 * and hover lights the one under the pointer.
 * @param scopes  [{ box, canvas, channels }] in channel order: the left channel's, the right's. A scope may draw
 *                more than one channel (channels: [0, 1]), each ring a step further in — the bar's stop has both.
 * @param o.colours  { accent, accentInner, idle } in place of the theme's
 * @param o.gain     the signal's multiplier before it bends the ring (a small ring wants more)
 * @param o.swing    how far a full-scale signal bends the ring, as a share of the side (native: 0.04)
 */
export function cardRings(scopes, { colours = null, gain = RING.gain, swing = RING.swing } = {}) {
  const reduceMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
  let live = false, raf = 0, bufs = null, read = null;
  const lit = scopes.map(() => false);

  const ring = (canvas, samples, stroke, alpha, baseR = RING.radius) => {
    const ctx = canvas.getContext("2d");
    const side = canvas.width, c = side / 2, radius = baseR * side, amp = swing * side;
    ctx.strokeStyle = stroke;
    ctx.globalAlpha = alpha;
    ctx.lineWidth = RING.stroke * side;
    ctx.lineCap = ctx.lineJoin = "round";
    ctx.beginPath();
    if (!samples) ctx.arc(c, c, radius, 0, 2 * Math.PI);
    else {
      const n = samples.length, step = Math.max(1, Math.floor(n / RING.points));   // a ring this size needs no more points than this
      for (let i = 0; i <= n; i += step) {
        const theta = ((i % n) / n) * 2 * Math.PI;
        const r = radius + Math.max(-1, Math.min(1, samples[i % n] * gain)) * amp;
        const x = c + r * Math.cos(theta), y = c + r * Math.sin(theta);
        if (i === 0) ctx.moveTo(x, y); else ctx.lineTo(x, y);
      }
    }
    ctx.stroke();
    ctx.globalAlpha = 1;
  };

  function paint(samples) {
    const dpr = window.devicePixelRatio || 1;
    scopes.forEach(({ box, canvas, channels }, k) => {
      channels ??= [k];   // one ring, its own channel, unless the scope names its channels
      const side = Math.round((box.clientWidth || 92) * dpr);
      if (side && canvas.width !== side) canvas.width = canvas.height = side;
      if (!canvas.width) return;
      // native: the rings rest in the foreground colour and take the accent once they are live; a second ring, a step in, in the accent's softer half
      const accent = live || lit[k];
      canvas.getContext("2d").clearRect(0, 0, side, side);
      channels.forEach((ch, j) => {
        const stroke = !accent ? (colours?.idle ?? css("Foreground")) : j === 0 ? (colours?.accent ?? css("HighlightedBackground")) : (colours?.accentInner ?? cssColour(blend(colour("Foreground"), colour("HighlightedBackground"), 0.5)));
        ring(canvas, samples?.[ch], stroke, accent ? 1 : RING.idle, RING.radius - RING.step * j);
      });
    });
  }

  let drawn = null;   // the last frame's write position: the same frame again is not drawn again
  function frame() {
    if (!live) return;
    const f = read?.();
    if (f && f.writePosition != null && f.writePosition === drawn) { raf = requestAnimationFrame(frame); return; }
    drawn = f?.writePosition ?? null;
    if (f) {
      const { interleaved, channels, frames } = f;
      const want = Math.max(2, ...scopes.map((s) => (s.channels ?? []).length ? Math.max(...s.channels) + 1 : scopes.length));
      if (bufs?.[0].length !== frames || bufs.length !== want) bufs = Array.from({ length: want }, () => new Float32Array(frames));
      for (let i = 0; i < frames; i++) bufs.forEach((b, k) => { b[i] = interleaved[i * channels + Math.min(k, channels - 1)]; });
      paint(bufs);
    } else paint(null);
    raf = requestAnimationFrame(frame);
  }

  return {
    /** Play: the rings follow the card's own scope stream, readFrame() giving its newest frames. */
    play(readFrame) {
      read = readFrame;
      live = true;
      cancelAnimationFrame(raf);
      if (read && !reduceMotion.matches) raf = requestAnimationFrame(frame);   // under reduced motion the rings light up but stay still
      else paint(null);
    },
    stop() {
      live = false;
      cancelAnimationFrame(raf);
      paint(null);
    },
    /** The ring round scope k lit (hover, focus) or not. */
    hover(k, on) {
      lit[k] = on;
      if (!live) paint(null);
    },
    repaint: () => paint(null),
  };
}

/**
 * The transport: Play and Stop, each a disc in a scope box with the card's rings drawn round it once it plays (the
 * left channel round Play, the right round Stop) — the quickstart card's, and the live synth's on a docs page.
 * At rest a box's ring is CSS (card.css .qs-scope::before): a page of hundreds makes no canvas until one plays.
 *   const t = createTransport({ onRun, onStop }); host.append(t.el); t.setPlaying(true, readFrame)
 */
export function createTransport({ onRun = null, onStop = null, onPress = false } = {}) {
  const transport = el("div", "qs-transport");
  const scope = (cls) => ({ box: el("div", `qs-scope ${cls}`), canvas: null });
  const left = scope("qs-scope-play"), right = scope("qs-scope-stop");
  const run = el("button", "qs-run");
  run.type = "button";
  run.title = "Play";
  run.innerHTML = `${TRANSPORT}<span class="sr-only">Play</span>`;
  // onPress (an instrument's, beside its keys): Play and Stop act as the pointer goes down, as a key does, not when it
  // comes up; a click from the keyboard (Enter, Space) still acts, and the press's own click is not taken twice
  const act = (b, fn) => {
    let pressed = false;
    if (onPress) b.addEventListener("pointerdown", (e) => { if (e.button > 0 || b.disabled) return; pressed = true; fn?.(); });
    b.addEventListener("click", (e) => { if (pressed && e.detail > 0) { pressed = false; return; } pressed = false; fn?.(); });
  };
  act(run, () => onRun?.());
  left.box.appendChild(run);
  const halt = el("button", "qs-run qs-stop");
  halt.type = "button";
  halt.title = "Stop";
  halt.innerHTML = `${TRANSPORT_STOP}<span class="sr-only">Stop</span>`;
  halt.disabled = true;
  act(halt, () => onStop?.());
  right.box.appendChild(halt);
  transport.append(left.box, right.box);
  let rings = null;
  return {
    el: transport, run, halt,
    get rings() { return rings; },
    /** Waiting on the engine (it is starting, or a synthdef or a sample the program names is loading): an arc goes
     *  round the play button until the run has started, and a screen reader is told it is busy. */
    setBusy(on) { transport.classList.toggle("busy", on); if (on) run.setAttribute("aria-busy", "true"); else run.removeAttribute("aria-busy"); },
    /** Playing or not: Stop live, the rings following readFrame() (the run's scope stream) while it plays. */
    setPlaying(on, readFrame = null) {
      transport.classList.toggle("playing", on);
      halt.disabled = !on;
      if (on && !rings) {
        for (const s of [left, right]) { s.canvas = el("canvas", "qs-rings"); s.canvas.setAttribute("aria-hidden", "true"); s.box.prepend(s.canvas); }
        rings = cardRings([left, right]);
      }
      if (on) rings.play(readFrame); else rings?.stop();
    },
    /** Off the page: the rings stop drawing. */
    detach() { rings?.stop(); },
  };
}

// an icon button that still says what it does, to a screen reader and to text lookups
function action(name, label, onClick) {
  const b = el("button", `qs-card-action qs-card-${name}`);
  b.type = "button";
  b.title = label;
  b.setAttribute("aria-label", label);
  b.innerHTML = icon(name, "");
  if (onClick) b.addEventListener("click", onClick);
  return b;
}

/**
 * @param o.title     the card's name, in its bar
 * @param o.code      the program
 * @param o.blurb     a line under it, beside the scope ("" for none)
 * @param o.key       what the card is known by (a deck adopts a playing card by it across re-renders; storage)
 * @param o.actions   of "edit", "reset", "copy", "insert", "drag": the bar's buttons — edit toggles the editor (off to
 *                    begin with: the code is a block to read), reset shows once the code is changed
 * @param o.open      (code, job) => void: the bar's "Open in Sonic Pi" action, the code as it is now
 * @param o.hooks     { copy(code), insert(code) } as the actions need
 * @param o.remember  a storage prefix: an edit is kept under it until Reset (null: not kept)
 * @param o.root      the document or shadow root the card lives in (CodeMirror's styles go there)
 * @param o.wide      spans the grid
 * @param o.playable  false for a fragment that cannot run on its own: no play control
 */
let titles = 0;   // each card's title an id of its own, for the card's name

// A card's live loops' scopes, as the editor has them (loopscope.js): each loop's own sound, read from the scope slot
// its scope_out FX writes to, drawn on its live_loop line in the card's code — the still block's, or the editor's
// while it is edited. The card tells it three things: a record of its run came (the slots are in them: a loop's fx
// starting, and freed), its code was drawn again (paint), and it stopped (clear). The preferences' Show and Scroll
// Loop Scopes rule (hooks.loopScopes).
function cardLoopScopes(body, hooks) {
  const loops = new Map();         // the loop's fx node → { name, slot }
  const states = new Map();        // slot → LoopScopeState
  let raf = 0, code = null;        // code: what shows, { block } or { view }
  const prefs = () => hooks.loopScopes?.() ?? { show: true, scroll: false };
  function paint(now = code) {
    code = now;
    const list = prefs().show ? [...loops.values()] : [];
    if (code?.view) code.view.dispatch({ effects: setLoopScopes.of(list) });
    if (code?.block?.isConnected) {
      for (const c of code.block.querySelectorAll(".sp-loop-scope")) c.remove();
      const rows = [...code.block.querySelectorAll(".cm-line")];
      for (const { name, slot } of list) {
        const re = new RegExp("^\\s*live_loop\\s+:" + name.replace(/[^\w]/g, "\\$&") + "\\b");
        rows.find((r) => re.test(r.textContent))?.appendChild(loopScopeCanvas(name, slot));
      }
    }
    if (list.length && !raf) raf = requestAnimationFrame(draw);
  }
  function draw() {
    const canvases = body.querySelectorAll(".sp-loop-scope");
    if (!canvases.length) { raf = 0; states.clear(); return; }
    raf = requestAnimationFrame(draw);
    const scroll = !!prefs().scroll;
    for (const c of canvases) {
      const slot = Number(c.dataset.slot);
      let st = states.get(slot);
      if (!st) states.set(slot, (st = new LoopScopeState()));
      const frame = hooks.scopeFrame?.(slot, scroll ? SCROLL_WINDOW : SWEEP_WINDOW + SWEEP_SEARCH) ?? null;
      if (st.feed(frame, scroll) || !c.dataset.painted) { drawLoopScope(c, st, { scroll }); c.dataset.painted ||= "1"; }   // said once: a write is a change to the page, even of the same value
    }
  }
  return {
    paint,
    record(r) {
      if (r.kind === "synth" && r.synth === "sonic-pi-fx_scope_out" && r.node != null && r.args?.scope_num != null && /^live_loop_/.test(r.name ?? "")) {
        loops.set(r.node, { name: r.name.slice(10), slot: r.args.scope_num });
        paint();
      } else if (r.kind === "fx_free" && loops.delete(r.node)) paint();
    },
    clear() { if (!loops.size) return; loops.clear(); paint(); },
    detach() { loops.clear(); cancelAnimationFrame(raf); raf = 0; },
  };
}

export function createCard(o) {
  const { title, blurb = "", key = title, actions = [], open = null, hooks = {}, remember = null, root = document, wide = false, playable = true } = o;
  // a card with more to it (the synth, docs.js createInstrument): `panel` between the bar and the code, `extra` in the
  // bar after the title, the code `still` (made, not typed: no editor), `level` its heading's, Play `onPress`
  const { panel = null, extra = null, still = false, level = 2, onPress = false } = o;
  const original = o.code.replace(/\s+$/, "");   // a trailing newline: the block ignores it, the editor would draw it as an empty line
  const storeKey = remember ? remember + key : null;
  const saved = storeKey ? store.get(storeKey) : null;
  let text = saved ?? original;

  const art = el("section", `qs-card${wide || original.split("\n").length > 40 ? " qs-wide" : ""}`);
  art.dataset.key = key;
  art.dataset.title = title;
  // a group named by its title: a screen reader entering it says which card its Play, Edit, … are for (a group, not
  // a region: a docs page has hundreds, and landmarks are for finding your way about the page)
  const titleId = `qs-title-${++titles}`;
  art.setAttribute("role", "group");
  art.setAttribute("aria-labelledby", titleId);

  // ── the bar: the title, then the actions in native's order — reset (when edited), copy, insert, the drag handle ──
  const head = el("header", "qs-card-head");
  const h = el(`h${level}`, "qs-card-title", title);   // under its section's h1 (a card that is its page's own subject: the h1)
  h.id = titleId;
  head.appendChild(h);
  const buttons = {};
  if (actions.includes("edit")) {
    buttons.edit = action("edit", "Edit", () => setEditing(!editing));
    buttons.edit.setAttribute("aria-pressed", "false");
  }
  if (actions.includes("reset")) buttons.reset = action("reset", "Reset", () => { setCode(original); store.set(storeKey, null); api.onReset?.(); });
  if (actions.includes("copy")) {
    buttons.copy = action("copy", "Copy", async () => {
      try { await (hooks.copy ? hooks.copy(code()) : navigator.clipboard.writeText(code())); } catch {}
      buttons.copy.classList.add("done");
      buttons.copy.querySelector("svg").innerHTML = paths("check");
      setTimeout(() => { buttons.copy.classList.remove("done"); buttons.copy.querySelector("svg").innerHTML = paths("copy"); }, 1200);
    });
  }
  if (actions.includes("insert")) buttons.insert = action("insert", "Insert into the editor", () => hooks.insert?.(code()));
  if (actions.includes("drag")) {
    buttons.drag = action("drag", "Drag into the editor");
    buttons.drag.draggable = true;
    buttons.drag.addEventListener("dragstart", (e) => { e.dataTransfer.setData("text/plain", code().replace(/\n?$/, "\n")); e.dataTransfer.effectAllowed = "copy"; });
  }
  if (open) buttons.open = action("open", "Open in Sonic Pi", (ev) => { ev.stopPropagation(); open(code(), api.job); });
  if (extra) head.append(...[extra].flat());
  head.append(...["edit", "reset", "copy", "insert", "drag", "open"].map((k) => buttons[k]).filter(Boolean));

  // ── the body: a highlighted block, until it is clicked into; then the editor, with the caret there ──
  const body = el("div", "qs-card-body");
  // the code, reachable from the keyboard (a long line scrolls: arrows scroll it) and named for a screen reader
  body.tabIndex = 0;
  body.setAttribute("role", "group");
  body.setAttribute("aria-label", `${title}, code`);
  const scopes = cardLoopScopes(body, hooks);
  let view = null;
  const editor = () => {
    if (view) return view;
    view = new EditorView({
      state: EditorState.create({
        doc: text,
        extensions: [
          history(), indentUnit.of("  "), highlighting, loopScopeField,   // the platform's own caret: it is right under a pane's zoom, where a drawn one can go astray
          keymap.of([{ key: "Mod-Enter", run: () => (api.onRun?.(), true) }, { key: "Mod-.", run: () => (api.onStop?.(), true) }, indentWithTab, ...defaultKeymap, ...historyKeymap]),
          EditorView.updateListener.of((u) => { if (u.docChanged) changed(); }),
        ],
      }),
      parent: body,
      root,   // the shadow root the card lives in (the page may not be attached yet): CodeMirror's styles must land there, not in the document
    });
    return view;
  };
  // the block: the code highlighted in the editor's own shape (highlight.js renderLines); shown while not editing
  let block = null, editing = false;
  const showBlock = () => {
    const now = view ? view.state.doc.toString() : text;   // as edited, if it has been
    block = renderLines(now);
    block.classList.add("qs-static", ...editorClasses(root));
    if (view) view.dom.remove();   // off the page while the block shows (CodeMirror's own display: flex !important beats hidden); the view lives on
    body.appendChild(block);
    scopes.paint({ block });
  };
  const setEditing = (on) => {
    if (on === editing) return;
    editing = on;
    art.classList.toggle("editing", on);
    buttons.edit?.classList.toggle("on", on);
    buttons.edit?.setAttribute("aria-pressed", String(on));
    if (on) { block?.remove(); block = null; const v = editor(); if (!v.dom.isConnected) body.appendChild(v.dom); scopes.paint({ view: v }); v.focus(); }
    else showBlock();
  };
  if (buttons.edit || still) showBlock(); else scopes.paint({ view: editor() });   // no edit toggle: the editor from the start (a still card: the block only)
  const code = () => (view ? view.state.doc.toString() : text);
  const setCode = (t) => { text = t; if (view) view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: t } }); if (!editing && (buttons.edit || still)) { block?.remove(); showBlock(); } else if (!view) editor(); };
  let saveTimer = 0;
  function changed() {
    const edited = code() !== original;
    art.classList.toggle("edited", edited);
    if (!storeKey) return;
    clearTimeout(saveTimer);
    saveTimer = setTimeout(() => store.set(storeKey, edited ? code() : null), 300);
  }
  if (saved != null && saved !== original) art.classList.add("edited");

  // the line a sound came from flashes: a wash laid on that line, over the block or the editor alike. Animated (Web
  // Animations), never added, taken out or restyled: a playing card changes nothing in the page, where a page-wide
  // watcher (an ad blocker's) would hear every note (../shadow.js). A few, laid ready, for lines that flash together.
  const washes = Array.from({ length: 4 }, () => { const w = el("div", "qs-line-flash"); w.setAttribute("aria-hidden", "true"); body.appendChild(w); return w; });
  let nextWash = 0;
  function flash(line) {
    let row = null;
    if (view && editing) {
      if (line < 1 || line > view.state.doc.lines) return;
      const at = view.domAtPos(view.state.doc.line(line).from).node;
      row = (at.nodeType === 1 ? at : at.parentElement)?.closest(".cm-line");
    } else if (block?.isConnected) row = block.querySelectorAll(".cm-line")[line - 1];
    if (!row) return;
    // the line's place in the body, in the body's own pixels: where it is on screen, over the body's scale there (a
    // pane's or a page's zoom). Summed offsetTops would do the same in one engine and not in another: how offsetTop
    // meets CSS zoom differs between Safari's versions, and in one of them the wash would land half a line off.
    const b = body.getBoundingClientRect(), r = row.getBoundingClientRect();
    const scale = body.offsetHeight ? b.height / body.offsetHeight : 1;
    const top = `${(r.top - b.top) / scale - body.clientTop}px`, height = `${r.height / scale}px`;
    const wash = washes[nextWash++ % washes.length], lit = getComputedStyle(body).getPropertyValue("--flashWash");
    for (const a of wash.getAnimations()) a.cancel();
    wash.animate([{ top, height, backgroundColor: lit }, { top, height, backgroundColor: "transparent" }], { duration: 450, easing: "ease-out" });
  }

  // ── the footer: the blurb, the way out, the error strip; the transport — the scope round Play, and Stop beside it ──
  const foot = el("footer", "qs-card-foot");
  const p = el("p", "qs-blurb");
  if (blurb) p.append(blurb, " ");
  const state = el("span", "qs-state");
  p.appendChild(state);
  // the blurb, and beneath it the strip of the card's own sounds scrolling by (./strip.js), beside the transport
  const main = el("div", "qs-foot-main");
  const out = el("div", "qs-out");   // what the program puts, the last few lines, as the app's log shows them
  out.setAttribute("role", "log"); out.setAttribute("aria-live", "polite");
  main.append(p, out);
  const OUT_LINES = 3;
  const puts = (text) => {
    for (const line of String(text ?? "").split("\n")) { out.appendChild(el("div", "qs-out-line", line)); }
    while (out.children.length > OUT_LINES) out.firstChild.remove();
  };
  const tp = createTransport({ onRun: () => api.onRun?.(), onStop: () => api.onStop?.(), onPress });
  const transport = tp.el;
  foot.append(main, transport);
  if (!playable) { transport.hidden = true; art.classList.add("qs-still"); }   // a fragment, not a program: no transport
  art.append(head, ...(panel ? [panel] : []), body, foot);

  // the live part, made the first time the card plays: the strip's canvas (the rings' are the transport's)
  let strip = null;
  const live = () => {
    if (strip) return;
    const stripCanvas = el("canvas", "qs-strip");
    stripCanvas.setAttribute("aria-hidden", "true");
    main.appendChild(stripCanvas);
    strip = createEventStrip(stripCanvas, { now: () => hooks.now?.() ?? null, defaults: (synth) => hooks.synthDefaults?.(synth) ?? null });
  };

  const api = {
    el: art, key, title,
    get rings() { return tp.rings; },
    onRun: null, onStop: null, onReset: null,   // the deck's (./deck.js)
    job: null,                                  // the run playing this card, while one is
    code, setCode, flash,
    get edited() { return code() !== original; },
    /** Playing or not: the border, the rings (readFrame gives the card's scope stream), Stop live; Play stays Play — pressed again, the code runs again. */
    setPlaying(on, readFrame = null) {
      art.classList.toggle("playing", on);
      tp.setPlaying(on, readFrame);
      if (on) { live(); strip.start(); } else { strip?.stop(hooks.now?.() ?? null); scopes.clear(); }   // stopped: its loops' scopes go with it
    },
    /** A record of the card's run: its strip draws the sound; what the program puts shows under the blurb. */
    record(r) { live(); strip.record(r); scopes.record(r); if (r.kind === "output") puts(r.text); },
    /** A new run from rest: the output lines go. */
    clearOutput() { out.textContent = ""; },
    setBooting(on) { art.classList.toggle("booting", on); tp.setBusy(on); },
    /** An error in the program, or null for none: the strip in the footer. */
    setError(message) { art.classList.toggle("errored", !!message); state.textContent = message ?? ""; },
    focus() { setEditing(true); },
    get editing() { return editing; },
    setEditing,
    /** Off the page: the rings and the strip stop drawing. */
    detach() { tp.detach(); strip?.clear(); scopes.detach(); },
  };
  return api;
}
