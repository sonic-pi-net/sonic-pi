// SPDX-License-Identifier: AGPL-3.0-or-later
// A showcase: the app in a frame on another page, playing a piece to be looked at (the plugin page's opening,
// site.js mountShowcase). The page that frames it asks by putting a `showcase` on the frame's element before the
// frame loads:
//
//   iframe.showcase = { code, view, audio, storage };   iframe.src = "code.html";
//
//   code     the piece: the editor opens holding it (it is the workspace's starter), and it is run at once
//   line     the code's first line on show (1, its top, unless said): a piece is shown from where there is most to see
//   view     what the panel shows, a name of VIEWS; the framed page offers window.spShowcase.show(name) for a change
//   audio    an AudioContext the framing page made and started in a press: the piece is heard on it. With none (or
//            one that is closed) the piece is silent
//   storage  what the app finds kept, as { key: text } (a preference: how small the log's text is)
//   follows  keys the framing page may keep under later though it has nothing under them now (its colour scheme,
//            not yet chosen): see keep, below
//   plugin   "VST" or "AU": the app is shown as that plugin is, its bar the plugin's (its name, its few tabs). A
//            picture of the plugin on the page that sells it; the app is still the web's
//
// What the framing page keeps afterwards under a key it handed over (its colour scheme, changed while the showcase
// plays) it tells the showcase: the framed page offers window.spShowcase.keep(key, text).
//
// A showcase has no status bar (main.js, style.css): the line of a running app, its state and its version, is not
// part of the picture.
//
// As a showcase the app keeps nothing (its storage is this page's own and goes with it), takes nothing a person does
// to it (it is looked at; the framing page owns every control), and needs no gesture to start. A browser lets no
// audio context start without one, so the silent piece plays on an offline context instead, rendered a block at a
// time at the pace of the clock: the engine, the runtime and every view are the app's own, running as they do with
// sound. Rendering stops while the frame is not on show.
//
// This module is the first thing main.js imports, so the storage and the audio context are in place before any other
// module of the app looks for them.

/** What the panel shows for a view's name: the pane, and for the processes pane which of its views. */
export const VIEWS = { processes: ["insight", "processes"], timeline: ["insight", "timeline"], roll: ["insight", "roll"], metrics: ["debug"], logs: ["logs"] };

/** What the framing page asked for, or null: the app opened as itself, or in a frame that asks for no showcase. */
export function request(win) {
  try {
    const asked = win.frameElement?.showcase;
    return asked && typeof asked.code === "string" ? asked : null;
  } catch { return null; }   // a frame of another origin's page: its element is not ours to read
}

/** A storage like the browser's (text by key) that is kept nowhere. */
export function memoryStorage(seed = {}) {
  const items = new Map(Object.entries(seed).map(([k, v]) => [k, String(v)]));
  return {
    get length() { return items.size; },
    key: (i) => [...items.keys()][i] ?? null,
    getItem: (k) => items.get(String(k)) ?? null,
    setItem: (k, v) => void items.set(String(k), String(v)),
    removeItem: (k) => void items.delete(String(k)),
    clear: () => items.clear(),
  };
}

/**
 * An audio context nothing is heard from, whose time moves as the clock's does. It is an offline context that stops
 * after every block (suspend) and is let on to the next when the clock has reached it. The app is shown a context
 * that is running: its own resume (it resumes a context it takes to be waiting for a gesture) would otherwise let
 * the rendering run on as fast as the machine goes.
 * An offline context holds all it renders, so it has an end (`seconds`): onEnd is told when it gets there. What it
 * holds is taken when it is made, all of it, and WebKit does not give it back when the page goes: at five minutes of
 * stereo (115 MB) a page loaded some forty times, or a showcase's sound switched as often, ran its process out of
 * memory, and neither it nor the page's own engine could start. So it renders to one channel (nobody hears it: the
 * engine's own two channels, which its scopes show, are untouched) and for a shorter while, within RENDER_MOST.
 * @returns { context, hold(on) }  hold: rendering stops while it is on, and carries on from where it was
 */
export const RENDER_MOST = 32 * 1024 * 1024;   // bytes a silent showcase's render may hold
export function pacedContext({ OfflineAudioContext, now = () => performance.now(), later = setTimeout, rate = 48000, block = 2048, seconds = 150, onEnd = null }) {
  const context = new OfflineAudioContext({ numberOfChannels: 1, length: rate * seconds, sampleRate: rate });
  const letOn = context.resume.bind(context);
  const step = block / rate;
  let next = 0, origin = now();   // the context's time the rendering stops at next, and the clock's time at the context's zero
  let held = false, heldAt = 0, waiting = false;
  const onward = () => { arm(); letOn(); };
  // the block rendered: on to the next when the clock is due, or when let go if held meanwhile
  const due = () => { if (held) waiting = true; else onward(); };
  function arm() {
    next += step;
    context.suspend(next).then(() => later(due, Math.max(0, origin + next * 1000 - now())));
  }
  context.resume = () => Promise.resolve();
  Object.defineProperty(context, "state", { get: () => "running" });
  // what a real context has and an offline one may not: the app reads them for its clock and its latency
  if (!("getOutputTimestamp" in context)) context.getOutputTimestamp = () => ({ contextTime: context.currentTime, performanceTime: now() });
  for (const k of ["baseLatency", "outputLatency"]) if (!(k in context)) Object.defineProperty(context, k, { value: 0 });
  arm();
  context.startRendering().then(() => onEnd?.(), () => {});
  return {
    context,
    hold(on) {
      if (on === held) return;
      held = on;
      if (on) { heldAt = now(); return; }
      origin += now() - heldAt;   // the time held never happened: the next block is due as far off as it was
      if (waiting) { waiting = false; onward(); }
    },
  };
}

const HANDS = ["pointerdown", "pointerup", "pointermove", "pointerover", "mousedown", "mouseup", "mousemove", "mouseover", "click", "dblclick", "contextmenu", "wheel", "keydown", "keyup", "keypress", "beforeinput", "paste", "cut", "drop", "dragstart"];
/** Whatever a person does to `target` is stopped before the app hears it. What a script does (the app pressing its own Run) is not. */
export function quietInput(target) {
  for (const type of HANDS) target.addEventListener(type, (e) => { if (!e.isTrusted) return; e.stopImmediatePropagation(); if (e.cancelable) e.preventDefault(); }, { capture: true, passive: false });
}

/** The page made a showcase, if it was asked for one: null otherwise, and nothing touched. */
export function begin(win) {
  const asked = request(win);
  if (!asked) return null;
  Object.defineProperty(win, "localStorage", { configurable: true, value: memoryStorage(asked.storage) });
  const loud = asked.audio && asked.audio.state !== "closed" ? asked.audio : null;   // the framing page's, started in its press (the app resumes it again as it boots)
  if (loud) win.spAudioContext = loud;
  else {
    // when all it can hold is rendered the piece starts again: the frame is loaded anew, as the framing page left it
    const paced = pacedContext({ OfflineAudioContext: win.OfflineAudioContext, onEnd: () => win.location.reload() });
    win.spAudioContext = paced.context;
    // not on show (scrolled away, another tab, its page put aside): nothing is rendered, and nothing is used
    let onShow = true;
    const seen = () => paced.hold(!onShow || win.document.visibilityState === "hidden");
    win.document.addEventListener("visibilitychange", seen);
    if (win.IntersectionObserver) new win.IntersectionObserver((entries) => { onShow = entries[entries.length - 1].isIntersecting; seen(); }).observe(win.document.documentElement);
  }
  quietInput(win);
  // what the framing page keeps afterwards under a key it handed over: it says so (keep), and whoever follows is told
  const followers = new Set();
  const handed = new Set([...Object.keys(asked.storage ?? {}), ...(Array.isArray(asked.follows) ? asked.follows : [])]);
  const keep = (key, text) => {
    if (!handed.has(key) || text == null) return;
    win.localStorage.setItem(key, text);
    for (const fn of followers) fn(key, text);
  };
  return { code: asked.code, view: asked.view in VIEWS ? asked.view : "processes", loud: !!loud, line: Number.isInteger(asked.line) && asked.line > 1 ? asked.line : 1, plugin: FORMATS.includes(asked.plugin) ? asked.plugin : "",
    keep, kept: (fn) => void followers.add(fn) };
}

const FORMATS = ["VST", "AU"];
// The plugin's bar: the tabs its build has (scripts/build-site.mjs: the pages marked plugin, the tutorial, the editor).
const PLUGIN_TABS = ["examples", "tutorial", "code"];
/** The page's bar as the plugin has it: its name and its tabs. */
export function dressAsPlugin(document) {
  for (const tab of document.querySelectorAll(".ic-tabs .ic-tab")) if (!PLUGIN_TABS.includes(tab.dataset.tab)) tab.remove();
  const brand = document.querySelector("#site-nav .sn-brand");
  if (brand) brand.textContent = "Sonic Pi";
  document.querySelector("#site-nav")?.setAttribute("aria-label", "Sonic Pi");
}

/** This page's showcase, or null. */
export const showcase = typeof window === "undefined" ? null : begin(window);
