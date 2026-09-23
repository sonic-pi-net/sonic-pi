// SPDX-License-Identifier: AGPL-3.0-or-later
// The docs, as native Sonic Pi v5 has them: Synths, FX, Samples and Lang,
// every snippet runnable. Synths and FX are instrument
// pages: a faceplate of dials for the numeric opts, grouped as native groups
// them, the program those settings make, play and stop, and a QWERTY piano
// (a w s e d f t g y h u j k o l p, z and x for octaves, Space for the demo).
import { renderCode } from "./highlight.js";
import { icon, icon as glyph } from "./icons.js";
import { createSubmenu } from "./ui/submenu.js";
import { packRack } from "./rack.js";
import { createCard, createTransport } from "./ui/card.js";
import { createDeck } from "./ui/deck.js";
import { announce, Announcement } from "./announce.js";
import { gridFor } from "./completion/cm.js";
import { piano } from "./ui/piano.js";
import { CORE } from "./instrument-knobs.js";

// native's, but the examples and the tutorial: they are pages of the site (its Examples tab, its tutorial's book)
const SECTIONS = [
  ["synths", "Synths"],
  ["fx", "FX"], ["samples", "Samples"], ["lang", "Lang"],
];
const PLACE_KEY = "sp-docs-place";
const store = {
  get: (k) => { try { return JSON.parse(localStorage.getItem(k) ?? "null"); } catch { return null; } },
  set: (k, v) => { try { localStorage.setItem(k, JSON.stringify(v)); } catch {} },
};

const el = (tag, cls, text) => {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text != null) e.textContent = text;
  return e;
};
const button = (cls, text, onClick) => {
  const b = el("button", cls, text);
  b.type = "button";
  if (onClick) b.addEventListener("click", onClick);
  return b;
};
const escapeHTML = (s) => String(s).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
const optDocHTML = (s) => escapeHTML(s ?? "").replace(/`([^`]+)`/g, "<code>$1</code>");
/** Native lists synths and FX by name: :dark_ambience is Dark Ambience. */
export const titleCase = (key) => key.split("_").map((w) => w.charAt(0).toUpperCase() + w.slice(1)).join(" ");

const svgIcon = (name) => icon({ play: "player-play", stop: "player-stop" }[name] ?? name, "");   // the one registry (icons.js)

// ── Dials ─────────────────────────────────────────────────────────────────

const GROUPS = ["PITCH", "ENVELOPE", "FILTER", "MOD", "CHARACTER", "OUT"];
// an FX's Out module (createInstrument UNIVERSAL): the opts taken out of their group and shown beside Play and Stop
const UNIVERSAL_FX = ["mix", "note", "amp"];
// a synth's metadata may say which module an opt sits in (its opt's "group"); else its name does
const groupFor = (o) => (o.group && GROUPS.includes(o.group) ? o.group : groupOf(o.name));
function groupOf(name) {
  if (/^(amp|pan|mix|pre_amp|pre_mix)$/.test(name)) return "OUT";
  if (/^(attack|decay|sustain|release)(_level)?$|^env_/.test(name)) return "ENVELOPE";
  if (/^(note|pitch|detune|freq)$|detune/.test(name)) return "PITCH";
  if (/cutoff|^res$|_res$|^lpf|^hpf|^filter/.test(name)) return "FILTER";
  if (/^mod_|phase|^depth$|^divisor$|^rate$|^pulse_width$|wave/.test(name)) return "MOD";
  return "CHARACTER";
}

// The reference gives no range for a few opts that are still sounds to dial: one whose default is another opt
// (decay_level follows sustain_level, tb303's cutoff_attack follows attack) takes that opt's range and default; the
// rest have theirs here. Setup values with none (seed, buffer, output, max_frames) stay off the faceplate.
const RANGES = {
  divisor: [0, 10], depth: [0, 20], krunch: [0, 20], delay: [0, 20], decay_curve: [-300, 0], detune: [-24, 24],
  amp_min: [0, 1], amp_max: [0, 1],
};
function ranged(o, opts) {
  if (typeof o.min === "number" && typeof o.max === "number") return o;
  const like = typeof o.default === "string" && o.default.startsWith(":") ? opts.find((x) => x.name === o.default.slice(1)) : null;
  if (like && typeof like.min === "number" && typeof like.max === "number") return { ...o, min: like.min, max: like.max, default: like.default, tracks: like.name };   // its range and default its partner's, and tracking it
  const r = typeof o.default === "number" ? RANGES[o.name] : null;
  return r ? { ...o, min: r[0], max: r[1] } : o;
}

// An FX is a pedal: something goes into it. Its page has the sounds to put through — drums, a pluck, a pad, a voice —
// and starts with the drums, whatever the FX: the break is rich from top to bottom, so every FX is heard at work on it
// at once, and the others are a tap away (a pluck's tail for the FX that give space, a pad for those that move a
// sound, the sour tone for those that work on pitch). The sound loops while it
// plays, so a knob turned is heard, and the FX switch takes the FX out of the way to hear the sound as it was.
// `play` is the same sound at a note of the player's own: a key on the FX's piano plays what the card is already
// putting through it, rather than some other synth. A sample has no note to play, so those fall back to a synth.
const FX_INPUTS = [
  { key: "drums", label: "Drums", code: "sample :loop_amen, beat_stretch: 2", sleep: 2 },
  { key: "pluck", label: "Pluck", code: "synth :pluck, note: :e4", sleep: 1, play: (n) => `synth :pluck, note: ${n}` },
  { key: "pad", label: "Pad", code: "synth :dsaw, note: :e2, sustain: 4", sleep: 4, play: (n) => `synth :dsaw, note: ${n}, sustain: 2` },
  // the FX that work on pitch want what native's autotuner asks for: one clean voice, and one a little out of tune —
  // four tenths of a semitone sharp, sour on its own, pulled into tune by the FX (its switch off and on is the two)
  { key: "tone", label: "Sour tone", code: "synth :saw, note: 60.4, sustain: 4", sleep: 4, kinds: ["pitch"], play: (n) => `synth :saw, note: ${n}, sustain: 2` },
  { key: "voice", label: "Voice", code: "sample :ambi_choir", sleep: 4 },
];
const FX_KINDS = {
  space: ["reverb", "gverb", "echo", "ping_pong"],
  motion: ["flanger", "slicer", "panslicer", "wobble", "tremolo", "ixi_techno"],
  pitch: ["autotuner", "pitch_shift", "octaver", "whammy", "vowel"],
};
const FX_FIRST_INPUT = "drums";
const fxKind = (key) => Object.keys(FX_KINDS).find((k) => FX_KINDS[k].includes(key)) ?? "whole";
// the keys are a note to play: an FX that works on pitch has one (the autotuner's note, the whammy's), the rest none
const PITCH_FX = new Set(FX_KINDS.pitch);

/** A paragraph that is a line of code and nothing else. */
const isCodeLine = (e) => e.tagName === "P" && e.children.length === 1 && e.firstElementChild.tagName === "CODE" && e.textContent.trim() === e.firstElementChild.textContent.trim();

/** A control's tooltip: the doc's first sentence and the default, not the whole docstring (that is the detail below). */
function optTip(opt) {
  const first = (opt.doc ?? "").trim().match(/^.*?[.!?](?=\s|$)/s)?.[0] ?? (opt.doc ?? "").trim();
  const d = opt.default;
  const def = typeof d === "string" && d.startsWith(":") ? `same as ${d.slice(1)}` : String(d ?? "");
  const said = first && !/[.!?]$/.test(first) ? `${first}.` : first;   // a doc that forgets its full stop
  return def ? `${said} Default: ${def}` : said;
}

// a panel's label for an opt, as a console abbreviates: attack_level → ATK LVL, max_delay_time → MAX DLY TIME. The
// name in full is the dial's tip's, and its aria-label's.
const PANEL_WORDS = { attack: "atk", decay: "dcy", sustain: "sus", release: "rel", level: "lvl", delay: "dly", frequency: "freq", modulation: "mod", resonance: "res", velocity: "vel", detune: "dtn", vibrato: "vib", phase: "phs", offset: "ofs", width: "wdth", pulse: "pls", cutoff: "cut", amplitude: "amp", depth: "dep", time: "tm", pitch: "pch", resolution: "rsl", length: "len", length1: "len1", length2: "len2", intermediate: "mid", slope: "slp", formant: "fmt", ratio: "rat", shelf: "shlf", low: "lo", high: "hi" };
const panelLabel = (name) => {
  const words = name.replace(/pulse_width/g, "pw").split("_");   // a synth's own PW
  return (words.length > 1 ? words.map((w) => PANEL_WORDS[w] ?? w) : words).join(" ");   // a one-word name stays whole
};

/** TutDial::valueText: whole numbers on an integer step, else two places, trimmed. */
export const dialText = (value, step) => (step >= 1 ? String(Math.round(value)) : String(Number(value.toFixed(2))));

function dial(opt, onChange) {
  let lo = opt.min, hi = opt.max;
  const step = Math.min(1, gridFor(lo, hi));
  if (opt.min_excl) lo += step;
  if (opt.max_excl) hi -= step;
  const def = typeof opt.default === "number" ? Math.min(hi, Math.max(lo, opt.default)) : lo;
  let value = def;
  // a default that names another opt (decay_level: :sustain_level, tb303's cutoff envelope: :attack …): paired, as
  // native's is — it shows and moves with that one until it is turned itself, and a reset pairs it again
  const follows = opt.tracks ?? (typeof opt.default === "string" && opt.default.startsWith(":") ? opt.default.slice(1) : null);
  let partner = null, paired = !!follows;

  const root = el("div", "dial");
  root.tabIndex = 0;
  root.title = follows ? `${optTip(opt)}\nTracks ${follows} until it is turned itself (double-click to track it again)` : optTip(opt);
  root.dataset.tipLink = `#opt-${opt.name}`;   // its tip goes on to the opt's detail below (tooltip.js)
  root.dataset.tipTitle = opt.name;   // and leads with its name, large: the panel's label is a small one
  // a slider to a screen reader: the opt's name, its range and its value
  root.setAttribute("role", "slider");
  root.setAttribute("aria-label", opt.name);
  root.setAttribute("aria-valuemin", String(lo));
  root.setAttribute("aria-valuemax", String(hi));
  const NS = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(NS, "svg");
  svg.setAttribute("viewBox", "0 0 48 48");
  const arc = (from, to) => {
    const r = 18, cx = 24, cy = 24;
    const a0 = (from * Math.PI) / 180, a1 = (to * Math.PI) / 180;
    const large = to - from > 180 ? 1 : 0;
    return `M ${cx + r * Math.cos(a0)} ${cy + r * Math.sin(a0)} A ${r} ${r} 0 ${large} 1 ${cx + r * Math.cos(a1)} ${cy + r * Math.sin(a1)}`;
  };
  const track = document.createElementNS(NS, "path");
  track.setAttribute("class", "dial-track");
  track.setAttribute("d", arc(135, 405));
  const fill = document.createElementNS(NS, "path");
  fill.setAttribute("class", "dial-fill");
  // native's knob: a dot riding the ring, the value written inside it
  const pointer = document.createElementNS(NS, "circle");
  pointer.setAttribute("class", "dial-pointer");
  pointer.setAttribute("r", "4");
  svg.append(track, fill, pointer);
  const val = el("div", "dial-val");
  // the panel's label: one line, a ship's console's (style.css); the tip has it whole and large
  const label = el("div", `dial-label${panelLabel(opt.name).length > 10 ? " long" : ""}`, panelLabel(opt.name));
  label.setAttribute("aria-hidden", "true");   // the slider is named by aria-label already
  root.append(svg, val, label);

  const paint = () => {
    const frac = (value - lo) / (hi - lo || 1);
    const angle = 135 + frac * 270;
    fill.setAttribute("d", frac > 0.001 ? arc(135, angle) : "");
    const a = (angle * Math.PI) / 180;
    pointer.setAttribute("cx", 24 + 18 * Math.cos(a));
    pointer.setAttribute("cy", 24 + 18 * Math.sin(a));
    val.textContent = dialText(value, step);
    root.setAttribute("aria-valuenow", String(value));
    root.setAttribute("aria-valuetext", paired ? `${dialText(value, step)}, tracking ${follows}` : dialText(value, step));
    root.classList.toggle("changed", follows ? !paired : Math.abs(value - def) > step / 2);
    root.classList.toggle("paired", paired);
  };
  const set = (v, notify = true) => {
    v = Math.min(hi, Math.max(lo, Math.round(v / step) * step));
    if (v === value) return;
    value = v;
    paint();
    if (notify) onChange();
  };

  // turned by the player: a paired dial lets go of its partner, and is its own from here
  const turn = (v) => { if (paired) { paired = false; paint(); if (Math.abs(v - value) < step / 2) onChange(); } set(v); };
  const repair = () => { if (!follows) return set(def); paired = true; if (partner) set(partner.value, false); paint(); onChange(); };
  root.addEventListener("pointerdown", (e) => {
    e.preventDefault();   // a drag turns the dial, never selects the page's text around it
    root.setPointerCapture(e.pointerId);
    root.focus({ preventScroll: true });
    const y0 = e.clientY, v0 = value;
    const move = (m) => turn(v0 + ((y0 - m.clientY) / (m.shiftKey ? 1000 : 150)) * (hi - lo));
    const up = () => { root.removeEventListener("pointermove", move); root.removeEventListener("pointerup", up); };
    root.addEventListener("pointermove", move);
    root.addEventListener("pointerup", up);
  });
  root.addEventListener("wheel", (e) => { e.preventDefault(); turn(value - Math.sign(e.deltaY) * Math.max(step, (hi - lo) / 100)); }, { passive: false });
  root.addEventListener("dblclick", repair);
  root.addEventListener("keydown", (e) => {
    const big = Math.max(step, (hi - lo) / 20);
    if (e.key === "ArrowUp" || e.key === "ArrowRight") { turn(value + (e.shiftKey ? step : big)); e.preventDefault(); e.stopPropagation(); }
    if (e.key === "ArrowDown" || e.key === "ArrowLeft") { turn(value - (e.shiftKey ? step : big)); e.preventDefault(); e.stopPropagation(); }
  });
  paint();
  return {
    root,
    name: opt.name,
    get value() { return value; },
    get changed() { return follows ? !paired : Math.abs(value - def) > step / 2; },
    text: () => dialText(value, step),
    reset: () => { if (follows) { paired = true; if (partner) set(partner.value, false); paint(); } else set(def, false); },
    set,
    follows,
    /** Its partner (the dial its default names): paired, it shows the partner's value. */
    pair(d) { partner = d; if (paired) { value = Math.min(hi, Math.max(lo, d.value)); paint(); } },
    /** Its partner has moved: paired, it moves with it. */
    follow() { if (paired && partner) set(partner.value, false); },
  };
}

// A choice among a few values (tb303's wave, hollow's noise, env_curve): chips named from the opt's doc ("0=saw
// wave, 1=pulse", "0 saw, 1 pulse", "0 => 12.5%"), on/off for a bare 0 or 1, else the numbers. A dial's interface.
function choiceNames(opt) {
  const names = new Map(), doc = opt.doc ?? "";
  const zip = /(-?\d+(?:\s*,\s*-?\d+)+)\s*=>\s*(.+)/.exec(doc);   // "1,2,3,4,5 => A,E,I,O,U"
  if (zip) { const vs = zip[1].split(",").map(Number), ws = zip[2].split(","); vs.forEach((v, i) => ws[i] && names.set(v, ws[i].trim())); }
  else for (const piece of doc.split(/,|;| and | or /)) {
    const m = /(-?\d+)\s*(?:=>|=|\s+for\s+(?:an?\s+)?|\s)\s*(.+)/.exec(piece.trim());   // "0=saw wave", "0 saw", "0 => 12.5%", "Use 0 for a sine wave"
    if (!m) continue;
    const v = Number(m[1]);
    const word = m[2].split(/\.(?:\s|$)/)[0].replace(/\(.*?\)/g, "").replace(/^resonant\s+/i, "").replace(/\s+(wave|noise|filter|signal)\b.*$/i, "").trim();
    if (opt.options.includes(v) && word && !names.has(v)) names.set(v, word);
  }
  if (opt.options.every((v) => names.has(v))) return (v) => names.get(v);
  if (opt.options.length === 2 && opt.options.includes(0) && opt.options.includes(1)) return (v) => (v ? "on" : "off");
  return (v) => String(v);
}
function choice(opt, onChange) {
  const def = opt.default;
  let value = def;
  const root = el("div", "choice");
  // off or on (hollow's norm): a switch, as a preference is, not two chips
  if (opt.options.length === 2 && opt.options.includes(0) && opt.options.includes(1)) {
    const sw = el("button", "switch pg-toggle");
    sw.type = "button";
    sw.setAttribute("role", "switch");
    sw.setAttribute("aria-label", opt.name);
    sw.title = optTip(opt);
    sw.appendChild(el("span", "track")).setAttribute("aria-hidden", "true");
    sw.addEventListener("click", () => set(value ? 0 : 1));
    const label = el("div", "dial-label", panelLabel(opt.name));
    label.title = optTip(opt);
    label.dataset.tipLink = `#opt-${opt.name}`;
    label.dataset.tipTitle = opt.name;
    root.classList.add("choice-toggle");
    root.append(sw, label);
    const paint = () => { sw.classList.toggle("on", !!value); sw.setAttribute("aria-checked", String(!!value)); root.classList.toggle("changed", value !== def); };
    const set = (v, notify = true) => { if (v === value) return; value = v; paint(); if (notify) onChange(); };
    paint();
    return { root, name: opt.name, get value() { return value; }, get changed() { return value !== def; }, text: () => String(value), reset: () => set(def, false), set };
  }
  const row = el("div", "seg");
  row.setAttribute("role", "radiogroup");
  row.setAttribute("aria-label", opt.name);
  const name = choiceNames(opt);
  const buttons = opt.options.map((v) => {
    const b = el("button", "", name(v));
    b.type = "button";
    b.title = `${opt.name}: ${v}`;
    b.setAttribute("role", "radio");
    b.addEventListener("click", () => set(v));
    row.appendChild(b);
    return [v, b];
  });
  const label = el("div", "dial-label", panelLabel(opt.name));
  label.title = optTip(opt);
  label.dataset.tipLink = `#opt-${opt.name}`;
  label.dataset.tipTitle = opt.name;
  root.append(row, label);
  const paint = () => {
    for (const [v, b] of buttons) { b.classList.toggle("active", v === value); b.setAttribute("aria-checked", String(v === value)); }
    root.classList.toggle("changed", value !== def);
  };
  const set = (v, notify = true) => { if (v === value) return; value = v; paint(); if (notify) onChange(); };
  paint();
  return { root, name: opt.name, get value() { return value; }, get changed() { return value !== def; }, text: () => String(value), reset: () => set(def, false), set };
}

// ── The pane ──────────────────────────────────────────────────────────────

/**
 * @param root element to fill
 * @param ref { lang, synths, fx, samples, examples, support } reference data; support is runtime-support.json, or null
 * @param hooks { run(code) → Promise<job>, stop(job), insert(code), copy(code), playSample(name) }
 */
// an icon's button: its glyph, its words for a screen reader, and its tip
function iconButton(cls, icon, label, onClick) {
  const b = button(cls, null, onClick);
  b.title = label;
  b.innerHTML = `${svgIcon(icon)}<span class="sr-only">${label}</span>`;
  return b;
}

/**
 * The live synth (or FX): its title, the keys and Play/Stop, the program the dials make, and the dials — the docs
 * pane's instrument page, and the home page's (info.js). A synth's or FX's page from the reference (ref.synths/fx);
 * hooks { play, run, copy, insert }; the deck its Play runs as a card of. Its QWERTY keys are keyHandler's, for
 * whoever holds it to listen on.
 */
// the first sentence of a page's words (its doc_html): a synth's or an FX's description in one line
function firstSentence(html) {
  const d = document.createElement("div");
  d.innerHTML = html ?? "";
  const text = d.textContent.replace(/\s+/g, " ").trim();
  return /^.+?[.!?](?=\s|$)/.exec(text)?.[0] ?? text;
}
// the element a page scrolls in (the docs pane's content, a site page's body), or the window's
function scrollerOf(node) {
  for (let n = node.parentElement; n; n = n.parentElement) {
    const o = getComputedStyle(n).overflowY;
    if ((o === "auto" || o === "scroll") && n.scrollHeight > n.clientHeight) return n;
  }
  return document.scrollingElement ?? document.documentElement;
}

// the dials a first look needs (Basic): a synth's note, how long it rings and how loud (CORE), and the two of its own
// its metadata names (gui.basic: native's SynthInfo GUI_BASIC, or a user synth's .json), else its first two; an FX's mix and its own
// first two. The rest (pan, the envelope's levels, the choices…) are All's.
const FX_GENERIC = new Set(["mix", "amp", "pre_amp", "pre_mix"]);
function basicOf(p, isFx, shown) {
  const has = new Set(shown.map((o) => o.name));
  // pan and the envelope are a synth's, shown in its own modules, so they are not among the two of its own that
  // Basic picks. An FX's are its actual controls — the Pan FX's pan, the flanger's decay — so for an FX they count.
  const elsewhere = (name) => !isFx && /^(pan|attack|decay|sustain)(_level)?$/.test(name);
  const own = shown.filter((o) => !FX_GENERIC.has(o.name) && !CORE.includes(o.name) && !elsewhere(o.name)).map((o) => o.name);
  const spare = (p.gui?.basic ?? own).filter((n) => has.has(n)).slice(0, 2);   // the synth's metadata says (native's GUI_BASIC; a user's .json)
  const core = isFx ? [] : CORE.filter((n) => has.has(n));   // an FX's mix, note and amp are its Out module's (UNIVERSAL below), as a synth's note, amp and pan are
  return { names: new Set([...core, ...spare]), spare: new Set(spare) };
}

export function createInstrument(p, isFx, hooks, deck, { fit = false, basic = true, place = "docs", heading = 1, open = null } = {}) {
  // The synth is a code card (ui/card.js): its name and Basic switch in the card's bar with the card's actions, the
  // synth itself — its knobs and keys — the card's panel, the code they make the card's code, and the card's foot
  // (the words, Play and Stop with their rings, the strip of its sounds). `face` is the panel.
  const face = el("div", `pg-face${isFx ? " pg-fx" : ""}`);   // an FX's rack is a grid (style.css): few modules, no keys under them
  const name = /[a-z][A-Z]/.test(p.title) ? titleCase(p.key) : p.title;
  const numeric = p.opts.map((o) => ranged(o, p.opts)).filter((o) => typeof o.min === "number" && typeof o.max === "number" && o.max > o.min && !o.name.endsWith("_slide") && !o.options);
  let shown = numeric;
  if (isFx) shown = [...numeric.filter((o) => o.name !== "mix" && o.name !== "amp"), ...numeric.filter((o) => o.name === "mix" || o.name === "amp")];
  const choices = p.opts.filter((o) => Array.isArray(o.options) && o.options.length > 1 && !o.name.endsWith("_slide"));   // every control the synth has: no cap, and the choices too
  const dials = [];
  const groups = el("div", "pg-groups");
  // An FX's modules, fewer and fuller: everything about level in one (pre_amp and pre_mix with amp and mix — the
  // signal's way in and out), and a module left holding a single control folded into Character, which is where an
  // opt with no module of its own goes anyway — not six ragged boxes, half of them holding one knob.
  const singles = !isFx ? new Set() : new Set(GROUPS.filter((g) => g !== "CHARACTER" && g !== "OUT"
    && shown.filter((o) => groupFor(o) === g).length + choices.filter((o) => groupFor(o) === g).length === 1));
  const moduleFor = (o) => {
    const g = groupFor(o);
    if (!isFx) return g;
    if (g === "OUT") return "OUT";          // named Levels below: every level opt together
    return singles.has(g) ? "CHARACTER" : g;
  };
  for (const g of GROUPS) {
    const opts = shown.filter((o) => moduleFor(o) === g), picks = choices.filter((o) => moduleFor(o) === g);
    if (!opts.length && !picks.length) continue;
    const box = el("section", "pg-group");
    const name = g === "OUT" && isFx ? "LEVELS" : g;   // in and out and how much of each: one module, not an Out beside an Out
    box.appendChild(el(`h${Math.min(6, heading + 1)}`, "pg-group-name", name));
    if (opts.length) {
      const row = el("div", "pg-dials");
      row.style.setProperty("--cols", opts.length <= 3 ? opts.length : Math.ceil(opts.length / 2));   // at most two rows: the faceplate stays short
      row.style.setProperty("--n", opts.length);   // all in one row, where a layout wants it (the home page's)
      for (const o of opts) {
        const d = dial(o, update);
        dials.push(d);
        row.appendChild(d.root);
      }
      box.appendChild(row);
    }
    if (picks.length) {
      const list = el("div", "pg-choices");
      for (const o of picks) { const c = choice(o, update); dials.push(c); list.appendChild(c.root); }
      box.appendChild(list);
    }
    groups.appendChild(box);
  }

  // Basic or All: a toggle in the title's row, remembered for where the instrument is (the home page's, the docs pane's)
  const { names: basicNames, spare } = basicOf(p, isFx, shown);
  for (const d of dials) if (spare.has(d.name)) d.root.classList.add("pg-spare");   // its own expressive controls: the secondary colour
  const DIALS_KEY = `sp-dials-${place}`;
  // a phone: the full set is a full screen of its own (a synth's panel is taller than the phone), so it always opens
  // on Basic, the compact one, whatever was left last time
  const pendingBar = [];   // the card's bar extras (the Basic switch), made before the card is
  const phone = matchMedia("(max-width: 760px)");
  let showBasic = (() => { if (phone.matches) return true; const v = store.get(DIALS_KEY); return v == null ? basic : v === "basic"; })();
  const viewSwitch = button("switch pg-view", null, () => {
    // the keys stay where they are on screen: the dials the switch brings in open upward, above them
    const keysAt = keysWrap.getBoundingClientRect().top;
    showBasic = !showBasic;
    store.set(DIALS_KEY, showBasic ? "basic" : "all");
    paintView();
    placeRack();
    // held only while the switch stays in view (a phone's panel is taller than its screen): else the switch, just
    // pressed, stays where the finger left it, and the dials open under it
    if (phone.matches) return fullScreen(!showBasic);   // a phone's full set: its own screen, nothing on the page to hold still
    const sc = scrollerOf(face), view = sc === document.scrollingElement ? 0 : sc.getBoundingClientRect().top;
    const hold = () => {
      const moved = keysWrap.getBoundingClientRect().top - keysAt;
      if (Math.abs(moved) > 0.5 && viewSwitch.getBoundingClientRect().top - moved >= view + 8) sc.scrollBy({ top: moved, behavior: "instant" });
    };
    hold();
    requestAnimationFrame(hold);   // and again once the keys have re-fitted to their room
    if (!showBasic) for (const d of dials) if (!basicNames.has(d.name)) { d.root.classList.remove("pg-in"); void d.root.offsetWidth; d.root.classList.add("pg-in"); }
  });
  // the full set as a screen of its own on a phone: the rack scrolls, the transport, display and keys stay at its foot;
  // Basic (the switch, in its title) or Escape brings the compact synth back
  let home = null;   // where the card sits on its page, while it is the screen (fullScreen)
  function fullScreen(on) {
    face.classList.toggle("fullscreen", on);
    card.el.classList.toggle("fullscreen", on);   // the whole card, its foot's Play and Stop with it
    // over everything while it is the screen: a page's layers (the site's overlay is one, at its own z-index) would
    // otherwise keep their bars over it whatever its own z-index, so it goes into the top layer, as a popover does —
    // still in its place in the page, its styles and its keys with it. A browser without popovers: out to the body.
    if (card.el.showPopover) {
      if (on && !card.el.matches(":popover-open")) { card.el.popover = "manual"; card.el.showPopover(); }
      else if (!on && card.el.popover) { if (card.el.matches(":popover-open")) card.el.hidePopover(); card.el.removeAttribute("popover"); }
    } else if (on && !home) { home = document.createComment("synth"); card.el.before(home); document.body.appendChild(card.el); }
    else if (!on && home) { home.replaceWith(card.el); home = null; }
    // the height of what shows (Safari's toolbar floats over the layout viewport's foot, and moves): the visual viewport's
    const vv = window.visualViewport;
    // a sheet on the foot of what shows, as tall as the synth needs and no taller than the screen
    // (in the card's own pixels: a pane's zoom, the docs pane's, scales them)
    const fitScreen = () => {
      if (!vv) return;
      const z = card.el.offsetWidth ? card.el.getBoundingClientRect().width / card.el.offsetWidth : 1;
      card.el.style.maxHeight = `${vv.height / z}px`;
      card.el.style.bottom = `${Math.max(0, window.innerHeight - vv.offsetTop - vv.height) / z}px`;
    };
    if (on) { fitScreen(); vv?.addEventListener("resize", fitScreen); vv?.addEventListener("scroll", fitScreen); card.fitScreen = fitScreen; }
    else { vv?.removeEventListener("resize", card.fitScreen); vv?.removeEventListener("scroll", card.fitScreen); card.el.style.maxHeight = card.el.style.bottom = ""; }
    document.documentElement.classList.toggle("pg-fullscreen", on);
    placeRack();
    if (on) viewSwitch.focus({ preventScroll: true });
    else card.el.scrollIntoView({ block: "nearest" });
  }
  phone.addEventListener("change", () => { if (!phone.matches && face.classList.contains("fullscreen")) fullScreen(false); else if (phone.matches && !showBasic) fullScreen(true); });
  viewSwitch.setAttribute("role", "switch");
  viewSwitch.title = "Basic: the dials to start with. Off: every opt the synth has";
  viewSwitch.append(el("span", "track"), el("span", "", "Basic"));
  viewSwitch.firstChild.setAttribute("aria-hidden", "true");
  pendingBar.push(viewSwitch);   // into the card's bar, after its name (the card is made once the program is)
  // Basic's line, in two regions of its own: the core three (what, how long, how loud), and the synth's own
  // expressive pair in the secondary colour. The dials move there and back (each remembers its row and place), so
  // a dial turned in one view is the same dial in the other.
  const basicLine = el("div", "pg-basic");
  const coreRegion = el("div", "pg-region pg-region-core"), spareRegion = el("div", "pg-region pg-region-spare");
  coreRegion.setAttribute("role", "group"); coreRegion.setAttribute("aria-label", "The basics");
  coreRegion.appendChild(el("span", "pg-module-name", isFx ? "Mix" : "Env"));   // a synth's core is its envelope; an FX's is how much of it is heard
  spareRegion.setAttribute("role", "group"); spareRegion.setAttribute("aria-label", `${isFx ? "The FX's" : "The synth's"} own`);
  spareRegion.appendChild(el("span", "pg-module-name", titleCase(p.key)));
  basicLine.append(coreRegion, spareRegion);
  // note, amp and pan: every synth's, so every synth has them in the same place — beside Play and Stop, over the keys,
  // in Basic and All alike; the panel above is what differs from synth to synth. An FX's Out is the same idea: how
  // much of it is heard and how loud, with the note it pulls to when it works on pitch
  const UNIVERSAL = isFx ? UNIVERSAL_FX.filter((n) => n !== "note" || PITCH_FX.has(p.key)) : ["note", "amp", "pan"];
  const universal = el("div", "pg-universal");
  universal.setAttribute("role", "group");
  universal.setAttribute("aria-label", isFx ? "Mix and amp" : "Note, amp and pan");
  universal.appendChild(el("span", "pg-module-name", "Out"));
  // a dial's home is the module it was built in, taken before the universal three are moved out of theirs: an FX
  // shows them in its Levels module in All, and beside Play and Stop in Basic, so they go home and back
  const homes = new Map(dials.map((d) => [d, { row: d.root.parentNode }]));
  const universalDials = UNIVERSAL.map((n) => dials.find((x) => x.name === n)).filter(Boolean);
  for (const d of universalDials) universal.appendChild(d.root);
  const hasUniversal = universalDials.length > 0;
  const order = (names) => [...names].map((n) => dials.find((d) => d.name === n)).filter(Boolean);
  function paintView() {
    face.classList.toggle("basic", showBasic);
    if (showBasic) {
      coreRegion.append(...order([...basicNames].filter((n) => !spare.has(n) && !UNIVERSAL.includes(n))).map((d) => d.root));
      spareRegion.append(...order(spare).map((d) => d.root));
    } else {
      for (const d of dials) homes.get(d).row.appendChild(d.root);   // each back in its row, in the order it was made
      if (!isFx) for (const d of universalDials) universal.appendChild(d.root);   // a synth keeps note, amp and pan over its keys in both views
    }
    if (showBasic) for (const d of universalDials) universal.appendChild(d.root);
    universal.hidden = isFx && !showBasic;   // an FX's levels are its Levels module's in All: no Out module as well
    spareRegion.hidden = !spareRegion.querySelector(".dial");
    coreRegion.hidden = !coreRegion.querySelector(".dial");   // an FX's core (mix, amp) is its Out module's: nothing left here
    // In lives on this line too (placeRack), so the line stays for it — asked of the DOM, not of `inputBox`, which
    // is made further down: paintView runs before it exists, and an FX with no knobs of its own reaches this operand
    basicLine.hidden = !showBasic || (coreRegion.hidden && spareRegion.hidden && !basicLine.querySelector(".pg-region-in"));
    groups.hidden = showBasic;
    viewSwitch.classList.toggle("on", showBasic);
    viewSwitch.setAttribute("aria-checked", String(showBasic));
    for (const d of dials) d.root.hidden = showBasic && !basicNames.has(d.name) && !UNIVERSAL.includes(d.name);
    for (const box of groups.querySelectorAll(":scope > .pg-group")) {   // the modules of knobs; In sits among them and has none
      for (const row of box.querySelectorAll(".pg-dials")) {
        const n = [...row.children].filter((c) => !c.hidden).length;
        row.style.setProperty("--cols", n <= 3 ? n : Math.ceil(n / 2));
        row.style.setProperty("--n", n);
      }
      box.hidden = ![...box.querySelectorAll(".dial, .choice")].some((c) => !c.hidden);
    }
  }
  paintView();

  // the paired: each shown with its partner, and moved with it on every change (update, below)
  for (const d of dials) if (d.follows) { const src = dials.find((x) => x.name === d.follows); if (src) d.pair(src); }
  const followAll = () => { for (const d of dials) if (d.follows) d.follow(); };

  // an FX's demo: what goes into it, and whether the FX itself is in the way (its row, below)
  let input = FX_FIRST_INPUT, fxOn = true;
  const noteDial = () => dials.find((d) => d.name === "note");
  const optsText = (skipNote) => dials.filter((d) => d.changed && !(skipNote && d.name === "note")).map((d) => `${d.name}: ${d.text()}`).join(", ");
  // the program follows the keys, as native's does: a played note becomes the note it shows
  let lastNote = null;
  const programFor = (note, { live = false } = {}) => {
    if (isFx) {
      const opts = optsText(false), head = `with_fx :${p.key}${opts ? `, ${opts}` : ""} do`;
      if (note != null) {   // a key: that note through the FX, played by whatever the card is putting through it
        const src = FX_INPUTS.find((i) => i.key === input) ?? FX_INPUTS[0];
        return `${head}\n  ${src.play?.(note) ?? `synth :prophet, note: ${note}, release: 2, cutoff: 80`}\nend`;
      }
      // the demo: the chosen sound looping, through the FX or (the switch off) on its own, so the two can be heard
      // against each other; run live, the FX's node is kept (Time State) so a dial turned while it plays steers it
      const src = FX_INPUTS.find((i) => i.key === input) ?? FX_INPUTS[0];
      const loop = `live_loop :fx_demo do\n  ${src.code}\n  sleep ${src.sleep}\nend`;
      if (!fxOn) return loop;
      return `${live ? `${head.slice(0, -3)} do |fx|\n  set :docs_fx, fx` : head}\n  ${loop.replace(/\n/g, "\n  ")}\nend`;
    }
    const n = note ?? (noteDial() ? noteDial().text() : lastNote ?? 52);
    const opts = optsText(true);
    return `use_synth :${p.key}\nplay ${n}${opts ? `, ${opts}` : ""}`;
  };

  // The card: the code the dials make its code (reset puts the dials back; copy and insert take the code), Play and
  // Stop its transport, on press as the keys are
  const card = createCard({
    title: name, key: `instrument/${p.key}`, code: programFor(null), actions: open ? ["reset", "copy"] : ["reset", "copy", "insert"], still: true, onPress: true, level: heading,
    // the synth's one-sentence summary (its metadata's), else its description's first sentence — but not on its own
    // docs page, where its full description is the next thing under the card and the summary only says it again
    blurb: place === "docs" ? "" : (p.summary || firstSentence(p.doc_html)),
    // every hook the card itself reads (ui/card.js): its loop scopes need scopeFrame and loopScopes as much as its
    // buttons need copy and insert — without them a loop's scope is placed on the line and then fed nothing, which
    // draws as a flat line under a synth that is plainly playing
    hooks: { copy: hooks.copy, insert: hooks.insert, now: hooks.now, synthDefaults: hooks.synthDefaults, scopeFrame: hooks.scopeFrame, loopScopes: hooks.loopScopes },
    panel: face, extra: pendingBar,
    // away from the editor (the home page), Open in Sonic Pi in place of Insert: the code into the editor, its sound going on
    open: open && ((code) => open(code, demo.job)),
  });
  card.el.classList.add("pg-card");
  // a knob's tip links to its opt's detail: below it on a docs page; from anywhere else (the home page), that detail on
  // the synth's docs page
  if (place !== "docs") for (const d of dials) for (const t of [d.root, ...d.root.querySelectorAll("[data-tip-link]")]) {
    if (t.dataset.tipLink?.startsWith("#opt-")) t.dataset.tipLink = `code.html#docs/${isFx ? "fx" : "synths"}/${p.key}/${t.dataset.tipLink.slice(1)}`;
  }
  const cardFoot = card.el.querySelector(".qs-card-foot");
  card.onReset = () => { dials.forEach((d) => d.reset()); update(); };
  // Until the synth has loaded (its first sound: the engine fetches its definition), a press is held, not queued:
  // each press takes the place of the last, and the last plays once it has loaded; from then on, every press plays
  let held = null, loading = null;
  const whenLoaded = (fire) => {
    if (isFx || p.user || hooks.synthReady?.(p.key) !== false) return fire();
    held = fire;
    if (loading) return;
    card.setBooting(true);
    loading = Promise.resolve(hooks.loadSynth?.(p.key)).catch(() => {}).finally(() => { loading = null; card.setBooting(false); const f = held; held = null; f?.(); });
  };
  card.onRun = () => whenLoaded(() => demo.onRun?.());
  card.onStop = () => demo.onStop?.();
  card.el.addEventListener("keydown", (e) => { if (home) keyHandler(e); });   // out on the body (fullScreen): the page's own listener no longer hears it
  card.el.addEventListener("keydown", (e) => { if (e.key === "Escape" && face.classList.contains("fullscreen")) { e.stopPropagation(); viewSwitch.click(); } });

  // What the page's deck plays: the card's program, as a key or Play asks for it. Its own group (Stop fades it and
  // nothing else) and scope slot (the card's rings draw its sound); Play again runs it again. Space plays it too.
  let keyNote = null;   // a key being played: the demo runs its note (demo.code)
  const demo = {
    el: card.el, key: `instrument/${p.key}`, title: p.title, job: null, onRun: null, onStop: null, onReset: null,
    code: () => {
      if (keyNote != null) return `use_real_time\nuse_debug false\n${programFor(keyNote)}`;   // a key: its note, in the demo's group and scope, so the rings show it and Stop stops it
      sent.clear(); for (const d of dials) sent.set(d.name, d.text());   // what the demo starts with
      // a synth's is one note, as a key's is: played now, not a schedule-ahead later. An FX's is a loop the dials
      // steer, which keeps the schedule-ahead that holds its rhythm steady (its steering is real time)
      return isFx ? programFor(null, { live: true }) : `use_real_time\n${programFor(null, { live: true })}`;   // logged as Play always has been
    },
    setPlaying: (on, readFrame) => card.setPlaying(on, readFrame),
    setBooting: (on) => card.setBooting(on), setError: (m) => card.setError(m), clearOutput: () => card.clearOutput(), flash() {},
    record: (r) => card.record(r),
    detach: () => card.detach(),
  };
  deck.add(demo);
  const toggleDemo = () => demo.onRun?.();
  // The QWERTY keyboard
  const KEYS = "awsedftgyhujkolp";
  let base = 48;
  const keysWrap = el("div", "pg-keys");
  const pianoBox = el("div", "pg-piano");
  // four octaves of 30px keys, scrolling where they do not fit (the docs pane); or, fit (the home page's), as many
  // whole octaves as the room holds, from two to four, the keys widened to fill it: nothing cut off, nothing to scroll
  let drawnFor = 0;
  const drawPiano = () => {
    const labels = {};
    [...KEYS].forEach((k, i) => (labels[base + i] = k));
    let octaves = 4, W = 30;
    if (fit) {
      const room = pianoBox.clientWidth || 600;
      drawnFor = room;
      octaves = Math.max(2, Math.min(4, Math.floor(room / (7 * 26))));
      W = Math.max(16, Math.min(44, room / (octaves * 7 + 1)));   // down to 16px; not rounded, so the keys end where the room does
    }
    pianoBox.textContent = "";
    pianoBox.appendChild(piano({ from: base, to: base + octaves * 12, labels, onKey: playKey, keyWidth: W }));
    pianoBox.title = `From C${base / 12 - 1}`;
  };
  if (fit) new ResizeObserver(() => { if (pianoBox.clientWidth && Math.abs(pianoBox.clientWidth - drawnFor) > 4) drawPiano(); }).observe(pianoBox);
  const shiftOctave = (by) => { base = Math.min(96, Math.max(24, base + by)); drawPiano(); };
  const down = button("zoom-btn", "", () => shiftOctave(-12));   // the app's − and + (the panes' zoom buttons), Tabler's circles
  down.innerHTML = glyph("circle-minus", "");
  down.title = "Octave down (z)"; down.setAttribute("aria-label", "Octave down");
  const up = button("zoom-btn", "", () => shiftOctave(12));
  up.innerHTML = glyph("circle-plus", "");
  up.title = "Octave up (x)"; up.setAttribute("aria-label", "Octave up");
  const withKey = (b, key) => { const w = el("span", "pg-octave"); w.append(b, el("kbd", "", key)); return w; };   // each octave button with its key under it
  keysWrap.append(pianoBox);   // the keys have the whole width: the octave buttons are over them
  // over the keys: Play and Stop and the octave buttons on the left, the universal three on the right
  const deckRow = el("div", "pg-deck");
  const octaves = el("span", "pg-octaves");
  octaves.append(withKey(down, "z"), withKey(up, "x"));
  const left = el("span", "pg-deck-left");
  left.append(octaves);
  deckRow.append(left, universal);   // Play, Stop and the octaves; note, amp and pan at the right

  // An FX's own row: what goes into it, and the FX itself on or off. The sound chosen loops while it plays, so a knob
  // turned is heard; the switch off plays that sound alone, to hear what the FX is doing to it.
  const inputBox = el("div", "pg-region pg-region-in");
  if (isFx) {
    inputBox.setAttribute("role", "group");
    inputBox.setAttribute("aria-label", "What goes into the FX");
    inputBox.appendChild(el("span", "pg-module-name", "In"));
    const row = el("div", "seg pg-inputs");
    row.setAttribute("role", "radiogroup");
    row.setAttribute("aria-label", "What goes into the FX");
    const chips = FX_INPUTS.filter((src) => !src.kinds || src.kinds.includes(fxKind(p.key))).map((src) => {
      const chip = button("", src.label, () => {
        if (input === src.key) return;
        input = src.key;
        paintInput();
        update();
        if (demo.job != null) { demo.onStop?.(); setTimeout(() => demo.onRun?.(), 320); }   // playing: the new sound, through the same FX
      });
      chip.setAttribute("role", "radio");
      row.appendChild(chip);
      return [src.key, chip];
    });
    const fxSwitch = button("switch pg-fx-on on", null, () => { fxOn = !fxOn; paintInput(); update(); if (demo.job != null) { demo.onStop?.(); setTimeout(() => demo.onRun?.(), 320); } });
    fxSwitch.setAttribute("role", "switch");
    fxSwitch.title = `${p.title} on the sound. Off: the sound as it was, to hear what the FX does`;
    fxSwitch.append(el("span", "track"), el("span", "", "FX"));
    fxSwitch.firstChild.setAttribute("aria-hidden", "true");
    var paintInput = () => {
      for (const [key, chip] of chips) { chip.classList.toggle("active", key === input); chip.setAttribute("aria-checked", String(key === input)); }
      fxSwitch.classList.toggle("on", fxOn);
      fxSwitch.setAttribute("aria-checked", String(fxOn));
    };
    inputBox.append(fxSwitch, row);   // the switch in the module's top corner, as a pedal's own; the sounds fill the module below it
    paintInput();
    left.appendChild(inputBox);
  }
  function playKey(n) {
    // an FX that takes a note (the autotuner): the key is the note the running FX pulls its voice to — the piano
    // sings it — and starts the voice if it is not going yet
    if (isFx && noteDial()) {
      noteDial().set(n);
      if (demo.job == null) demo.onRun?.();
      const k = keysWrap.querySelector(`[data-note="${n}"]`);
      if (k) { k.classList.add("lit"); setTimeout(() => k.classList.remove("lit"), 180); }
      return;
    }
    whenLoaded(() => { keyNote = n; demo.onRun?.(); keyNote = null; });   // the deck reads the code at once, before its run awaits
    if (noteDial()) noteDial().set(n); else { lastNote = n; update(); }   // the dial's change re-renders the program
    const k = keysWrap.querySelector(`[data-note="${n}"]`);
    if (k) { k.classList.add("lit"); setTimeout(() => k.classList.remove("lit"), 180); }
  }
  drawPiano();

  function update() {
    followAll();   // a dial that tracks another moves with it
    card.setCode(programFor(null));
    card.el.classList.toggle("edited", dials.some((d) => d.changed));   // the card's Reset shows
    if (isFx && demo.job != null) steerDemo();
  }
  // The FX demo playing: a dial turned reaches the running FX at once (control, its slidable opts); one that cannot
  // slide (a choice, a buffer size) starts the demo again with it. Throttled, so a drag is a stream, not a flood.
  let steerTimer = null, steerRestart = false;
  const slides = new Set(p.opts.filter((o) => o.slidable).map((o) => o.name));
  const sent = new Map();
  function steerDemo() {
    for (const d of dials) if (!slides.has(d.name) && sent.has(d.name) && sent.get(d.name) !== d.text()) steerRestart = true;
    if (steerTimer) return;
    steerTimer = setTimeout(() => {
      steerTimer = null;
      if (demo.job == null) return;
      if (steerRestart) { steerRestart = false; demo.onStop?.(); setTimeout(() => demo.onRun?.(), 320); return; }   // a fresh FX with the new setting, once the old one has faded
      const args = dials.filter((d) => slides.has(d.name)).map((d) => `${d.name}: ${d.text()}`).join(", ");
      if (args) hooks.run(`use_real_time\nuse_debug false\ncontrol get(:docs_fx), ${args}`, { quiet: true });
    }, 40);
  }
  update();

  const pad = el("div", "pg-pad");
  pad.tabIndex = 0;
  // the rack: Basic's modules (or All's groups), and at its right end the universal three
  const modules = el("div", "pg-modules");
  modules.append(basicLine, groups);
  pad.append(modules);
  // as a synth is laid out: its controls on top, the keys under them (with Play and Stop), then the code they make
  if (isFx && !PITCH_FX.has(p.key)) { keysWrap.hidden = true; octaves.hidden = true; }   // nothing to play: the sound is the input's, the keys (and their octaves) a pitch FX's
  face.append(pad, deckRow, keysWrap);   // the code the dials and keys make, under the keys, the width of the synth
  // the rack in the row with Play, Stop and Out when it fits there (its dials beside Out's: all the knobs together),
  // else on a line of its own above: tried in the row, and moved up if the row would run over
  // Tried in turn, the first that fits: the octaves, the rack and Out in the one row; the rack and Out there with the
  // octave buttons in a slim bar over the keys, − over the low end and + over the high, the keys still the full width
  // (a phone: every knob on one line); else the rack on a line above
  const [downKey, upKey] = [...octaves.children];
  const octaveBar = el("div", "pg-octave-bar");
  const flank = (on) => {
    if (on) { octaves.remove(); octaveBar.append(downKey, upKey); keysWrap.prepend(octaveBar); }
    else { octaveBar.remove(); octaves.append(downKey, upKey); left.appendChild(octaves); }
    face.classList.toggle("pg-octaves-flank", on);
  };
  // Basic, for a synth: the row over the keys holds its opts, the pitch at its bottom left — the octave buttons and
  // the note — and release and amp at its bottom right; the synth's own knobs between them, or on a line above where
  // they do not fit. Pan is the full set's. All puts every opt back where the full set has it.
  const pitch = el("div", "pg-region pg-region-pitch");
  pitch.setAttribute("role", "group"); pitch.setAttribute("aria-label", "Octave and note");
  pitch.appendChild(el("span", "pg-module-name", "Pitch"));
  let basicOn = false;
  const byName = (n) => dials.find((d) => d.name === n);
  function basicMode(on) {
    if (on === basicOn) return;
    basicOn = on;
    face.classList.toggle("pg-basic-row", on);
    if (on) {
      octaves.remove(); octaveBar.remove();
      pitch.append(downKey, upKey, ...[byName("note")].filter(Boolean).map((d) => d.root));
      universal.append(...["release", "amp"].map(byName).filter(Boolean).map((d) => d.root));
      byName("pan")?.root.remove();   // the full set's
      universal.setAttribute("aria-label", "Release and amp");
      deckRow.prepend(pitch);
    } else {
      pitch.remove();
      octaves.append(downKey, upKey);
      universal.append(...UNIVERSAL.map(byName).filter(Boolean).map((d) => d.root));   // back in Out, in its order
      universal.setAttribute("aria-label", "Note, amp and pan");
      const rel = byName("release");
      if (rel) (showBasic && basicNames.has("release") ? coreRegion : homes.get(rel).row).appendChild(rel.root);
    }
    coreRegion.hidden = !coreRegion.querySelector(".dial");
  }
  // a module's knobs in one row where the room allows (the rack no taller than a knob), else in two
  const setCols = (one) => { for (const row of groups.querySelectorAll(".pg-dials")) { const n = row.querySelectorAll(":scope > .dial").length; row.style.setProperty("--cols", one || n <= 3 ? Math.max(1, n) : Math.ceil(n / 2)); } };

  function placeRack() {
    const had = [pad, octaves, keysWrap, deckRow].find((w) => w.contains(document.activeElement)) ? document.activeElement : null;
    // In is a module like the rest, and it sits in the same place in both views: at the head of the rack, whichever
    // rack is showing — Basic's line of the FX's own knobs, or All's grid. An FX with no keys has nothing else in the
    // row under All's rack, so that row goes.
    if (isFx && keysWrap.hidden) {
      const row = showBasic ? basicLine : groups;
      if (inputBox.parentNode !== row) row.prepend(inputBox);
      if (!showBasic) {
        deckRow.classList.remove("rack-in");
        face.insertBefore(pad, deckRow);   // the rack is the whole panel: it must be out of the row before that row goes
        deckRow.hidden = true;
        packRack(groups, { width: pad.clientWidth });   // the rack has the panel to itself: its rows planned, not filled greedily (rack.js)
        if (had && document.activeElement !== had) had.focus({ preventScroll: true });
        return;
      }
      basicLine.hidden = false;   // paintView hid it before In moved in: an FX with no knobs of its own still has this line
      deckRow.hidden = false;
    } else if (isFx && (deckRow.hidden || inputBox.parentNode !== left)) { deckRow.hidden = false; left.appendChild(inputBox); }   // a synth has no In module: its empty box would sit in the row
    basicMode(!isFx && showBasic && !face.classList.contains("fullscreen") && !!byName("note"));
    if (basicOn) {
      face.classList.remove("pg-octaves-flank");   // the octave buttons are the pitch module's
      deckRow.classList.add("rack-in");
      universal.before(pad);
      if (deckRow.scrollWidth > deckRow.clientWidth + 1) { deckRow.classList.remove("rack-in"); face.insertBefore(pad, deckRow); }   // the synth's own on a line above
      if (had && document.activeElement !== had) had.focus({ preventScroll: true });
      return;
    }
    const tryRow = (octavesHere) => {
      flank(!octavesHere);
      deckRow.classList.add("rack-in");
      universal.before(pad);
      return !face.classList.contains("fullscreen") && deckRow.scrollWidth <= deckRow.clientWidth + 1;
    };
    let placed = false;
    for (const one of [true, false]) { setCols(one); if (tryRow(true) || tryRow(false)) { placed = true; break; } }
    if (!placed) {
      deckRow.classList.remove("rack-in");
      flank(phone.matches && !face.classList.contains("fullscreen"));   // a phone's keys take them: Out has the row
      face.insertBefore(pad, deckRow);
      packRack(showBasic ? basicLine : groups, { width: pad.clientWidth });   // on its own line, with the room to plan its rows (rack.js)
    }
    if (had && document.activeElement !== had) had.focus({ preventScroll: true });   // moved, a focused control loses its focus
  }
  placeRack.width = 0;
  new ResizeObserver(() => { const w = face.clientWidth; if (w && Math.abs(w - placeRack.width) > 1) { placeRack.width = w; requestAnimationFrame(placeRack); } }).observe(face);
  // the piano's keys take no focus (their mousedown is stopped, so the editor keeps its caret);
  // a click on the instrument still has to bring the QWERTY keys here, or a-w-s-e-d type into the code
  for (const w of [keysWrap, deckRow, cardFoot]) w.addEventListener("pointerdown", (e) => { if (!e.target.closest(".dial")) pad.focus({ preventScroll: true }); });
  // and a press there (Play, Stop, the octave buttons) takes no focus of its own: Safari, which does not focus a pressed
  // button, would hand it to the page around the instrument, and the QWERTY keys with it
  for (const w of [deckRow, cardFoot, keysWrap]) w.addEventListener("mousedown", (e) => { if (!e.target.closest(".dial")) e.preventDefault(); });   // pointerdown: the piano cancels its keys' (ui/piano.js), so no mousedown follows

  const keyHandler = (e) => {
    if (e.metaKey || e.ctrlKey || e.altKey || e.target.closest("input, textarea, .cm-editor")) return;   // a dial keeps only its arrows (it stops them); every other key is the instrument's, whatever in it has focus
    const i = KEYS.indexOf(e.key.toLowerCase());
    if (i >= 0 && !e.repeat) { playKey(base + i); e.preventDefault(); }
    else if (e.key === "z") shiftOctave(-12);
    else if (e.key === "x") shiftOctave(12);
    else if (e.key === " ") { toggleDemo(); e.preventDefault(); }
  };
  return { face: card.el, keyHandler };
}

export function createDocs(root, ref, hooks) {
  const saved = store.get(PLACE_KEY);
  const place = saved && SECTIONS.some(([id]) => id === saved.section) ? saved : { section: SECTIONS[0][0], key: null };
  // "web" for what the web build never has, "runtime" for what the runtime lacks yet
  const supportOf = (key) => (ref.support?.notOnWeb.includes(key) ? "web" : ref.support?.missing.includes(key) ? "runtime" : null);
  // what the web build says of a function it does not have: never (a browser cannot), or not yet, and why
  const webNote = (key) => { const w = ref.support?.why?.[key]; return w ? `${w.kind === "never" ? "Never on the web" : "Not on the web yet"}: ${w.reason}.` : "Not in the web build."; };
  const playingJobs = new Map(); // job → reset the button
  let keyHandler = null;

  root.textContent = "";
  root.classList.add("docs");
  const head = el("div", "docs-head");
  const tabs = el("div", "seg docs-tabs");
  head.append(tabs);
  const body = el("div", "docs-body");
  // native's list column: its filter on top, then the section's pages (ui/submenu.js)
  const menu = createSubmenu({ label: "Contents", filter: "Filter…", onPick: (key) => open(place.section, key), onFilter: (n) => announce(`${n} topics`, false, Announcement.Navigation) });   // native's count as the filter is typed
  const content = el("div", "docs-content");
  body.append(menu.el, content);
  root.append(head, body);

  for (const [id, title] of SECTIONS) {
    tabs.appendChild(button("", title, () => show(id)));
  }

  // ── Snippets: the code card (ui/card.js), named for the heading above it, numbered within its section ──
  const deck = createDeck({ play: (code, opts) => hooks.play?.(code, opts) ?? hooks.run(code), stopGroup: hooks.stopGroup, group: hooks.group, scopeFrame: hooks.scopeFrame }, root);
  const counts = new Map();
  function snippet(page, code, { runnable = true } = {}) {
    let h = page.lastElementChild;
    while (h && !/^H[1-4]$/.test(h.tagName)) h = h.previousElementSibling;
    const section = h?.textContent.trim().replace(/\s+/g, " ") ?? "Example";
    const n = (counts.get(section) ?? 0) + 1; counts.set(section, n);
    return deck.add(createCard({ title: n > 1 ? `${section} · ${n}` : section, code, key: `${place.section}/${place.key}/${section}/${n}`, actions: ["edit", "reset", "copy", "insert", "drag"], hooks, playable: runnable })).el;
  }

  // ── Lists ──

  function items(section) {
    switch (section) {
      case "synths": case "fx": {
        // a program's own synths (load_synthdef, their metadata: synth-meta.js) first, under their own heading
        const all = ref[section].pages.map((p) => ({ key: p.key, title: p.user ? p.title : titleCase(p.key), user: !!p.user })).sort((a, b) => a.key.localeCompare(b.key));
        const mine = all.filter((i) => i.user);
        return mine.length ? [{ group: "Your synths" }, ...mine, { group: "Sonic Pi's" }, ...all.filter((i) => !i.user)] : all;
      }
      case "samples": return ref.samples.groups.map((g) => ({ key: g.title, title: g.title }));
      case "lang": return ref.lang.pages.map((p) => ({ key: p.key, title: p.key, absent: !!supportOf(p.key) }));
      default: return [];
    }
  }

  function renderList() {
    menu.set(items(place.section).map((it) => it.group ? { group: it.group } : { key: it.key, title: it.title, sub: !!it.depth, absent: !!it.absent, hint: it.absent ? "Not in the web runtime yet" : null }));
    menu.active(place.key);
  }

  async function show(section, key = null) {
    deck.detach(); counts.clear();   // the page's cards are about to be replaced
    place.section = section;
    [...tabs.children].forEach((b, i) => { const on = SECTIONS[i][0] === section; b.classList.toggle("active", on); b.setAttribute("aria-pressed", String(on)); });
    if (menu.search) menu.search.placeholder = `Filter ${SECTIONS.find(([id]) => id === section)?.[1] ?? ""}…`;
    const all = items(section).filter((i) => !i.group);
    const target = key && all.some((i) => i.key === key) ? key : (place.section === section && all.some((i) => i.key === place.key) ? place.key : all[0]?.key);
    await open(section, target);
  }

  // Picking a page rebuilds the list and the page, and what had focus with them: as native's tutorial pane
  // (restoreFocusAfterBuild, announcePage), focus goes back where it was — the page now lit in the list, or the new
  // page's heading when it came from a link on the page — and the page's title is said.
  async function open(section, key) {
    const had = root.contains(document.activeElement) ? document.activeElement : null;
    const fromList = !!had && menu.list.contains(had), fromPage = !!had && content.contains(had);
    // Moving to another synth or FX stops the one playing: its card is going, and a demo left sounding under a page
    // that no longer shows it can only be stopped from the bar. The same page re-rendered (a dial turned, a resize)
    // keeps playing — the deck adopts the new card by its key.
    if (place.section !== section || place.key !== key) deck.stop();
    place.section = section;
    place.key = key;
    store.set(PLACE_KEY, place);
    if (keyHandler) { root.removeEventListener("keydown", keyHandler); keyHandler = null; }
    renderList();
    content.textContent = "";
    content.scrollTop = 0;
    const page = el("article", "sp-doc");
    content.appendChild(page);
    if (section === "synths" || section === "fx") renderInstrument(page, section, key);
    else if (section === "samples") renderSamples(page, key);
    else if (section === "lang") renderLang(page, key);
    if (!had || place.key !== key) return;   // not the player's pick (a restore, a link), or overtaken by another
    const title = items(section).find((i) => i.key === key)?.title ?? key;
    if (fromList) menu.node(key)?.focus({ preventScroll: true });
    else if (fromPage) { const h = page.querySelector("h1, h2"); if (h) { h.tabIndex = -1; h.focus(); } }
    if (fromList || fromPage) announce(title, false, Announcement.Navigation);
  }

  function renderLang(page, key) {
    const p = ref.lang.pages.find((x) => x.key === key);
    if (!p) return;
    page.appendChild(el("h1", "", p.key));
    const support = supportOf(p.key);
    if (support) {
      const note = el("p", "doc-unsupported");
      if (support === "web") note.textContent = webNote(p.key);
      else note.innerHTML = 'Not in the web runtime yet. <a href="specs.html">The spec browser</a> shows what the runtime does.';
      page.appendChild(note);
    }
    page.appendChild(el("p", "doc-summary", p.summary));
    const usage = el("div", "doc-usage");
    usage.appendChild(renderCode(p.usage));
    page.appendChild(usage);
    if (p.introduced) page.appendChild(el("p", "doc-meta", `Introduced in v${String(p.introduced).replace(/^v/, "")}`));   // the data says "v4.0" already
    page.insertAdjacentHTML("beforeend", p.doc_html);
    if (p.examples?.length) {
      page.appendChild(el("h2", "", "Examples"));
      for (const ex of p.examples) page.appendChild(snippet(page, ex.code, { runnable: ex.runnable }));
    }
  }

  function renderSamples(page, key) {
    const g = ref.samples.groups.find((x) => x.title === key);
    if (!g) return;
    page.appendChild(el("h1", "", g.title));
    const grid = el("div", "sample-grid");
    for (const s of g.samples) {
      const row = el("div", "sample-row");
      const play = button("sp-mini-btn sp-mini-icon", "", () => hooks.playSample(s));
      play.innerHTML = icon("player-play");
      play.title = `sample :${s}`;
      const code = renderCode(`sample :${s}`);
      row.append(play, code, button("sp-mini-btn", "Insert", () => hooks.insert(`sample :${s}`)));
      grid.appendChild(row);
    }
    page.appendChild(grid);
  }

  // ── The instrument page, laid out as native's: the faceplate of dials, the
  // program they make, the transport and keyboard, every opt with its
  // default; then the description and each opt's own doc ──


  function renderInstrument(page, section, key) {
    const p = ref[section].pages.find((x) => x.key === key);
    if (!p) return;
    const isFx = section === "fx";
    const inst = createInstrument(p, isFx, hooks, deck, { fit: true });   // its keys fitted to the pane, as the home page's are
    page.appendChild(inst.face);
    keyHandler = inst.keyHandler;
    root.addEventListener("keydown", keyHandler);
    const doc = el("div", "pg-doc");
    doc.innerHTML = p.doc_html;
    // native's docs sometimes give an example a paragraph a line (<p><code>line</code></p> …): the run is one
    // program, so it becomes one card, as every other example on the page is
    for (const first of [...doc.children]) {
      if (first.parentNode !== doc || !isCodeLine(first)) continue;   // still in the page, not merged into a card already
      const run = [first];
      while (run.at(-1).nextElementSibling && isCodeLine(run.at(-1).nextElementSibling)) run.push(run.at(-1).nextElementSibling);
      if (run.length < 2) continue;
      const code = run.map((e) => e.textContent.replace(/^\n|\n$/g, "")).join("\n");
      const holder = el("div");
      first.before(holder);
      run.forEach((e) => e.remove());
      holder.replaceWith(snippet(doc, code, { runnable: true }));
    }

    page.append(doc);   // the dials say each opt and its default (their tooltips); the detail is below

    page.appendChild(el("div", "pg-subhead", "Opts in detail"));
    const grid = el("div", "opt-grid");
    for (const o of p.opts) {
      const card = el("div", "opt-card");
      card.id = `opt-${o.name}`;
      const range = o.options ? o.options.join(", ") : typeof o.min === "number" ? `${o.min} … ${o.max}` : "";
      card.innerHTML = `<div class="opt-name"><code>${escapeHTML(o.name)}:</code>${o.slidable ? ' <span class="opt-slidable">slidable</span>' : ""}</div>`
        + `<div class="opt-default">Default: <code>${escapeHTML(o.default ?? "none")}</code>${range ? ` · Range: <code>${escapeHTML(range)}</code>` : ""}</div>`
        + `<p>${optDocHTML(o.doc)}</p>`;
      grid.appendChild(card);
    }
    page.appendChild(grid);
  }

  /** Opens the docs for a word from the editor: a synth, an FX, a function, a sample. */
  function showFor(word, hint = null) {
    const bare = word.replace(/^:/, "");
    const has = (s) => (s === "lang" ? ref.lang.pages.some((p) => p.key === bare) : ref[s].pages?.some((p) => p.key === bare));
    const order = hint ? [hint, "synths", "fx", "lang"] : word.startsWith(":") ? ["synths", "fx", "lang"] : ["lang", "synths", "fx"];
    for (const s of order) if (has(s)) return show(s, bare), true;
    const g = ref.samples.groups.find((x) => x.samples.includes(bare));
    if (g) return show("samples", g.title), true;
    return false;
  }

  show(place.section, place.key);

  return {
    show,
    showFor,
    /** The synths have changed (a program loaded one of its own): the list says so, if it is showing them. */
    refreshList() { if (place.section === "synths") renderList(); },
    /** The session's live jobs: a sample's play button whose job has finished rests again. */
    jobs(live) {
      for (const [job, reset] of playingJobs) {
        if (!live.includes(job)) { playingJobs.delete(job); reset(); }
      }
    },
    error: (r) => deck.error(r),
    flash: (job, line) => deck.flash(job, line),
    release: (job) => deck.release(job),
    get playing() { return deck.playing; },
    groups: (live) => deck.groups(live),
    owns: (job) => deck.owns(job),
    record: (r) => deck.record(r),
    get starting() { return deck.starting; },
  };
}
