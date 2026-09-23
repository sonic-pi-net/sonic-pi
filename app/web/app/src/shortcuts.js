// SPDX-License-Identifier: AGPL-3.0-or-later
// Keyboard shortcuts as native Sonic Pi has them: its catalogue
// (./shortcut-defs.js, generated from MainWindow::shortcutDefs), its three
// keymaps and Custom (a base keymap plus the player's changes), native's
// notation resolved as MainWindow::resolveShortcut resolves it, the keys in
// force worked out as updateShortcuts works them out, and native's
// keyboard-shortcuts .ini read and written. No DOM here: main.js dispatches
// what a key press matches, shortcuts-ui.js is the editor.
import { base as baseKeys, shift as shiftKeys } from "w3c-keyname";
import { SHORTCUT_DEFS, SOURCE } from "./shortcut-defs.js";

export { SOURCE };

// The web build's own commands, in native's shape, on keys no keymap uses.
const WEB_DEFS = [
  { id: "FlightMark", desc: "Mark this moment in the flight recorder (I heard that)", group: "Web", mac: "ShiftMeta+h", win: "ShiftMeta+h", emacs: "ShiftMeta+h", secondary: { mac: [], other: [] } },
  { id: "FlightSave", desc: "Save a flight report", group: "Web", mac: "ShiftMeta+e", win: "ShiftMeta+e", emacs: "ShiftMeta+e", secondary: { mac: [], other: [] } },
];
// Secondary keys the web adds to native's: Cmd/Ctrl+Enter,
// Cmd/Ctrl+. and Cmd/Ctrl+/ for Run, Stop and Comment. Like native's, a
// secondary never takes a key another command's primary has.
const WEB_SECONDARY = { Run: ["Native+Return"], Stop: ["Native+."], Comment: ["Native+/"] };
// A Mac's buffer keys that reach a browser tab: native's Shift-Cmd-[ and ] are every browser's tab keys, and Safari
// keeps Cmd-1…9 for its own, so Control-1…0 pick a buffer and Opt-Cmd-[ and ] step through them (in the notation's
// prefixes, Ctrl+ is the Control key everywhere and Meta+ a Mac's Cmd). In Safari all of them
// reach the page. A Mac with Mission Control's "Switch to Desktop" keys on keeps Control-1…9 for those.
const WEB_SECONDARY_MAC = {
  TabPrev: ["Meta+Alt+["], TabNext: ["Meta+Alt+]"],
  ...Object.fromEntries([..."1234567890"].map((n) => [`Tab${n}`, [`Ctrl+${n}`]])),
};

export const DEFS = [...SHORTCUT_DEFS, ...WEB_DEFS].map((d) => ({
  ...d,
  secondary: {
    mac: [...d.secondary.mac, ...(WEB_SECONDARY[d.id] ?? []), ...(WEB_SECONDARY_MAC[d.id] ?? [])],
    other: [...d.secondary.other, ...(WEB_SECONDARY[d.id] ?? [])],
  },
}));
export const DEF = new Map(DEFS.map((d) => [d.id, d]));
export const GROUPS = [...new Set(DEFS.map((d) => d.group))];
export const PRESETS = [["mac", "Mac"], ["win", "Windows | Linux"], ["emacs", "Emacs Live"]];
export const MODES = [...PRESETS, ["custom", "Custom"]];
const PRESET_IDS = new Set(PRESETS.map(([id]) => id));
const MODE_IDS = new Set(MODES.map(([id]) => id));

/** "mac" where there is a Cmd key (a Mac, an iPad), otherwise "other": which keys native's Meta and Ctrl are. */
export function hostPlatform(nav = globalThis.navigator) {
  return /mac|iphone|ipad|ipod/i.test(nav?.userAgentData?.platform || nav?.platform || "") ? "mac" : "other";
}
/** Native's keymap until the player picks one: Mac on a Mac, Windows | Linux elsewhere (os_shortcut_mode). */
export const defaultMode = (platform) => (platform === "mac" ? "mac" : "win");

// ── Chords ────────────────────────────────────────────────────────────────
// A chord is a string: "Shift-", "Meta-", "Ctrl-", "Alt-" in that order, then
// the key as a KeyboardEvent names it ("Enter", "ArrowUp", "F1", " ") or a
// lower-case character. Meta is the Cmd key on a Mac. A symbol typed with
// Shift on a US keyboard is its key with Shift ("?" is Shift-/), as Qt
// matches "Meta+?" and "ShiftMeta+/" alike; so each press has one name.

const US_SHIFTED = { ")": "0", "!": "1", "@": "2", "#": "3", $: "4", "%": "5", "^": "6", "&": "7", "*": "8", "(": "9", ":": ";", "+": "=", _: "-", "<": ",", ">": ".", "?": "/", "~": "`", "{": "[", "|": "\\", "}": "]", '"': "'" };

export function chordId({ key, ctrl = false, alt = false, shift = false, meta = false }) {
  if ([...key].length === 1) {
    if (US_SHIFTED[key]) { key = US_SHIFTED[key]; shift = true; }
    else key = key.toLowerCase();
  }
  return `${shift ? "Shift-" : ""}${meta ? "Meta-" : ""}${ctrl ? "Ctrl-" : ""}${alt ? "Alt-" : ""}${key}`;
}

export function parseChord(id) {
  const parts = id.split(/-(?!$)/);
  const key = parts.pop();
  return { key, shift: parts.includes("Shift"), meta: parts.includes("Meta"), ctrl: parts.includes("Ctrl"), alt: parts.includes("Alt") };
}

// ── Native's notation ─────────────────────────────────────────────────────

// Qt's key names, lower-cased, as a KeyboardEvent names the key
const QT_KEYS = { return: "Enter", enter: "Enter", space: " ", up: "ArrowUp", down: "ArrowDown", left: "ArrowLeft", right: "ArrowRight", pgup: "PageUp", pageup: "PageUp", pgdown: "PageDown", pagedown: "PageDown", home: "Home", end: "End", backspace: "Backspace", del: "Delete", delete: "Delete", ins: "Insert", insert: "Insert", esc: "Escape", escape: "Escape", tab: "Tab" };

// resolveShortcut's prefixes, checked in its order, and the modifiers each stands for
const PREFIXES = [["shiftmeta+", "shift", "meta"], ["metashift+", "shift", "meta"], ["ctrlmeta+", "ctrl", "meta"], ["metactrl+", "ctrl", "meta"], ["ctrlshift+", "ctrl", "shift"], ["shiftctrl+", "ctrl", "shift"], ["native+", "native"], ["meta+", "meta"], ["ctrl+", "ctrl"]];

/**
 * A key in native's notation as the chord it is on this platform, or null for
 * none. As resolveShortcut: Meta is Cmd on a Mac and Alt elsewhere, Ctrl the
 * Control key everywhere, Native the platform's own (Cmd or Ctrl); what
 * follows the prefix is a QKeySequence, whose Ctrl is Cmd on a Mac and whose
 * Meta is the Mac's Control key.
 */
export function resolve(notation, platform) {
  let s = String(notation ?? "").trim().toLowerCase();
  if (!s) return null;
  const mac = platform === "mac";
  const c = { key: null, ctrl: false, alt: false, shift: false, meta: false };
  const prefix = PREFIXES.find(([p]) => s.startsWith(p));
  if (prefix) {
    s = s.slice(prefix[0].length);
    for (const m of prefix.slice(1)) {
      if (m === "shift") c.shift = true;
      else if (m === "ctrl") c.ctrl = true;
      else if (mac) c.meta = true;               // meta and native: Cmd
      else if (m === "meta") c.alt = true;
      else c.ctrl = true;
    }
  }
  let key, mods;
  if (s === "+") [key, mods] = ["+", []];
  else if (s.endsWith("++")) [key, mods] = ["+", s.slice(0, -2).split("+")];
  else { mods = s.split("+"); key = mods.pop(); }
  for (const m of mods) {
    if (m === "shift") c.shift = true;
    else if (m === "alt") c.alt = true;
    else if (m === "ctrl") mac ? (c.meta = true) : (c.ctrl = true);
    else if (m === "meta") mac ? (c.ctrl = true) : (c.meta = true);
    else return null;
  }
  const f = /^f([1-9]|1\d|2[0-4])$/.exec(key);
  c.key = QT_KEYS[key] ?? (f ? `F${f[1]}` : [...key].length === 1 ? key : null);
  return c.key ? chordId(c) : null;
}

// ── The keys in force ─────────────────────────────────────────────────────

/**
 * What each command's keys are, as native's updateShortcuts has them.
 * mode: "mac" | "win" | "emacs" | "custom"; custom: { base, keys: { id: notation } }.
 * Custom is its base keymap with the player's keys over it, and a player's key
 * unbinds a base default on the same chord. Off a Mac bare F10 (the menu bar's
 * key, which screen readers use) is never bound. Then the secondaries, each
 * unless another command's primary has its chord.
 * Returns { base, primary: Map id → chord | null, chords: Map id → [chord] (primary first), byChord: Map chord → [id] }.
 */
export function buildBindings({ mode, custom = {}, platform }) {
  const base = mode === "custom" ? (PRESET_IDS.has(custom.base) ? custom.base : defaultMode(platform)) : mode;
  const primary = new Map(DEFS.map((d) => [d.id, resolve(d[base], platform)]));
  if (mode === "custom") {
    const keys = custom.keys ?? {};
    const overridden = new Set(DEFS.filter((d) => Object.hasOwn(keys, d.id)).map((d) => d.id));
    for (const id of overridden) primary.set(id, resolve(keys[id], platform));
    const taken = new Set([...overridden].map((id) => primary.get(id)).filter(Boolean));
    for (const d of DEFS) if (!overridden.has(d.id) && taken.has(primary.get(d.id))) primary.set(d.id, null);
  }
  if (platform !== "mac" && primary.get("FocusMode") === "F10") primary.set("FocusMode", "Ctrl-F10");
  const primaries = new Set([...primary.values()].filter(Boolean));
  const chords = new Map(), byChord = new Map();
  for (const d of DEFS) {
    const own = primary.get(d.id);
    const list = own ? [own] : [];
    for (const key of d.secondary[platform === "mac" ? "mac" : "other"]) {
      const c = resolve(key, platform);
      if (c && (c === own || !primaries.has(c)) && !list.includes(c)) list.push(c);
    }
    chords.set(d.id, list);
    for (const c of list) byChord.set(c, [...(byChord.get(c) ?? []), d.id]);
  }
  return { base, primary, chords, byChord };
}

// ── Key presses ───────────────────────────────────────────────────────────

const MODIFIER_KEYS = new Set(["Shift", "Control", "Alt", "Meta", "AltGraph", "CapsLock", "Fn", "FnLock", "Hyper", "Super", "OS", "Dead", "Process", "Compose", "Unidentified"]);

/** The key an event pressed, as w3c-keyname names it (a Mac's Cmd+Shift leaves .key unshifted, so the key code says) */
export function keyName(e, platform) {
  const ignore = (platform === "mac" && e.metaKey && e.shiftKey && !e.ctrlKey && !e.altKey) || e.key === "Unidentified";
  const name = (!ignore && e.key) || (e.shiftKey ? shiftKeys : baseKeys)[e.keyCode] || e.key || "Unidentified";
  return { Esc: "Escape", Del: "Delete", Left: "ArrowLeft", Up: "ArrowUp", Right: "ArrowRight", Down: "ArrowDown", Spacebar: " " }[name] ?? name;
}

/**
 * The chords a key press could be, the likeliest first, as CodeMirror's
 * runHandlers tries them: the key as typed; for a symbol, first without the
 * Shift that typed it; then, with Ctrl, Cmd or Alt held, the key the key code
 * says (a Mac's Option types ®, not r), except Ctrl+Alt off a Mac, which is AltGr.
 */
export function eventChords(e, platform) {
  const name = keyName(e, platform);
  if (MODIFIER_KEYS.has(name)) return [];
  const mods = { ctrl: !!e.ctrlKey, alt: !!e.altKey, meta: !!e.metaKey };
  const out = [];
  const add = (key, shift) => {
    const id = chordId({ ...mods, key, shift });
    if (!out.includes(id)) out.push(id);
  };
  const isChar = [...name].length === 1 && name !== " ";
  if (!isChar) add(name, !!e.shiftKey);
  else if (name.toLowerCase() !== name.toUpperCase()) add(name, !!e.shiftKey);
  else {
    add(name, false);
    if (e.shiftKey) add(name, true);
  }
  const b = baseKeys[e.keyCode];
  if (isChar && (e.ctrlKey || e.metaKey || e.altKey) && b?.length === 1 && !(platform !== "mac" && e.ctrlKey && e.altKey)) add(b, !!e.shiftKey);
  return out;
}

/** The command ids a key press is bound to, and the chord it matched them on, or null. */
export function matchEvent(e, bindings, platform) {
  for (const chord of eventChords(e, platform)) {
    const ids = bindings.byChord.get(chord);
    if (ids) return { chord, ids };
  }
  return null;
}

const QT_NAMES = { Enter: "Return", " ": "Space", ArrowUp: "Up", ArrowDown: "Down", ArrowLeft: "Left", ArrowRight: "Right", PageUp: "PgUp", PageDown: "PgDown", Delete: "Del", Insert: "Ins", Escape: "Esc", Backspace: "Backspace", Tab: "Tab", Home: "Home", End: "End" };

/**
 * A pressed chord in native's notation, as native's shortcut recorder writes it
 * (chordToSonicPiNotation), or null for a modifier alone. On a Mac Cmd is
 * Meta and Control is Ctrl; elsewhere Alt is Meta and the Windows key is left out.
 */
export function notationFromEvent(e, platform) {
  let name = keyName(e, platform);
  if (MODIFIER_KEYS.has(name)) return null;
  let k;
  if (QT_NAMES[name]) k = e.code === "NumpadEnter" ? "Enter" : QT_NAMES[name];
  else if (/^F\d{1,2}$/.test(name)) k = name;
  else if ([...name].length === 1) {
    if ((e.altKey || e.ctrlKey || e.metaKey) && !/^[\x21-\x7e]$/.test(name) && baseKeys[e.keyCode]?.length === 1) name = baseKeys[e.keyCode];
    k = /^[a-z]$/.test(name) ? name.toUpperCase() : name;
  } else return null;
  const mac = platform === "mac";
  const meta = mac ? e.metaKey : e.altKey, ctrl = e.ctrlKey, shift = e.shiftKey;
  let prefix = "", shiftUsed = false;
  if (meta && ctrl) prefix = "CtrlMeta";
  else if (meta && shift) [prefix, shiftUsed] = ["ShiftMeta", true];
  else if (meta) prefix = "Meta";
  else if (ctrl && shift) [prefix, shiftUsed] = ["CtrlShift", true];
  else if (ctrl) prefix = "Ctrl";
  const rest = [...(mac && e.altKey ? ["Alt"] : []), ...(shift && !shiftUsed ? ["Shift"] : []), k].join("+");
  return prefix ? `${prefix}+${rest}` : rest;
}

// ── Showing a chord ───────────────────────────────────────────────────────

const MAC_KEYS = { Enter: "↩", Backspace: "⌫", Delete: "⌦", Escape: "⎋", Tab: "⇥", " ": "Space", ArrowUp: "↑", ArrowDown: "↓", ArrowLeft: "←", ArrowRight: "→", PageUp: "⇞", PageDown: "⇟", Home: "↖", End: "↘" };
const OTHER_KEYS = { " ": "Space", ArrowUp: "Up", ArrowDown: "Down", ArrowLeft: "Left", ArrowRight: "Right", PageUp: "PgUp", PageDown: "PgDown", Delete: "Del", Insert: "Ins", Escape: "Esc" };

/** A chord as the platform's menus write one: ⌃⌥⇧⌘K on a Mac, Ctrl+Alt+Shift+K elsewhere. */
export function formatChord(id, platform) {
  if (!id) return "";
  const c = parseChord(id);
  const key = (names) => names[c.key] ?? (c.key.length === 1 ? c.key.toUpperCase() : c.key);
  if (platform === "mac") return `${c.ctrl ? "⌃" : ""}${c.alt ? "⌥" : ""}${c.shift ? "⇧" : ""}${c.meta ? "⌘" : ""}${key(MAC_KEYS)}`;
  return [c.meta && "Meta", c.ctrl && "Ctrl", c.alt && "Alt", c.shift && "Shift", key(OTHER_KEYS)].filter(Boolean).join("+");
}

/** Words a search for a chord might use: modifier and key names as people say them. */
export function chordWords(id, platform) {
  if (!id) return "";
  const c = parseChord(id);
  const mac = platform === "mac";
  return [
    c.meta && (mac ? "cmd command" : "meta win super"),
    c.ctrl && "ctrl control",
    c.alt && (mac ? "opt option alt" : "alt"),
    c.shift && "shift",
    { Enter: "enter return", " ": "space", Escape: "esc escape", Delete: "del delete", Backspace: "backspace", PageUp: "pgup pageup", PageDown: "pgdown pagedown" }[c.key] ?? c.key.replace(/^Arrow/, "").toLowerCase(),
    formatChord(id, platform).toLowerCase(),
  ].filter(Boolean).join(" ");
}

// ── Keys a browser keeps ──────────────────────────────────────────────────
// A tab never sees these: the browser's own tab and window keys (Chromium
// reserves them, Firefox and Safari keep them too) and the system's. An
// installed app, which has no tabs, gets them.
const RESERVED = {
  mac: ["Meta+w", "ShiftMeta+w", "Meta+t", "ShiftMeta+t", "Meta+n", "ShiftMeta+n", "Meta+q", "ShiftMeta+[", "ShiftMeta+]", "Meta+Alt+Left", "Meta+Alt+Right", "Ctrl+Tab", "CtrlShift+Tab", "Ctrl+PgUp", "Ctrl+PgDown", "Meta+Tab", "Meta+`", "Meta+Space"],
  other: ["Ctrl+w", "CtrlShift+w", "Ctrl+t", "CtrlShift+t", "Ctrl+n", "CtrlShift+n", "CtrlShift+q", "Ctrl+Tab", "CtrlShift+Tab", "Ctrl+PgUp", "Ctrl+PgDown", "Ctrl+F4", "Alt+F4", "Alt+Tab"],
};
// Safari keeps more than Chromium lets a page have: these never reach the page: Cmd-L its address bar, Cmd-R and
// Opt-Cmd-R reload, Cmd-1…9 its tabs, Cmd-M minimise, Cmd-, its settings, Cmd-H hide,
// Opt-Cmd-L its downloads. Shift-Cmd-L, Shift-Cmd-B, Cmd-D, Shift-Cmd-D and Cmd-Y do reach the page, whatever
// Safari's menus say.
const SAFARI_RESERVED = ["Meta+l", "Meta+r", "Meta+Alt+r", "Meta+Alt+l", "Meta+m", "Meta+,", "Meta+h", ...Array.from({ length: 9 }, (_, i) => `Meta+${i + 1}`)];
const isSafari = (nav = globalThis.navigator) => !!nav && /safari/i.test(nav.userAgent ?? "") && !/chrome|chromium|crios|fxios|edg\//i.test(nav.userAgent ?? "");
const reservedSets = {};
export function isReserved(chord, platform, safari = isSafari()) {
  const p = platform === "mac" ? "mac" : "other";
  const key = `${p}${safari ? "-safari" : ""}`;
  reservedSets[key] ??= new Set([...RESERVED[p], ...(safari && p === "mac" ? SAFARI_RESERVED : [])].map((k) => resolve(k, platform)));
  return !!chord && reservedSets[key].has(chord);
}

// ── Native's keyboard-shortcuts .ini ──────────────────────────────────────
// QSettings' IniFormat: [General], base=<keymap>, then <id>=<notation> for
// each key changed from the base. A value with , ; or = is quoted, \ and "
// escaped. Ids this catalogue does not have are left out, as native leaves them.

function unescapeIni(raw) {
  let s = raw.trim(), out = "", quoted = false;
  if (s.startsWith("@@")) s = s.slice(1);
  const ESC = { a: "\x07", b: "\b", f: "\f", n: "\n", r: "\r", t: "\t", v: "\v" };
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (ch === '"') quoted = !quoted;
    else if (ch === "\\" && i + 1 < s.length) {
      const next = s[++i];
      if (next === "x") {
        const hex = /^[0-9a-fA-F]+/.exec(s.slice(i + 1))?.[0] ?? "";
        out += hex ? String.fromCodePoint(parseInt(hex, 16)) : "x";
        i += hex.length;
      } else out += ESC[next] ?? next;
    } else out += ch;
  }
  return out;
}

function escapeIni(value) {
  const v = String(value);
  let out = v.startsWith("@") ? "@" : "";
  for (const ch of v) out += ch === "\\" ? "\\\\" : ch === '"' ? '\\"' : ch === "\n" ? "\\n" : ch === "\t" ? "\\t" : ch;
  return /[;,=]/.test(v) || /^\s|\s$/.test(v) ? `"${out}"` : out;
}

/** { base, keys } from a .ini's text: base null when it names no keymap. */
export function parseIni(text) {
  const ids = new Map(DEFS.map((d) => [d.id.toLowerCase(), d.id]));
  let base = null, section = "general";
  const keys = {};
  for (const raw of String(text).split(/\r?\n/)) {
    const line = raw.trim();
    if (!line || line.startsWith(";") || line.startsWith("#")) continue;
    const head = /^\[(.*)\]$/.exec(line);
    if (head) { section = head[1].trim().toLowerCase(); continue; }
    const eq = line.indexOf("=");
    if (eq < 0 || section !== "general") continue;
    const name = line.slice(0, eq).trim().toLowerCase(), value = unescapeIni(line.slice(eq + 1));
    if (name === "base") base = value.trim().toLowerCase();
    else if (ids.has(name)) keys[ids.get(name)] = value;
  }
  return { base: PRESET_IDS.has(base) ? base : null, keys };
}

/** A .ini native reads: the base keymap, then the changed keys in the catalogue's order. */
export function serializeIni({ base, keys }) {
  const lines = ["[General]", `base=${escapeIni(base)}`];
  for (const d of DEFS) if (Object.hasOwn(keys, d.id)) lines.push(`${d.id}=${escapeIni(keys[d.id])}`);
  return `${lines.join("\n")}\n`;
}

// ── The player's shortcuts ────────────────────────────────────────────────

const MODE_KEY = "sp-shortcut-mode";
const CUSTOM_KEY = "sp-shortcuts-custom";

/**
 * The shortcuts in force, remembered in the browser: the keymap (native's
 * prefs/shortcut-mode) and Custom's base and keys (native's .ini), which are
 * kept while another keymap is chosen, as native keeps its file.
 * store: { get(key, default), set(key, value) }.
 */
export function createShortcuts({ store = null, platform = hostPlatform() } = {}) {
  const installed = () => globalThis.navigator?.standalone === true || !!globalThis.matchMedia?.("(display-mode: standalone)").matches;
  const sanitize = (c) => {
    const keys = {};
    for (const [id, v] of Object.entries(c?.keys ?? {})) if (DEF.has(id) && typeof v === "string") keys[id] = v;
    return { base: PRESET_IDS.has(c?.base) ? c.base : defaultMode(platform), keys };
  };
  let mode = store?.get(MODE_KEY, null);
  if (!MODE_IDS.has(mode)) mode = defaultMode(platform);
  let custom = sanitize(store?.get(CUSTOM_KEY, null));
  let bindings = buildBindings({ mode, custom, platform });
  const listeners = new Set();
  const changed = () => {
    bindings = buildBindings({ mode, custom, platform });
    for (const fn of listeners) fn();
  };
  const label = (id) => formatChord(bindings.primary.get(id) ?? bindings.chords.get(id)?.[0], platform);
  return {
    platform,
    get mode() { return mode; },
    get custom() { return { base: custom.base, keys: { ...custom.keys } }; },
    get bindings() { return bindings; },
    setMode(m) {
      if (!MODE_IDS.has(m) || m === mode) return;
      mode = m;
      store?.set(MODE_KEY, m);
      changed();
    },
    setCustom(c) {
      custom = sanitize(c);
      store?.set(CUSTOM_KEY, custom);
      changed();
    },
    label,
    /** text with the command's key after it, as native's tooltips have it — not a key the browser keeps for itself
     * (Safari's Cmd-M minimises), which would never reach the command; installed as an app, the page has them all */
    title: (text, id) => {
      const chord = bindings.primary.get(id) ?? bindings.chords.get(id)?.[0];
      const kept = !installed() && isReserved(chord, platform);
      return label(id) && !kept ? `${text} (${label(id)})` : text;
    },
    match: (e) => matchEvent(e, bindings, platform),
    resolve: (notation) => resolve(notation, platform),
    format: (chord) => formatChord(chord, platform),
    words: (chord) => chordWords(chord, platform),
    notation: (e) => notationFromEvent(e, platform),
    reserved: (chord) => isReserved(chord, platform),
    onChange: (fn) => (listeners.add(fn), () => listeners.delete(fn)),
  };
}
