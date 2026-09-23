// SPDX-License-Identifier: AGPL-3.0-or-later
// The keyboard shortcuts are native Sonic Pi's (MainWindow::shortcutDefs,
// resolveShortcut, updateShortcuts, chordToSonicPiNotation, its .ini), so
// these check the web's reading of them against what native does.
import { test } from "node:test";
import assert from "node:assert/strict";
import { SHORTCUT_DEFS } from "../src/shortcut-defs.js";
import {
  DEFS, DEF, GROUPS, PRESETS, resolve, buildBindings, eventChords, matchEvent, notationFromEvent,
  formatChord, isReserved, parseIni, serializeIni, createShortcuts,
} from "../src/shortcuts.js";

const key = (key, keyCode, mods = {}) => ({ key, keyCode, ctrlKey: false, altKey: false, shiftKey: false, metaKey: false, ...mods });

test("the catalogue is native's, with the web's own commands after it", () => {
  assert.equal(SHORTCUT_DEFS.length, 93);
  assert.equal(DEFS.length, SHORTCUT_DEFS.length + 2);
  assert.equal(GROUPS[0], "Live");
  assert.deepEqual(DEF.get("Help").secondary.mac, ["F1", "Meta+i"]);
  assert.deepEqual(DEF.get("Help").secondary.other, ["F1"]);
  assert.deepEqual(DEF.get("Run").secondary.mac, ["Meta+R", "Native+Return"]);
});

test("every key in every keymap resolves, on a Mac and elsewhere", () => {
  for (const platform of ["mac", "other"]) {
    for (const d of DEFS) {
      for (const [preset] of PRESETS) {
        if (d[preset]) assert.notEqual(resolve(d[preset], platform), null, `${d.id} ${preset} ${d[preset]} on ${platform}`);
      }
      for (const s of d.secondary[platform]) assert.notEqual(resolve(s, platform), null, `${d.id} secondary ${s}`);
    }
  }
});

test("native's notation: Meta is Cmd on a Mac and Alt elsewhere, Ctrl the Control key, Native the platform's own", () => {
  const cases = [
    ["Meta+Return", "Meta-Enter", "Alt-Enter"],
    ["Ctrl+O", "Ctrl-o", "Ctrl-o"],
    ["CtrlMeta+i", "Meta-Ctrl-i", "Ctrl-Alt-i"],
    ["CtrlShift+e", "Shift-Ctrl-e", "Shift-Ctrl-e"],
    ["ShiftMeta+[", "Shift-Meta-[", "Shift-Alt-["],
    ["MetaShift+,", "Shift-Meta-,", "Shift-Alt-,"],
    ["Native+c", "Meta-c", "Ctrl-c"],
    ["Native+Shift+z", "Shift-Meta-z", "Shift-Ctrl-z"],
    ["ShiftCtrl+z", "Shift-Ctrl-z", "Shift-Ctrl-z"],
    ["Alt+Shift+Backspace", "Shift-Alt-Backspace", "Shift-Alt-Backspace"],
    // "Ctrl+" is native's prefix, the Control key, whatever follows it
    ["Ctrl+Alt+x", "Ctrl-Alt-x", "Ctrl-Alt-x"],
    // a bare QKeySequence: Qt's Ctrl is Cmd on a Mac, its Meta the Control key
    ["Alt+Ctrl+x", "Meta-Alt-x", "Ctrl-Alt-x"],
    ["Alt+Meta+x", "Ctrl-Alt-x", "Meta-Alt-x"],
    ["Shift+F1", "Shift-F1", "Shift-F1"],
    ["PgUp", "PageUp", "PageUp"],
    ["CtrlShift+Space", "Shift-Ctrl- ", "Shift-Ctrl- "],
  ];
  for (const [notation, mac, other] of cases) {
    assert.equal(resolve(notation, "mac"), mac, `${notation} on a Mac`);
    assert.equal(resolve(notation, "other"), other, `${notation} elsewhere`);
  }
});

test("a shifted symbol is its key with Shift, however native wrote it", () => {
  assert.equal(resolve("Meta+?", "mac"), "Shift-Meta-/");
  assert.equal(resolve("ShiftMeta+/", "mac"), "Shift-Meta-/");
  assert.equal(resolve("Ctrl++", "other"), "Shift-Ctrl-=");
  assert.equal(resolve("Ctrl+_", "other"), "Shift-Ctrl--");
  assert.notEqual(resolve("Ctrl+_", "other"), resolve("Ctrl+-", "other"));   // native's TextZoomOut and LogZoomOut
});

test("no key, or one native cannot read, is no chord", () => {
  assert.equal(resolve("", "mac"), null);
  assert.equal(resolve("Bogus+x", "mac"), null);
});

test("a keymap's keys: the primary, then secondaries native keeps", () => {
  const mac = buildBindings({ mode: "mac", platform: "mac" });
  assert.deepEqual(mac.chords.get("Run"), ["Meta-Enter", "Meta-r"]);
  assert.deepEqual(mac.chords.get("Help"), ["Shift-Meta-/", "F1", "Meta-i"]);
  const emacs = buildBindings({ mode: "emacs", platform: "other" });
  assert.deepEqual(emacs.chords.get("Help"), ["Alt-i", "F1"]);
  assert.deepEqual(emacs.chords.get("Run"), ["Alt-Enter", "Alt-r", "Ctrl-Enter"]);
  const win = buildBindings({ mode: "win", platform: "other" });
  assert.equal(win.primary.get("Right"), null);
  assert.equal(win.primary.get("Find"), "Ctrl-f");
  assert.deepEqual(win.chords.get("ContextualDocs"), ["Shift-F1"]);
});

test("off a Mac bare F10 is never bound", () => {
  assert.equal(buildBindings({ mode: "mac", platform: "other" }).primary.get("FocusMode"), "Ctrl-F10");
  assert.equal(buildBindings({ mode: "mac", platform: "mac" }).primary.get("FocusMode"), "F10");
});

test("Custom: the player's key unbinds a base default on the same chord, and takes it from secondaries", () => {
  const b = buildBindings({ mode: "custom", custom: { base: "mac", keys: { Scope: "Meta+Return" } }, platform: "mac" });
  assert.equal(b.primary.get("Run"), null);
  assert.deepEqual(b.chords.get("Run"), ["Meta-r"]);
  assert.deepEqual(b.byChord.get("Meta-Enter"), ["Scope"]);
  const s = buildBindings({ mode: "custom", custom: { base: "mac", keys: { Scope: "Meta+R" } }, platform: "mac" });
  assert.deepEqual(s.chords.get("Run"), ["Meta-Enter"]);
  assert.deepEqual(s.byChord.get("Meta-r"), ["Scope"]);
});

test("Custom: two of the player's keys on one chord are both kept, and ambiguous", () => {
  const b = buildBindings({ mode: "custom", custom: { base: "mac", keys: { Run: "Meta+k", Stop: "Meta+k" } }, platform: "mac" });
  assert.deepEqual(b.byChord.get("Meta-k"), ["Run", "Stop"]);
});

test("Custom with a base it does not know is the platform's keymap", () => {
  assert.equal(buildBindings({ mode: "custom", custom: { base: "vim" }, platform: "other" }).base, "win");
});

test("key presses match as native's keys do", () => {
  const mac = buildBindings({ mode: "mac", platform: "mac" });
  const on = (e, b, platform = "mac") => matchEvent(e, b, platform)?.ids;
  assert.deepEqual(on(key("Enter", 13, { metaKey: true }), mac), ["Run"]);
  // Chrome on a Mac leaves Cmd+Shift+[ unshifted in .key
  assert.deepEqual(on(key("[", 219, { metaKey: true, shiftKey: true }), mac), ["TabPrev"]);
  assert.deepEqual(on(key("/", 191, { metaKey: true, shiftKey: true }), mac), ["Help"]);
  assert.deepEqual(on(key("?", 191, { metaKey: true, shiftKey: true }), mac), ["Help"]);
  assert.deepEqual(on(key("ArrowUp", 38, { altKey: true }), mac), ["ShiftUp"]);
  assert.deepEqual(on(key("z", 90, { metaKey: true, shiftKey: true }), mac), ["Redo"]);
  const win = buildBindings({ mode: "win", platform: "other" });
  assert.deepEqual(on(key("+", 187, { ctrlKey: true, shiftKey: true }), win, "other"), ["TextZoomIn"]);
  assert.deepEqual(on(key("=", 187, { ctrlKey: true }), win, "other"), ["LogZoomIn"]);
  const emacs = buildBindings({ mode: "emacs", platform: "other" });
  assert.deepEqual(on(key("!", 49, { altKey: true, shiftKey: true }), emacs, "other"), ["Tab1"]);
  assert.deepEqual(on(key("p", 80, { ctrlKey: true, altKey: true }), emacs, "other"), ["ShiftUp"]);
  // AltGr (Ctrl+Alt) typing ń on a Polish layout is not Ctrl+Alt+N
  assert.equal(on(key("ń", 78, { ctrlKey: true, altKey: true }), emacs, "other"), undefined);
});

test("a Mac's Option types a symbol, but a key bound on Option still matches", () => {
  const b = buildBindings({ mode: "custom", custom: { base: "mac", keys: { Scope: "Alt+R" } }, platform: "mac" });
  assert.deepEqual(eventChords(key("®", 82, { altKey: true }), "mac"), ["Alt-®", "Alt-r"]);
  assert.deepEqual(matchEvent(key("®", 82, { altKey: true }), b, "mac").ids, ["Scope"]);
});

test("a modifier alone is no chord", () => {
  assert.deepEqual(eventChords(key("Shift", 16, { shiftKey: true }), "mac"), []);
  assert.equal(notationFromEvent(key("Meta", 91, { metaKey: true }), "mac"), null);
});

test("a recorded chord is written as native's recorder writes it, and resolves to the chord pressed", () => {
  const cases = [
    ["mac", key("r", 82, { metaKey: true, shiftKey: true }), "ShiftMeta+R"],
    ["mac", key("i", 73, { metaKey: true, ctrlKey: true }), "CtrlMeta+I"],
    ["mac", key("ArrowLeft", 37, { altKey: true, shiftKey: true }), "Alt+Shift+Left"],
    ["mac", key("e", 69, { ctrlKey: true, shiftKey: true }), "CtrlShift+E"],
    ["other", key("Enter", 13, { altKey: true }), "Meta+Return"],
    ["other", key("+", 187, { ctrlKey: true, shiftKey: true }), "CtrlShift++"],
    ["other", key("!", 49, { altKey: true, shiftKey: true }), "ShiftMeta+!"],
    ["other", key("F1", 112, { shiftKey: true }), "Shift+F1"],
    ["other", key(" ", 32, { ctrlKey: true }), "Ctrl+Space"],
  ];
  for (const [platform, e, notation] of cases) {
    assert.equal(notationFromEvent(e, platform), notation);
    assert.equal(resolve(notation, platform), eventChords(e, platform)[0], notation);
  }
  assert.equal(resolve("ShiftMeta+!", "other"), resolve("ShiftMeta+1", "other"));
  assert.equal(notationFromEvent(key("®", 82, { altKey: true }), "mac"), "Alt+R");
});

test("chords show as the platform's menus show them", () => {
  assert.equal(formatChord("Shift-Meta-/", "mac"), "⇧⌘/");
  assert.equal(formatChord("Meta-Ctrl-i", "mac"), "⌃⌘I");
  assert.equal(formatChord("Meta-Enter", "mac"), "⌘↩");
  assert.equal(formatChord("Shift-Ctrl-=", "other"), "Ctrl+Shift+=");
  assert.equal(formatChord("Alt-Enter", "other"), "Alt+Enter");
  assert.equal(formatChord("Ctrl- ", "other"), "Ctrl+Space");
  assert.equal(formatChord(null, "mac"), "");
});

test("the browser's own tab and window keys are known", () => {
  assert.ok(isReserved("Meta-t", "mac"));
  assert.ok(isReserved("Shift-Meta-[", "mac"));
  assert.ok(isReserved("Ctrl-w", "other"));
  assert.ok(!isReserved("Meta-r", "mac"));
  assert.ok(!isReserved("Ctrl-t", "mac"));
});

test("a .ini native wrote reads back as its base and the keys this catalogue has", () => {
  const ini = '[General]\nbase=Emacs\nRun="Meta+,"\nstop=Ctrl+k\nNotACommand=Meta+x\n\n[Other]\nScope=Meta+q\n';
  assert.deepEqual(parseIni(ini), { base: "emacs", keys: { Run: "Meta+,", Stop: "Ctrl+k" } });
  assert.equal(parseIni("[General]\nbase=vim\n").base, null);
});

test("a .ini written here round-trips quotes, commas, backslashes and unbound keys", () => {
  const custom = { base: "win", keys: { Run: "Meta+,", Stop: "", Find: 'Ctrl+"', Comment: "Meta+\\" } };
  const text = serializeIni(custom);
  assert.match(text, /^\[General\]\nbase=win\n/);
  assert.match(text, /\nRun="Meta\+,"\n/);
  assert.deepEqual(parseIni(text), custom);
});

test("the keymap and Custom's keys are remembered, and ids native lacks dropped", () => {
  const saved = new Map();
  const store = { get: (k, d) => (saved.has(k) ? saved.get(k) : d), set: (k, v) => saved.set(k, v) };
  const keys = createShortcuts({ store, platform: "other" });
  assert.equal(keys.mode, "win");
  let changes = 0;
  keys.onChange(() => changes++);
  keys.setCustom({ base: "emacs", keys: { Run: "Ctrl+R", Nope: "Meta+x" } });
  keys.setMode("custom");
  assert.equal(changes, 2);
  const again = createShortcuts({ store, platform: "other" });
  assert.equal(again.mode, "custom");
  assert.deepEqual(again.custom, { base: "emacs", keys: { Run: "Ctrl+R" } });
  assert.equal(again.label("Run"), "Ctrl+R");
  assert.equal(again.title("Run", "Run"), "Run (Ctrl+R)");
  // emacs's own Ctrl+r (FindPrev) gives way to the player's Run
  assert.equal(again.bindings.primary.get("FindPrev"), null);
});

test("a Mac's buffers have keys a browser tab gets: Control-1…0, and Opt-Cmd-[ and ] to step", () => {
  for (const mode of ["mac", "win", "emacs"]) {
    const b = buildBindings({ mode, platform: "mac" });
    for (const n of "1234567890") {
      assert.ok(b.chords.get(`Tab${n}`).includes(`Ctrl-${n}`), `${mode}: Tab${n}`);
      assert.ok(!isReserved(`Ctrl-${n}`, "mac", true));
    }
    assert.ok(b.chords.get("TabPrev").includes("Meta-Alt-["), mode);
    assert.ok(b.chords.get("TabNext").includes("Meta-Alt-]"), mode);
    assert.ok(isReserved(b.primary.get("TabNext"), "mac", true), "native's own is the browser's");
  }
  // elsewhere native's Shift-Alt keys reach the page, and nothing is added
  const other = buildBindings({ mode: "win", platform: "other" });
  assert.deepEqual(other.chords.get("TabNext"), ["Shift-Alt-]"]);
  assert.deepEqual(other.chords.get("Tab1"), ["Shift-Alt-1"]);
});
