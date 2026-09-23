// SPDX-License-Identifier: AGPL-3.0-or-later
// The shortcut editor, as native's Preferences → Keyboard Shortcuts has it
// (SettingsWidget::createKeyboardShortcutsTab): the keymap as a segmented
// control, the catalogue in native's menu groups, the presets read only,
// Custom on a base keymap with a key recorder, a clash offering Reassign to
// this, Keep both or Cancel, clashes red, changed keys marked with what they
// were, Reset, and Import and Export of native's .ini. Beyond native's: search
// by name or by pressing the key, only the changed keys or the clashes, a key
// back to its default from its row, the keys a browser keeps for itself
// flagged, a key taken by another shown as not in force, and a preset's key
// customised from its row. A sheet over the page, as the Info card is.
import { DEFS, DEF, GROUPS, PRESETS, MODES, parseIni, serializeIni } from "./shortcuts.js";
import { icon } from "./icons.js";

const el = (tag, cls, text) => {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text != null) e.textContent = text;
  return e;
};
const button = (cls, text, title) => {
  const b = el("button", cls, text);
  b.type = "button";
  if (title) { b.title = title; b.setAttribute("aria-label", title); }
  return b;
};

/**
 * keys: createShortcuts(); unavailable: { id: why } for commands the web
 * build lacks; standalone: installed as an app, where the browser keeps no keys.
 */
export function createShortcutEditor({ keys, unavailable = {}, standalone = false, toast = () => {} }) {
  const state = { query: "", byKey: null, listening: false, show: "all", recording: null, clash: null, hint: null, confirmReset: false };
  const fmt = (chord) => keys.format(chord);
  const name = (id) => DEF.get(id).desc;
  const presetName = (id) => PRESETS.find(([p]) => p === id)?.[1] ?? id;
  let lastFocus = null;

  // ── The sheet ──
  const overlay = el("div", "sc-overlay");
  overlay.hidden = true;
  const dialog = el("div", "sc-dialog");
  dialog.setAttribute("role", "dialog");
  dialog.setAttribute("aria-modal", "true");
  dialog.setAttribute("aria-labelledby", "sc-title");
  overlay.appendChild(dialog);

  // The keymap beside the title, then one row of tools, then a line of hint:
  // the list starts as near the top as it can, and the rest is in the rows.
  const head = el("div", "sc-head");
  const title = el("h3", "sc-title", "Keyboard shortcuts");
  title.id = "sc-title";
  const modeSeg = el("div", "seg sc-modes");
  modeSeg.setAttribute("role", "radiogroup");
  modeSeg.setAttribute("aria-label", "Keymap");
  for (const [id, label] of MODES) {
    const b = button("", label);
    b.dataset.mode = id;
    b.setAttribute("role", "radio");
    b.addEventListener("click", () => { state.hint = null; state.confirmReset = false; keys.setMode(id); });
    modeSeg.appendChild(b);
  }
  const closeBtn = button("round-btn square sc-close", "", "Close (Esc)");
  closeBtn.innerHTML = icon("x");
  head.append(title, modeSeg, el("span", "sc-spacer"), closeBtn);

  const tools = el("div", "sc-bar sc-tools");
  const search = el("input", "sc-search");
  Object.assign(search, { type: "search", placeholder: "Search", spellcheck: false });
  search.setAttribute("aria-label", "Search commands and keys");
  const byKeyBtn = button("sc-bykey", "", "Find what a key does: press it");
  byKeyBtn.innerHTML = '<svg viewBox="0 0 24 24" aria-hidden="true"><rect x="2" y="6" width="20" height="12" rx="2"/><path d="M6 10h.01M10 10h.01M14 10h.01M18 10h.01M6 14h.01M18 14h.01M10 14h4"/></svg>';
  const byKeyChip = button("sc-chip", "");
  const showSeg = el("div", "seg sc-show");
  for (const [id, label] of [["all", "All"], ["changed", "Changed"], ["clashes", "Clashes"]]) {
    const b = button("", label);
    b.dataset.show = id;
    b.addEventListener("click", () => { state.show = id; render(); });
    showSeg.appendChild(b);
  }
  tools.append(search, byKeyBtn, showSeg, byKeyChip);

  const customBar = el("div", "sc-bar sc-custom");
  const baseSelect = el("select", "sc-select");
  baseSelect.setAttribute("aria-label", "Custom's base keymap");
  for (const [id, label] of PRESETS) baseSelect.appendChild(Object.assign(el("option", "", label), { value: id }));
  const changedLabel = el("span", "sc-changed");
  const resetBtn = button("sc-quiet", "Reset…");
  const resetConfirm = el("span", "sc-confirm");
  customBar.append(el("span", "sc-label", "Base"), baseSelect, changedLabel, el("span", "sc-spacer"), resetBtn, resetConfirm);

  const hint = el("p", "sc-hint");
  const list = el("div", "sc-list");
  list.setAttribute("role", "list");

  const foot = el("div", "sc-foot");
  const importBtn = button("sp-mini-btn", "Import…", "Read native Sonic Pi's v5-keyboard-shortcuts.ini");
  const exportBtn = button("sp-mini-btn", "Export…", "Write native Sonic Pi's v5-keyboard-shortcuts.ini");
  const file = Object.assign(el("input"), { type: "file", accept: ".ini,text/plain", hidden: true });
  // the way out, where a sheet's is looked for; the × and Escape close it too
  const doneBtn = button("sp-mini-btn primary sc-done", "Done", "Close (Esc)");
  doneBtn.addEventListener("click", close);
  foot.append(el("span", "sc-foot-note", "native Sonic Pi's own .ini"), importBtn, exportBtn, el("span", "sc-spacer"), doneBtn, file);

  const live = el("div", "sr-only");
  live.setAttribute("aria-live", "polite");
  dialog.append(head, tools, customBar, hint, list, foot, live);
  document.body.appendChild(overlay);

  const say = (text) => {
    live.textContent = "";
    requestAnimationFrame(() => { live.textContent = text; });
  };
  const keyButton = (id) => list.querySelector(`[data-id="${id}"] .sc-key`);
  const focusIn = (id, role = "key") => list.querySelector(`[data-id="${id}"] [data-role="${role}"]`)?.focus();

  // ── Custom's keys ──

  // Changes to Custom: a key the same as its base's is no change, as native's diff has it
  function setCustomKeys(changes) {
    const c = keys.custom;
    for (const [id, notation] of Object.entries(changes)) {
      if (keys.resolve(notation) === keys.resolve(DEF.get(id)[c.base])) delete c.keys[id];
      else c.keys[id] = notation;
    }
    keys.setCustom(c);
  }

  // A new base keeps the player's changes over it, as native's base combo does
  // (the diffs against the old base, laid over the new); one the new base has anyway is no change
  function changeBase(base) {
    const c = keys.custom;
    const next = {};
    for (const [id, value] of Object.entries(c.keys)) {
      if (keys.resolve(value) !== keys.resolve(DEF.get(id)[base])) next[id] = value;
    }
    keys.setCustom({ base, keys: next });
  }

  // ── What each row shows ──

  function model() {
    const b = keys.bindings, c = keys.custom, custom = keys.mode === "custom";
    return DEFS.map((d) => {
      const own = custom && Object.hasOwn(c.keys, d.id);
      const configured = keys.resolve(own ? c.keys[d.id] : d[b.base]);
      const chord = b.primary.get(d.id);
      return {
        d, own, chord, configured,
        base: keys.resolve(d[b.base]),
        clash: chord ? (b.byChord.get(chord) ?? []).filter((id) => id !== d.id) : [],
        takenBy: configured && !chord ? (b.byChord.get(configured) ?? []) : [],
        moved: !!(configured && chord && chord !== configured),
        secondaries: b.chords.get(d.id).filter((x) => x !== chord),
        reserved: !standalone && keys.reserved(chord),
        unavailable: unavailable[d.id] ?? null,
      };
    });
  }

  function visible(r) {
    if (state.show === "changed" && !r.own) return false;
    if (state.show === "clashes" && !r.clash.length) return false;
    if (state.byKey && r.chord !== state.byKey && !r.secondaries.includes(state.byKey)) return false;
    const words = state.query.trim().toLowerCase().split(/\s+/).filter(Boolean);
    if (!words.length) return true;
    const hay = [r.d.desc, r.d.id, r.d.group, keys.words(r.chord ?? r.configured), ...r.secondaries.map(keys.words)].join(" ").toLowerCase();
    return words.every((w) => hay.includes(w));
  }

  function render() {
    const custom = keys.mode === "custom";
    const rows = model();
    const changed = rows.filter((r) => r.own).length;
    const clashes = rows.filter((r) => r.clash.length).length;
    const reserved = rows.filter((r) => r.reserved).length;
    if (state.show === "changed" && !custom) state.show = "all";

    for (const b of modeSeg.children) {
      const on = b.dataset.mode === keys.mode;
      b.classList.toggle("active", on);
      b.setAttribute("aria-checked", String(on));
    }
    customBar.hidden = !custom;
    baseSelect.value = keys.custom.base;
    changedLabel.textContent = changed ? `${changed} changed` : "no changes";
    resetBtn.hidden = state.confirmReset;
    resetBtn.disabled = !changed;
    resetConfirm.hidden = !state.confirmReset;
    if (state.confirmReset) paintResetConfirm(changed);

    for (const b of showSeg.children) {
      const id = b.dataset.show;
      b.classList.toggle("active", id === state.show);
      b.hidden = (id === "changed" && !custom) || (id === "clashes" && !clashes && state.show !== "clashes");
      b.textContent = id === "changed" ? `Changed ${changed}` : id === "clashes" ? `Clashes ${clashes}` : "All";
    }
    showSeg.hidden = !custom && !clashes && state.show === "all";   // with nothing to filter by, "All" on its own says nothing
    byKeyBtn.classList.toggle("listening", state.listening);
    byKeyChip.hidden = !state.byKey;
    if (state.byKey) {
      byKeyChip.textContent = `${fmt(state.byKey)} ×`;
      byKeyChip.title = "Show every command again";
    }

    // one line, not a paragraph: which keymap this is, and what a key press does here
    hint.textContent = state.listening
      ? "Press the keys to find what they run."
      : custom
        ? `Custom, on ${presetName(keys.custom.base)}. Click a key, then press the new one; Backspace leaves none.`
        : `${presetName(keys.mode)} is native's keymap, read only. Click a key to customise it.`;
    if (reserved && !state.listening) hint.appendChild(el("span", "sc-warn", `  !  ${reserved} ${reserved === 1 ? "key is" : "keys are"} the browser's own until Sonic Pi is installed as an app`));

    const had = list.contains(document.activeElement) ? document.activeElement : null;
    const hadId = had?.closest("[data-id]")?.dataset.id, hadRole = had?.dataset.role;
    list.textContent = "";
    let shown = 0;
    for (const group of GROUPS) {
      const inGroup = rows.filter((r) => r.d.group === group && visible(r));
      if (!inGroup.length) continue;
      list.appendChild(el("div", "sc-group", group));
      for (const r of inGroup) {
        list.appendChild(rowEl(r, custom));
        if (state.clash?.id === r.d.id) list.appendChild(clashEl());
        if (state.hint === r.d.id) list.appendChild(hintEl());
        shown++;
      }
    }
    if (!shown) list.appendChild(el("div", "sc-empty", state.byKey ? `Nothing is on ${fmt(state.byKey)}.` : "No shortcut matches."));
    if (state.clash) list.querySelector("[data-role='reassign']")?.focus();
    else if (state.hint) list.querySelector("[data-role='customise']")?.focus();
    else if (hadId) focusIn(hadId, hadRole === "reset" && !list.querySelector(`[data-id="${hadId}"] [data-role="reset"]`) ? "key" : hadRole);
  }

  function rowEl(r, custom) {
    const { d } = r;
    const row = el("div", "sc-row");
    row.dataset.id = d.id;
    row.setAttribute("role", "listitem");
    row.classList.toggle("clash", r.clash.length > 0);
    row.classList.toggle("changed", r.own);
    row.classList.toggle("unavailable", !!r.unavailable);

    const desc = el("div", "sc-desc", d.desc);
    if (r.unavailable) {
      const tag = el("span", "sc-tag", "native only");
      tag.title = r.unavailable;
      desc.appendChild(tag);
    }

    const tips = [];
    if (r.clash.length) tips.push(`Also on ${r.clash.map(name).join(", ")}. A key on two commands runs neither, as in native Sonic Pi.`);
    if (r.takenBy.length) tips.push(`Not in force: ${fmt(r.configured)} is on ${r.takenBy.map(name).join(", ")}, which you set.`);
    if (r.moved) tips.push(`${fmt(r.configured)} is the menu bar's key here, so this is ${fmt(r.chord)}.`);
    if (r.own) tips.push(`Changed from ${r.base ? fmt(r.base) : "none"}.`);
    if (r.reserved) tips.push(`The browser keeps ${fmt(r.chord)} for itself (its tabs, windows or address bar): it reaches Sonic Pi installed as an app.`);
    if (r.unavailable) tips.push(r.unavailable);
    if (r.secondaries.length) tips.push(`Also works: ${r.secondaries.map(fmt).join(", ")}.`);

    const keysBox = el("div", "sc-keys");
    if (r.clash.length || r.reserved || r.takenBy.length) {
      const flag = el("span", `sc-flag${r.clash.length ? " bad" : ""}`, "!");
      flag.title = tips.join("\n");
      flag.setAttribute("aria-hidden", "true");
      keysBox.appendChild(flag);
    }
    const shownChord = r.chord ?? r.configured;
    const key = button("sc-key");
    key.dataset.role = "key";
    key.classList.toggle("off", !r.chord && !!r.configured);
    key.classList.toggle("none", !shownChord);
    key.textContent = shownChord ? fmt(shownChord) : "none";
    key.title = [...tips, custom ? "Click to change" : "Click to customise"].join("\n");
    key.setAttribute("aria-label", `${d.desc}: ${shownChord ? fmt(shownChord) : "no shortcut"}. ${tips.join(" ")} ${custom ? "Press Enter to change" : ""}`.trim());
    key.addEventListener("click", () => (keys.mode === "custom" ? startRecording(d.id) : showHint(d.id)));
    key.addEventListener("blur", () => { if (state.recording === d.id) stopRecording(); });
    keysBox.appendChild(key);
    if (custom && r.own) {
      const back = button("sc-reset", "↺", `Back to ${r.base ? fmt(r.base) : "none"}`);
      back.dataset.role = "reset";
      back.addEventListener("click", () => {
        const c = keys.custom;
        delete c.keys[d.id];
        keys.setCustom(c);
        say(`${d.desc}: back to ${r.base ? fmt(r.base) : "none"}`);
        focusIn(d.id);
      });
      keysBox.appendChild(back);
    } else keysBox.appendChild(el("span", "sc-reset-gap"));
    row.append(desc, keysBox);
    return row;
  }

  // ── Recording a key ──

  function startRecording(id) {
    if (state.recording && state.recording !== id) stopRecording();
    state.recording = id;
    state.clash = state.hint = null;
    const key = keyButton(id);
    if (!key) return;
    key.classList.add("recording");
    key.textContent = "Type shortcut…";
    key.focus();
    say(`Recording a shortcut for ${name(id)}. Press the keys. Backspace for none, Escape to keep it.`);
  }

  function stopRecording() {
    const id = state.recording;
    state.recording = null;
    const key = id && keyButton(id);
    if (!key) return;
    const r = model().find((m) => m.d.id === id);
    key.classList.remove("recording");
    key.textContent = r.chord ?? r.configured ? fmt(r.chord ?? r.configured) : "none";
  }

  function commit(id, notation) {
    state.recording = null;
    const chord = keys.resolve(notation);
    const b = keys.bindings;
    const clashes = chord ? DEFS.filter((d) => d.id !== id && b.primary.get(d.id) === chord).map((d) => d.id) : [];
    if (clashes.length) {
      state.clash = { id, notation, chord, clashes };
      render();
      say(`${fmt(chord)} is already on ${clashes.map(name).join(", ")}. Reassign it, keep both, or cancel.`);
      return;
    }
    setCustomKeys({ [id]: notation });
    focusIn(id);
    say(chord ? `${name(id)}: ${fmt(chord)}` : `${name(id)}: no shortcut`);
    if (chord && !standalone && keys.reserved(chord)) toast(`${fmt(chord)} is the browser's: it reaches Sonic Pi installed as an app`);
  }

  // The recorder and the key search take every key first, before any shortcut sees it
  window.addEventListener("keydown", (e) => {
    if (overlay.hidden || (!state.recording && !state.listening)) return;
    if (e.key === "Tab" && !e.ctrlKey && !e.metaKey && !e.altKey) {
      if (state.recording) stopRecording();
      state.listening = false;
      byKeyBtn.classList.remove("listening");
      return;
    }
    e.preventDefault();
    e.stopImmediatePropagation();
    if (e.repeat) return;
    if (e.key === "Escape") {
      const id = state.recording;
      if (id) stopRecording();
      state.listening = false;
      render();
      if (id) focusIn(id); else byKeyBtn.focus();
      say("Kept as it was");
      return;
    }
    const notation = keys.notation(e);
    if (state.listening) {
      if (!notation) return;
      state.listening = false;
      state.byKey = keys.resolve(notation);
      render();
      byKeyChip.focus();
      say(`Showing what is on ${fmt(state.byKey)}`);
      return;
    }
    const plain = !e.ctrlKey && !e.metaKey && !e.altKey && !e.shiftKey;
    if (plain && (e.key === "Backspace" || e.key === "Delete")) commit(state.recording, "");
    else if (notation) commit(state.recording, notation);
  }, true);

  function clashEl() {
    const { id, notation, chord, clashes } = state.clash;
    const box = el("div", "sc-inline sc-clash");
    box.setAttribute("role", "alert");
    box.appendChild(el("span", "", `${fmt(chord)} is already on ${clashes.map(name).join(", ")}.`));
    const actions = el("span", "sc-actions");
    const reassign = button("sp-mini-btn primary", "Reassign to this");
    reassign.dataset.role = "reassign";
    const both = button("sp-mini-btn", "Keep both");
    const cancel = button("sp-mini-btn", "Cancel");
    reassign.addEventListener("click", () => {
      state.clash = null;
      setCustomKeys({ ...Object.fromEntries(clashes.map((c) => [c, ""])), [id]: notation });
      focusIn(id);
      say(`${fmt(chord)} is now ${name(id)}`);
    });
    both.addEventListener("click", () => {
      state.clash = null;
      setCustomKeys({ [id]: notation });
      focusIn(id);
    });
    cancel.addEventListener("click", () => {
      state.clash = null;
      render();
      focusIn(id);
    });
    actions.append(reassign, both, cancel);
    box.appendChild(actions);
    return box;
  }

  function showHint(id) {
    state.hint = state.hint === id ? null : id;
    render();
    if (!state.hint) focusIn(id);
  }

  function hintEl() {
    const id = state.hint, preset = keys.mode;
    const box = el("div", "sc-inline");
    box.appendChild(el("span", "", `${presetName(preset)} is read only. Customise it to change this key, and keep the rest.`));
    const actions = el("span", "sc-actions");
    const go = button("sp-mini-btn primary", `Customise ${presetName(preset)}`);
    go.dataset.role = "customise";
    const no = button("sp-mini-btn", "Not now");
    go.addEventListener("click", () => {
      state.hint = null;
      if (keys.custom.base !== preset) changeBase(preset);
      keys.setMode("custom");
      startRecording(id);
    });
    no.addEventListener("click", () => {
      state.hint = null;
      render();
      focusIn(id);
    });
    actions.append(go, no);
    box.appendChild(actions);
    return box;
  }

  function paintResetConfirm(changed) {
    resetConfirm.textContent = "";
    const yes = button("sp-mini-btn primary", "Reset");
    const no = button("sp-mini-btn", "Keep them");
    yes.addEventListener("click", () => {
      state.confirmReset = false;
      keys.setCustom({ base: keys.custom.base, keys: {} });
      say(`Back to ${presetName(keys.custom.base)}`);
      resetBtn.focus();
    });
    no.addEventListener("click", () => {
      state.confirmReset = false;
      render();
      resetBtn.focus();
    });
    resetConfirm.append(el("span", "", `Discard ${changed === 1 ? "your change" : `all ${changed} changes`}?`), yes, no);
  }

  // ── Controls ──

  closeBtn.addEventListener("click", close);
  overlay.addEventListener("pointerdown", (e) => { if (e.target === overlay) close(); });
  baseSelect.addEventListener("change", () => changeBase(baseSelect.value));
  resetBtn.addEventListener("click", () => {
    state.confirmReset = true;
    render();
    resetConfirm.querySelector("button")?.focus();
  });
  search.addEventListener("input", () => { state.query = search.value; render(); });
  byKeyBtn.addEventListener("click", () => {
    state.listening = !state.listening;
    state.byKey = null;
    render();
    byKeyBtn.focus();
    if (state.listening) say("Press the keys to find what is on them");
  });
  byKeyChip.addEventListener("click", () => { state.byKey = null; render(); byKeyBtn.focus(); });
  importBtn.addEventListener("click", () => file.click());
  file.addEventListener("change", async () => {
    const f = file.files[0];
    if (!f) return;
    const { base, keys: imported } = parseIni(await f.text());
    file.value = "";
    keys.setCustom({ base: base ?? "mac", keys: imported });   // native's import: a file naming no base is on Mac
    keys.setMode("custom");
    const n = Object.keys(imported).length;
    toast(`imported ${f.name}: ${presetName(base ?? "mac")} with ${n} change${n === 1 ? "" : "s"}`);
  });
  exportBtn.addEventListener("click", () => {
    const custom = keys.mode === "custom" ? keys.custom : { base: keys.mode, keys: {} };
    const a = el("a");
    a.href = URL.createObjectURL(new Blob([serializeIni(custom)], { type: "text/plain" }));
    a.download = "sonic-pi-shortcuts.ini";
    a.click();
    setTimeout(() => URL.revokeObjectURL(a.href), 1000);
  });

  // up and down move between the keys, as native's tree does
  list.addEventListener("keydown", (e) => {
    if (state.recording || (e.key !== "ArrowDown" && e.key !== "ArrowUp")) return;
    const all = [...list.querySelectorAll(".sc-key")];
    const i = all.indexOf(document.activeElement);
    if (i < 0) return;
    e.preventDefault();
    all[Math.max(0, Math.min(all.length - 1, i + (e.key === "ArrowDown" ? 1 : -1)))].focus();
  });

  // Escape and Tab are the sheet's while it is up, wherever the focus went (a file picker hands it back to the page)
  document.addEventListener("keydown", (e) => {
    if (overlay.hidden || e.defaultPrevented) return;
    if (e.key === "Escape") {
      e.preventDefault();
      e.stopPropagation();
      if (state.clash || state.hint || state.confirmReset) {
        const id = state.clash?.id ?? state.hint;
        state.clash = state.hint = null;
        state.confirmReset = false;
        render();
        if (id) focusIn(id);
      } else close();
    } else if (e.key === "Tab") {
      const focusable = [...dialog.querySelectorAll("button, input, select")].filter((n) => !n.disabled && !n.hidden && n.offsetParent !== null);
      const first = focusable[0], last = focusable[focusable.length - 1];
      if (!dialog.contains(document.activeElement)) { e.preventDefault(); (e.shiftKey ? last : first).focus(); }
      else if (e.shiftKey && document.activeElement === first) { e.preventDefault(); last.focus(); }
      else if (!e.shiftKey && document.activeElement === last) { e.preventDefault(); first.focus(); }
    }
  });

  keys.onChange(() => { if (!overlay.hidden) render(); });

  function open() {
    if (!overlay.hidden) return;
    lastFocus = document.activeElement;
    overlay.hidden = false;
    render();
    search.focus();
  }

  function close() {
    if (state.recording) stopRecording();
    Object.assign(state, { listening: false, clash: null, hint: null, confirmReset: false });
    overlay.hidden = true;
    lastFocus?.focus?.();
  }

  return {
    open,
    close,
    get isOpen() { return !overlay.hidden; },
  };
}
