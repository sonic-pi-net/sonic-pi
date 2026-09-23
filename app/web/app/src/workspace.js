// SPDX-License-Identifier: AGPL-3.0-or-later
// The workspace: every set and its buffers, which set shows and which of its buffers, and what code arriving does.
// The one place this lives. The editor shows a buffer of it and hands its edits back; the buffer tabs, the sets'
// chip and list and the Share panel draw from it; each hears what changed (subscribe) and draws again. No DOM here,
// so all of it is tested in Node (test/workspace.test.mjs).
//
// A set has a name, a description if it's given one, and its own number of buffers, up to MAX_BUFFERS (native's ten,
// and the .sonicpi format's): a new set has defaultSize; one whose file doesn't say (native's, and every set from
// before sets had sizes) has ten; none is so small its code is cut off. Its buffers may have names, each the name its
// file is saved by: kept with the workspace, not in the file, while what a buffer's name is in the format is open.
//
// Code arriving never goes over code: a set goes on top as a set of its own (or takes the place of one whose buffers
// are all free); a buffer's code goes into the buffer showing if it is free, else the next free one on from it,
// else a set of its own. A free buffer is an empty one, or one holding the first visit's starter untouched.
//
// Kept in storage (the browser's localStorage, or anything with get, set and remove): sp-workspace, the list (each
// set's id, name, description, size, buffer names, when last used) and which set shows; and sp-set:<id>, each set as
// its .sonicpi file's text, so what the browser keeps is what a file or a link would carry. Edits are written after
// a pause (schedule); anything else at once. The storage from before the workspace (sp-buffer:<i>, sp-sets) is read
// once and carried over.
import { serialise, deserialise, isSet, MAX_BUFFERS } from "./set-bundle.js";

export { MAX_BUFFERS };
export const NAME_MAX = 80, DESCRIPTION_MAX = 500;
const LEGACY_SIZE = 10;
const INDEX_KEY = "sp-workspace", TEXT_KEY = "sp-set:";
const LEGACY = { list: "sp-sets", current: "sp-set-current", buffer: "sp-buffer:", active: "sp-buffer-active" };

// a name or a description as kept: text, trimmed, within bounds; else none
const words = (v, max) => (typeof v === "string" && v.trim() ? v.trim().slice(0, max) : "");
const clampSize = (n) => Math.min(MAX_BUFFERS, Math.max(1, n));
const filledUpTo = (buffers) => buffers.reduce((last, t, i) => (t && t.trim() ? i + 1 : last), 0);
// a set's size as its meta says, else ten; never so few its code is cut off
const sizeFor = (meta, buffers) => Math.max(Number.isInteger(meta?.size) && meta.size >= 1 && meta.size <= MAX_BUFFERS ? meta.size : LEGACY_SIZE, filledUpTo(buffers));

/**
 * storage: { get(key), set(key, text), remove(key) }; starter: the first visit's buffer 0; formerStarters: starters an
 * earlier version gave, as free as today's; defaultSize: a new set's buffers; schedule(fn): runs fn after a pause
 * (edits are written then); now(): the time; warn(text): said when the storage refuses a write.
 */
export function createWorkspace({ storage, starter = "", formerStarters = [], defaultSize = 8, schedule = (fn) => setTimeout(fn, 300), now = Date.now, warn = () => {} }) {
  const isBlank = (t) => !t.trim() || t === starter || formerStarters.includes(t);
  const listeners = new Set();
  const tell = (kind, detail = {}) => { for (const fn of listeners) fn({ kind, ...detail }); };
  const newId = () => now().toString(36) + Math.random().toString(36).slice(2, 7);

  /** @type {Map<string, {id, name, description, size, used, active, buffers: string[], zooms: number[], meta: object, bufferNames: object}>} */
  const sets = new Map();
  let currentId = null;

  const blankSet = (fields) => ({ id: newId(), name: "My Set", description: "", size: clampSize(defaultSize), used: now(), active: 0, buffers: new Array(MAX_BUFFERS).fill(""), zooms: [], meta: {}, bufferNames: {}, ...fields });
  const fromText = (text, fields) => {
    const load = deserialise(text ?? "");
    const buffers = load.ok ? load.buffers : new Array(MAX_BUFFERS).fill("");
    const size = sizeFor(load.meta, buffers);
    return blankSet({ size, active: Math.min(load.current, size - 1), buffers, zooms: load.zooms, meta: load.meta, description: words(load.meta?.description, DESCRIPTION_MAX), ...fields });
  };
  const current = () => sets.get(currentId);
  const uniqueName = (name, self) => {
    const taken = new Set([...sets.values()].filter((s) => s !== self).map((s) => s.name));
    if (!taken.has(name)) return name;
    let n = 2;
    while (taken.has(`${name} ${n}`)) n++;
    return `${name} ${n}`;
  };

  // ── keeping it ──────────────────────────────────────────────────────────
  const dirty = new Set();
  let scheduled = false;
  const fileOf = (s) => {
    const meta = { ...s.meta, name: s.name, size: s.size };
    if (s.description) meta.description = s.description; else delete meta.description;
    return serialise(s.buffers.slice(0, s.size), s.active, s.zooms, meta);
  };
  function save() {
    scheduled = false;
    const index = { v: 1, current: currentId, sets: [...sets.values()].map(({ id, name, description, size, used, bufferNames }) => ({ id, name, description, size, used, bufferNames })) };
    try {
      for (const id of dirty) if (sets.has(id)) storage.set(TEXT_KEY + id, fileOf(sets.get(id)));
      dirty.clear();
      storage.set(INDEX_KEY, JSON.stringify(index));
    } catch { warn("the browser's storage is full: your sets can't be kept until one is deleted"); }
  }
  const saveNow = (...ids) => { for (const id of ids) dirty.add(id); save(); };
  const saveLater = (id) => { dirty.add(id); if (!scheduled) { scheduled = true; schedule(save); } };

  // ── reading it back, or carrying over what was there before ─────────────
  function load() {
    let index = null;
    try { index = JSON.parse(storage.get(INDEX_KEY)); } catch {}
    if (index?.v === 1 && Array.isArray(index.sets) && index.sets.length) {
      for (const r of index.sets) {
        const s = fromText(storage.get(TEXT_KEY + r.id), { id: r.id, used: r.used ?? 0, bufferNames: r.bufferNames ?? {} });
        s.name = words(r.name, NAME_MAX) || s.name;
        if (r.description != null) s.description = words(r.description, DESCRIPTION_MAX);
        if (Number.isInteger(r.size)) s.size = Math.max(clampSize(r.size), filledUpTo(s.buffers));
        sets.set(s.id, s);
      }
      currentId = sets.has(index.current) ? index.current : sets.keys().next().value;
      return;
    }
    const legacyBuffers = Array.from({ length: MAX_BUFFERS }, (_, i) => storage.get(LEGACY.buffer + i));
    const legacyActive = parseInt(storage.get(LEGACY.active) ?? "0", 10) || 0;
    let list = null;
    try { list = JSON.parse(storage.get(LEGACY.list)); } catch {}
    if (Array.isArray(list) && list.length) {
      // sets from before the workspace: the one showing kept its buffers as the editor did, the rest as their text
      const legacyCurrent = storage.get(LEGACY.current);
      for (const r of list) {
        const fields = { id: r.id, used: r.used ?? 0, bufferNames: r.bufferNames ?? {} };
        const s = r.id === legacyCurrent
          ? blankSet({ ...fields, buffers: legacyBuffers.map((t) => t ?? ""), active: legacyActive, zooms: r.zooms ?? [], meta: r.meta ?? {} })
          : fromText(storage.get(TEXT_KEY + r.id), fields);
        s.name = words(r.name, NAME_MAX) || s.name;
        s.description = words(r.description, DESCRIPTION_MAX) || s.description;
        s.size = Math.max(LEGACY_SIZE, filledUpTo(s.buffers));
        s.active = Math.min(s.active, s.size - 1);
        sets.set(s.id, s);
      }
      currentId = sets.has(legacyCurrent) ? legacyCurrent : sets.keys().next().value;
    } else if (legacyBuffers.some((t) => t != null)) {
      // the buffers from before sets: one set, as many as there were if the last have code
      const buffers = legacyBuffers.map((t) => t ?? "");
      const size = buffers.slice(defaultSize).some((t) => t.trim()) ? MAX_BUFFERS : clampSize(defaultSize);
      const s = blankSet({ buffers, size, active: Math.min(legacyActive, size - 1) });
      sets.set(s.id, s);
      currentId = s.id;
    } else {
      const s = blankSet();
      s.buffers[0] = starter;
      sets.set(s.id, s);
      currentId = s.id;
    }
    saveNow(...sets.keys());
  }
  load();

  // ── what arrives ────────────────────────────────────────────────────────
  // a new set on top, showing; or, every buffer showing free, in its place
  function push(fields) {
    const c = current();
    if (c.buffers.slice(0, c.size).every(isBlank)) {
      const id = c.id;
      sets.delete(id);
      const s = { ...blankSet(fields), id, used: now() };
      s.name = uniqueName(s.name, s);
      sets.set(id, s);
      saveNow(id);
      return s;
    }
    const s = blankSet({ ...fields, used: now() });
    s.name = uniqueName(s.name, s);
    sets.set(s.id, s);
    currentId = s.id;
    saveNow(c.id, s.id);
    return s;
  }

  const api = {
    /** Hears what changed: { kind: "edit" (the editor's own, i), "buffer" (a buffer shown, or its text put there from outside), "set" (another set shows), "sets" (a name, a description, the list) }. */
    subscribe(fn) { listeners.add(fn); return () => listeners.delete(fn); },

    get active() { return current().active; },
    get size() { return current().size; },
    text: (i) => current().buffers[i] ?? "",
    isBlank,
    /** The set showing: { id, name, description, size, active }. */
    set() { const { id, name, description, size, active } = current(); return { id, name, description, size, active }; },
    /** Every set, most recently used first: { id, name, description, size, used, current, filled (buffers with code), first (its first line of code) }. */
    sets() {
      return [...sets.values()].sort((a, b) => b.used - a.used).map((s) => {
        const full = s.buffers.slice(0, s.size).filter((t) => t.trim());
        const first = full.map((t) => t.split("\n").find((l) => l.trim() && !l.trim().startsWith("#"))).find(Boolean) ?? "";
        return { id: s.id, name: s.name, description: s.description, size: s.size, used: s.used, current: s.id === currentId, filled: full.length, first: first.trim() };
      });
    },
    /** The set showing, as its .sonicpi file's text. */
    fileText: () => fileOf(current()),

    /** Buffer i shown, if the set has it. */
    showBuffer(i) {
      const c = current();
      if (!Number.isInteger(i) || i < 0 || i >= c.size) return false;
      if (i === c.active) return true;
      c.active = i;
      saveLater(c.id);
      tell("buffer", { buffer: i });
      return true;
    },
    /** The editor's edit of buffer i. */
    edit(i, text) {
      const c = current();
      if (i < 0 || i >= MAX_BUFFERS || c.buffers[i] === text) return;
      c.buffers[i] = text;
      saveLater(c.id);
      tell("edit", { buffer: i });
    },
    bufferName: (i) => current().bufferNames[i] || `buffer_${i}`,
    setBufferName(i, name) {
      const c = current();
      name = words(name, NAME_MAX);
      c.bufferNames = { ...c.bufferNames };
      if (name && name !== `buffer_${i}`) c.bufferNames[i] = name; else delete c.bufferNames[i];
      saveNow(c.id);
      tell("sets");
    },

    /** A buffer's code arriving: where it went, { buffer, set } (set, its name, when a new one). */
    openProgram(code, { name = "Shared Code", bufferName = "" } = {}) {
      const c = current();
      for (let k = 0; k < c.size; k++) {
        const i = (c.active + k) % c.size;
        if (!isBlank(c.buffers[i])) continue;
        c.buffers[i] = code;
        c.active = i;
        const names = { ...c.bufferNames };
        if (words(bufferName, NAME_MAX)) names[i] = words(bufferName, NAME_MAX); else delete names[i];
        c.bufferNames = names;
        saveNow(c.id);
        tell("buffer", { buffer: i, replaced: true });
        return { buffer: i, set: null };
      }
      const buffers = new Array(MAX_BUFFERS).fill("");
      buffers[0] = code;
      const s = push({ name: words(name, NAME_MAX) || "Shared Code", buffers, bufferNames: words(bufferName, NAME_MAX) ? { 0: words(bufferName, NAME_MAX) } : {} });
      tell("set");
      return { buffer: 0, set: s.name };
    },
    /** A set's file text arriving: { ok, error, name, description }. */
    openSet(text, { fallbackName = "Shared Set" } = {}) {
      if (!isSet(text) && !/^#-- buffer \d+$/m.test(text)) return { ok: false, error: "not a Sonic Pi set file" };
      const load = deserialise(text);
      if (!load.ok) return { ok: false, error: load.error };
      const size = sizeFor(load.meta, load.buffers);
      const s = push({
        name: words(load.meta.name, NAME_MAX) || words(fallbackName, NAME_MAX) || "Shared Set", description: words(load.meta.description, DESCRIPTION_MAX),
        size, active: Math.min(load.current, size - 1), buffers: load.buffers, zooms: load.zooms, meta: load.meta,
      });
      tell("set");
      return { ok: true, name: s.name, description: s.description };
    },

    /** A new set, empty and the default size, named and described as asked, shown; the one showing kept. → { id, name } */
    newSet({ name = "New Set", description = "" } = {}) {
      const c = current();
      const s = blankSet({ name: words(name, NAME_MAX) || "New Set", description: words(description, DESCRIPTION_MAX), used: now() });
      s.name = uniqueName(s.name, s);
      sets.set(s.id, s);
      currentId = s.id;
      saveNow(c.id, s.id);
      tell("set");
      return { id: s.id, name: s.name };
    },
    /** Another set shown; the one showing kept as it is. */
    switchSet(id) {
      if (id === currentId || !sets.has(id)) return false;
      const was = currentId;
      currentId = id;
      current().used = now();
      saveNow(was, id);
      tell("set");
      return true;
    },
    /** A set's name and description: a blank name leaves the name it had. */
    editSet(id, { name, description } = {}) {
      const s = sets.get(id);
      if (!s) return;
      const n = words(name, NAME_MAX);
      if (n && n !== s.name) s.name = uniqueName(n, s);
      if (description != null) s.description = words(description, DESCRIPTION_MAX);
      saveNow(id);
      tell("sets");
    },
    /** A set not showing, deleted for good. */
    removeSet(id) {
      if (id === currentId || !sets.has(id)) return false;
      sets.delete(id);
      try { storage.remove(TEXT_KEY + id); } catch {}
      save();
      tell("sets");
      return true;
    },
    /** Whatever is waiting to be kept, kept now (the page going away). */
    flush() { if (dirty.size || scheduled) save(); },
  };
  return api;
}
