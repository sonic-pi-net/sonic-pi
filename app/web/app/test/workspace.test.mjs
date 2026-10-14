// SPDX-License-Identifier: AGPL-3.0-or-later
// The workspace (src/workspace.js): every set and its buffers, which one shows, and what arriving code does. The one
// place this state lives: the editor shows a buffer of it, the tabs and the sets' views draw from it, and it keeps
// itself in the browser's storage, each set as its .sonicpi file's text.
import { test } from "node:test";
import assert from "node:assert/strict";
import { createWorkspace } from "../src/workspace.js";
import { serialise, deserialise } from "../src/set-bundle.js";

const STARTER = "# Welcome\nplay 60\n";
function memory(entries = {}) {
  const m = new Map(Object.entries(entries));
  return { get: (k) => (m.has(k) ? m.get(k) : null), set: (k, v) => void m.set(k, String(v)), remove: (k) => void m.delete(k), keys: () => [...m.keys()], m };
}
let clock = 1000;
const make = (storage = memory(), opts = {}) => createWorkspace({ storage, starter: STARTER, defaultSize: 8, now: () => ++clock, schedule: (fn) => fn(), ...opts });
const fill = (ws, from = 0) => { for (let i = from; i < ws.size; i++) ws.edit(i, `play ${i}\n`); };

// ── a first visit ─────────────────────────────────────────────────────────

test("a first visit has one set, My Set, of the default size, the starter in buffer 0", () => {
  const ws = make();
  assert.equal(ws.sets().length, 1);
  assert.equal(ws.set().name, "My Set");
  assert.equal(ws.size, 8);
  assert.equal(ws.active, 0);
  assert.equal(ws.text(0), STARTER);
  assert.equal(ws.text(7), "");
});

test("what it holds is kept: a workspace made again on the same storage is the same", () => {
  const storage = memory();
  const ws = make(storage);
  ws.edit(3, "play 70\n");
  ws.showBuffer(3);
  ws.editSet(ws.set().id, { name: "Gig", description: "for Tuesday" });
  const again = make(storage);
  assert.equal(again.text(3), "play 70\n");
  assert.equal(again.active, 3);
  assert.equal(again.set().name, "Gig");
  assert.equal(again.set().description, "for Tuesday");
});

// ── buffers ───────────────────────────────────────────────────────────────

test("a buffer is shown only within its set's size", () => {
  const ws = make();
  assert.equal(ws.showBuffer(7), true);
  assert.equal(ws.active, 7);
  assert.equal(ws.showBuffer(8), false);
  assert.equal(ws.active, 7);
});

test("a buffer has a name: its file's, else buffer_<i>", () => {
  const ws = make();
  assert.equal(ws.bufferName(2), "buffer_2");
  ws.setBufferName(2, "  drums ");
  assert.equal(ws.bufferName(2), "drums");
  ws.setBufferName(2, "");
  assert.equal(ws.bufferName(2), "buffer_2");
});

test("a set's buffer names travel in its file, so native and every link read them", () => {
  const ws = make();
  ws.edit(0, "play 60\n");
  ws.setBufferName(0, "drums");
  ws.setBufferName(2, "pads");
  const load = deserialise(ws.fileText());
  assert.deepEqual(load.names.slice(0, 3), ["drums", "", "pads"]);
  // a set opened from a file has its buffers' names
  ws.openSet(serialise(["a\n", "b\n"], 1, [], { name: "Gig", names: ["kick", "bass"] }));
  assert.equal(ws.bufferName(0), "kick");
  assert.equal(ws.bufferName(1), "bass");
  assert.equal(ws.bufferName(2), "buffer_2");
});

test("a set with no named buffers writes no names", () => {
  const ws = make();
  ws.edit(0, "play 60\n");
  assert.equal("names" in deserialise(ws.fileText()).meta, false);
});

test("names kept beside the sets by an earlier version are carried into their files", () => {
  const storage = memory({
    "sp-workspace": JSON.stringify({ v: 1, current: "a", sets: [{ id: "a", name: "Gig", size: 8, used: 1, bufferNames: { 1: "bass" } }] }),
    "sp-set:a": serialise(["x\n", "y\n"], 1, [], { name: "Gig", size: 8 }),
  });
  const ws = make(storage);
  assert.equal(ws.bufferName(1), "bass");
  ws.edit(0, "z\n");
  assert.equal(deserialise(storage.get("sp-set:a")).names[1], "bass");
});

// ── a program arriving: never over code ──────────────────────────────────

test("a program goes into the buffer showing when it is free, the untouched starter counting as free", () => {
  const ws = make();
  const r = ws.openProgram("play 1\n");
  assert.deepEqual(r, { buffer: 0, set: null });
  assert.equal(ws.text(0), "play 1\n");
});

test("a program goes into the next free buffer on from the one showing, wrapping round, within the set's size", () => {
  const ws = make();
  fill(ws);
  ws.edit(2, "");
  ws.showBuffer(5);
  const r = ws.openProgram("play 100\n");
  assert.deepEqual(r, { buffer: 2, set: null });
  assert.equal(ws.active, 2);
  assert.equal(ws.text(5), "play 5\n");
});

test("with every buffer full a program starts a set of its own, the one it would have covered kept whole", () => {
  const ws = make();
  fill(ws);
  const first = ws.set().id, before = Array.from({ length: 8 }, (_, i) => ws.text(i));
  const r = ws.openProgram("play 100\n", { name: "Shared Code" });
  assert.equal(r.buffer, 0);
  assert.equal(r.set, "Shared Code");
  assert.notEqual(ws.set().id, first);
  assert.equal(ws.text(0), "play 100\n");
  assert.equal(ws.size, 8);
  ws.switchSet(first);
  assert.deepEqual(Array.from({ length: 8 }, (_, i) => ws.text(i)), before);
});

test("a program from a file names its buffer after the file", () => {
  const ws = make();
  ws.openProgram("sample :bd_haus\n", { bufferName: "drums" });
  assert.equal(ws.bufferName(0), "drums");
});

test("the same program arriving again shows the buffer it is in: no second copy (a link's page reloaded, a file loaded twice)", () => {
  const ws = make();
  ws.edit(0, "mine\n");
  ws.edit(3, "play 1\n");
  const r = ws.openProgram("play 1\n");
  assert.deepEqual(r, { buffer: 3, set: null });
  assert.equal(ws.active, 3);
  assert.equal(Array.from({ length: 8 }, (_, i) => ws.text(i)).filter((t) => t === "play 1\n").length, 1);
  // a buffer full of code that only looks alike takes no part: the same code, to the character
  assert.equal(ws.openProgram("play 1").buffer, 4);
});

// ── a set arriving ────────────────────────────────────────────────────────

test("a set goes on top as a set of its own, by its own name, the one showing kept", () => {
  const ws = make();
  ws.edit(0, "mine\n");
  const first = ws.set().id;
  const r = ws.openSet(serialise(["a\n", "b\n"], 1, [], { name: "Tuesday Gig", description: "four loops" }));
  assert.equal(r.ok, true);
  assert.equal(r.name, "Tuesday Gig");
  assert.equal(r.description, "four loops");
  assert.equal(ws.set().name, "Tuesday Gig");
  assert.equal(ws.active, 1);
  assert.equal(ws.sets().length, 2);
  ws.switchSet(first);
  assert.equal(ws.text(0), "mine\n");
});

test("a set with no name goes by the one it is given (its file's)", () => {
  const ws = make();
  ws.edit(0, "mine\n");
  assert.equal(ws.openSet(serialise(["a\n"], 0), { fallbackName: "gig" }).name, "gig");
});

test("when every buffer showing is free a set takes the place of the one showing: no second set", () => {
  const ws = make();
  ws.openSet(serialise(["a\n"], 0, [], { name: "Gig" }));
  assert.equal(ws.sets().length, 1);
  assert.equal(ws.set().name, "Gig");
});

test("a set's size is what its file says; a file that doesn't say is ten (native's, and every set before sizes)", () => {
  const ws = make();
  ws.openSet(serialise(["a\n"], 0, [], { name: "A", size: 4 }));
  assert.equal(ws.size, 4);
  ws.edit(0, "keep\n");
  ws.openSet(serialise(["b\n"], 0, [], { name: "B" }));
  assert.equal(ws.size, 10);
});

test("a set is never so small its code is cut off", () => {
  const ws = make();
  ws.openSet(serialise(["a\n", "", "", "", "", "", "c\n"], 0, [], { name: "A", size: 4 }));
  assert.equal(ws.size, 7);
  assert.equal(ws.text(6), "c\n");
});

test("a set made by hand, with no header, opens as native opens it: its line endings Windows' too", () => {
  const ws = make();
  ws.edit(0, "mine\n");
  const r = ws.openSet("#-- buffer 1\r\nplay 1\r\n", { fallbackName: "hand made" });
  assert.equal(r.ok, true);
  assert.equal(ws.set().name, "hand made");
  assert.equal(ws.text(1), "play 1");
});

test("a set whose buffers are all empty opens as a set", () => {
  const ws = make();
  ws.edit(0, "mine\n");
  const r = ws.openSet(serialise(new Array(10).fill(""), 2, [], { name: "Blank Slate" }));
  assert.equal(r.ok, true);
  assert.equal(ws.set().name, "Blank Slate");
  assert.equal(ws.sets().length, 2);
});

test("what is not a set is said to be not one, and nothing changes", () => {
  const ws = make();
  assert.deepEqual(ws.openSet("play 60\n"), { ok: false, error: "not a Sonic Pi set file" });
  assert.equal(ws.sets().length, 1);
});

test("a set from a newer version is refused, and nothing changes", () => {
  const ws = make();
  ws.edit(0, "mine\n");
  const r = ws.openSet("#-- Sonic Pi Set v99\n#-- buffer 0\nx\n");
  assert.equal(r.ok, false);
  assert.match(r.error, /newer version/);
  assert.equal(ws.sets().length, 1);
  assert.equal(ws.text(0), "mine\n");
});

test("names are kept apart: a second set of the same name is numbered", () => {
  const ws = make();
  ws.edit(0, "x\n");
  ws.openSet(serialise(["a\n"], 0, [], { name: "Gig" }));
  ws.openSet(serialise(["b\n"], 0, [], { name: "Gig" }));
  assert.deepEqual(ws.sets().map((s) => s.name).sort(), ["Gig", "Gig 2", "My Set"]);
});

// ── the set's file ────────────────────────────────────────────────────────

test("the set showing's file carries its buffers, the one showing, its name, description and size, and meta it doesn't know", () => {
  const ws = make();
  ws.openSet(serialise(["a\n", "b\n"], 1, [3, 4], { name: "Gig", description: "d", size: 6, future: { x: 1 } }));
  const back = deserialise(ws.fileText());
  assert.equal(back.ok, true);
  assert.deepEqual(back.buffers.slice(0, 2), ["a\n", "b\n"]);
  assert.equal(back.current, 1);
  assert.deepEqual(back.zooms.slice(0, 2), [3, 4]);
  assert.equal(back.meta.name, "Gig");
  assert.equal(back.meta.description, "d");
  assert.equal(back.meta.size, 6);
  assert.deepEqual(back.meta.future, { x: 1 });
});

// ── the sets ──────────────────────────────────────────────────────────────

test("sets are listed most recently used first, each with how many buffers have code and its first line of code", () => {
  const ws = make();
  ws.edit(0, "# a comment\nuse_synth :pluck\n");
  ws.edit(1, "play 1\n");
  ws.openSet(serialise(["x\n"], 0, [], { name: "Other" }));
  const [top, next] = ws.sets();
  assert.equal(top.name, "Other");
  assert.equal(top.current, true);
  assert.equal(next.name, "My Set");
  assert.equal(next.filled, 2);
  assert.equal(next.first, "use_synth :pluck");
});

test("switching sets shows another's buffers, the one showing put away as it was, its buffer showing too", () => {
  const ws = make();
  ws.edit(2, "mine\n");
  ws.showBuffer(2);
  const first = ws.set().id;
  ws.edit(0, "keep\n");
  ws.openSet(serialise(["x\n"], 0, [], { name: "Other" }));
  ws.switchSet(first);
  assert.equal(ws.active, 2);
  assert.equal(ws.text(2), "mine\n");
});

test("a set is named and described; a blank name leaves the name it had", () => {
  const ws = make();
  const id = ws.set().id;
  ws.editSet(id, { name: "  Gig ", description: "  loops  " });
  assert.equal(ws.set().name, "Gig");
  assert.equal(ws.set().description, "loops");
  ws.editSet(id, { name: "   " });
  assert.equal(ws.set().name, "Gig");
});

test("a set not showing is deleted for good; the one showing can't be", () => {
  const storage = memory();
  const ws = make(storage);
  ws.edit(0, "x\n");
  const first = ws.set().id;
  ws.openSet(serialise(["y\n"], 0, [], { name: "Other" }));
  assert.equal(ws.removeSet(ws.set().id), false);
  assert.equal(ws.removeSet(first), true);
  assert.equal(ws.sets().length, 1);
  assert.ok(!storage.keys().some((k) => k.includes(first)));
});

// ── changes are told ──────────────────────────────────────────────────────

test("whoever listens hears what changed: a buffer shown, a set shown, the sets, an edit", () => {
  const ws = make(), heard = [];
  ws.subscribe((e) => heard.push(e.kind));
  ws.showBuffer(1);
  ws.edit(1, "x\n");
  ws.openSet(serialise(["a\n"], 0, [], { name: "A" }));
  ws.editSet(ws.set().id, { name: "B" });
  assert.deepEqual(heard, ["buffer", "edit", "set", "sets"]);
});

test("edits are kept after a pause, not at every key: the storage is written when the save comes round", () => {
  let pending = null;
  const storage = memory();
  const ws = make(storage, { schedule: (fn) => { pending = fn; } });
  ws.edit(0, "a\n");
  ws.edit(0, "ab\n");
  assert.equal(make(memory(Object.fromEntries(storage.m))).text(0), STARTER);
  pending();
  assert.equal(make(memory(Object.fromEntries(storage.m))).text(0), "ab\n");
});

// ── from before the workspace ─────────────────────────────────────────────

test("a browser from before sets: its ten buffers become My Set, ten if 8 or 9 have code, else the default", () => {
  const legacy = { "sp-buffer-active": "3" };
  for (let i = 0; i < 10; i++) legacy[`sp-buffer:${i}`] = i === 9 ? "play 9\n" : i < 4 ? `play ${i}\n` : "";
  const ws = make(memory(legacy));
  assert.equal(ws.sets().length, 1);
  assert.equal(ws.size, 10);
  assert.equal(ws.active, 3);
  assert.equal(ws.text(9), "play 9\n");
  const small = make(memory({ "sp-buffer:0": "play 0\n" }));
  assert.equal(small.size, 8);
  assert.equal(small.text(0), "play 0\n");
});

test("a browser from before the workspace, with sets: every set carried over, names, descriptions and buffer names too, each ten", () => {
  const other = serialise(["o\n"], 0, [], { name: "Other", description: "d" });
  const legacy = {
    "sp-sets": JSON.stringify([{ id: "a", name: "Mine", used: 2, bufferNames: { 1: "bass" } }, { id: "b", name: "Other", description: "d", used: 1 }]),
    "sp-set-current": "a", "sp-set:b": other, "sp-buffer-active": "1", "sp-buffer:0": "play 0\n", "sp-buffer:1": "play 1\n",
  };
  const ws = make(memory(legacy));
  assert.deepEqual(ws.sets().map((s) => s.name), ["Mine", "Other"]);
  assert.equal(ws.set().name, "Mine");
  assert.equal(ws.size, 10);
  assert.equal(ws.active, 1);
  assert.equal(ws.text(1), "play 1\n");
  assert.equal(ws.bufferName(1), "bass");
  ws.switchSet(ws.sets().find((s) => s.name === "Other").id);
  assert.equal(ws.text(0), "o\n");
  assert.equal(ws.set().description, "d");
});

test("a new set: empty, the default size, showing, named and described as asked, apart from the rest; the one showing kept", () => {
  const ws = make();
  ws.edit(0, "mine\n");
  const was = ws.set().id;
  const r = ws.newSet({ name: "Gig", description: "tuesday" });
  assert.notEqual(ws.set().id, was);
  assert.equal(r.name, "Gig");
  assert.deepEqual([ws.set().name, ws.set().description], ["Gig", "tuesday"]);
  assert.equal(ws.size, 8);
  assert.equal(ws.active, 0);
  for (let i = 0; i < ws.size; i++) assert.equal(ws.text(i), "");
  assert.equal(ws.sets().length, 2);
  assert.equal(ws.newSet({ name: "Gig" }).name, "Gig 2");   // a name taken: numbered apart
  assert.equal(ws.newSet({ name: "  " }).name, "New Set");  // no name: the default
  ws.switchSet(was);
  assert.equal(ws.text(0), "mine\n");
});

test("a new set asked for while the set showing holds only the starter: a set added, the starter's kept", () => {
  const ws = make();
  assert.equal(ws.text(0), STARTER);
  const was = ws.set().id;
  ws.newSet();
  assert.equal(ws.sets().length, 2);
  ws.switchSet(was);
  assert.equal(ws.text(0), STARTER);
});
