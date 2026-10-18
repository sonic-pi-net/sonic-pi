// SPDX-License-Identifier: AGPL-3.0-or-later
// web/live-core.js with the real runtime (build/runtime, scripts/build-runtime.sh --wasm) on a clock of its own, and a
// loader whose loads come in when the test says: a run with nothing playing waits for what its first sounds need and
// starts, all of it, when it has it; nothing overtakes a sound held back for a load.
import { test } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { decode } from "../../web/osc.js";
import { LiveCore, retime } from "../../web/live-core.js";
import { createRecordReader } from "../../web/gui-stream.js";
import { flacInfo } from "../../scripts/lib/flac-info.mjs";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../..");
const BUILT = path.join(ROOT, "build/runtime/sp_runtime.mjs");
const skip = fs.existsSync(BUILT) ? false : "build/runtime is not built (scripts/build-runtime.sh --wasm)";

const RUNTIME = !skip && (async () => {
  const { default: load } = await import(BUILT);
  const m = await load();
  if (m._sp_init() !== 0) throw new Error("the runtime did not boot");
  const bytes = fs.readFileSync(path.join(ROOT, "../../etc/buffers/rand-stream.wav"));
  const ptr = m._malloc(bytes.length);
  m.HEAPU8.set(bytes, ptr);
  m.ccall("sp_install_table", "number", ["string", "number", "number"], ["white", ptr, bytes.length]);
  m._free(ptr);
  const dir = path.join(ROOT, "../../etc/samples");
  m.ccall("sp_set_samples_dir", "number", ["string"], [dir]);
  const samples = fs.readdirSync(dir).filter((f) => f.endsWith(".flac")).sort();
  for (const f of samples) {
    const { rate, chans, frames } = flacInfo(fs.readFileSync(path.join(dir, f)));
    m.ccall("sp_install_sample", "number", ["string", "number", "number", "number", "number", "number"], [path.join(dir, f), frames, chans, rate, 0, 0]);
  }
  return { module: m, samples, synthdefFor: (name, fx = false) => m.ccall("sp_synthdef_for", "string", ["string", "number"], [name, fx ? 1 : 0]) };
})();

// loads that come in when come() says: everything asked for is in `asked`; `ready` names what is in already
function slowLoader(ready = new Set()) {
  const waits = new Map(), asked = new Set();
  const ask = (key) => {
    asked.add(key);
    if (!waits.has(key)) { let resolve; const p = new Promise((r) => (resolve = r)); waits.set(key, { p, resolve }); if (ready.has(key)) resolve(true); }
    return waits.get(key).p;
  };
  return {
    asked, ready,
    come(ok = true) { for (const [key, w] of waits) if (!ready.has(key)) { if (ok) ready.add(key); w.resolve(ok); } },
    synthDef: (name) => ask(name),
    synthDefReady: (name) => ready.has(name),
    loadBuffer: (bufnum) => ask(`b${bufnum}`),
    bufferReady: (bufnum) => ready.has(`b${bufnum}`),
    bufferLoaded: (bufnum) => ask(`b${bufnum}`),
    freeBuffer() {}, synthDefUrl() {},
  };
}

// a sound's bundle: its time, and its messages
function unbundle(b) {
  const v = new DataView(b.buffer, b.byteOffset, b.byteLength);
  const hi = v.getUint32(8), time = hi === 0 ? null : hi + v.getUint32(12) / 4294967296;
  const msgs = [];
  for (let at = 16; at < b.length;) { const n = v.getInt32(at); msgs.push(decode(b.subarray(at + 4, at + 4 + n))); at += 4 + n; }
  return { time, msgs };
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const cores = [];
test.afterEach(() => { for (const c of cores.splice(0)) c.stop(); });   // a failed test's loops stop too, or node waits on them

async function session(loader, records = null) {
  const runtime = await RUNTIME;
  let clock = 1000;
  const sent = [];
  const reader = createRecordReader();   // the records, read as the page reads them
  const record = records && ((b) => records.push(reader.read(decode(b))));
  const core = new LiveCore(runtime, { now: () => clock, send: (b) => sent.push({ at: clock, ...unbundle(b) }), loader, record });
  cores.push(core);
  return { core, sent, advance: (s) => { clock += s; }, now: () => clock };
}

const REREZZED = `notes = (scale :e1, :minor_pentatonic, num_octaves: 2).shuffle
live_loop :rerezzed do
  tick_reset
  t = 0.04
  sleep -t
  with_fx :bitcrusher do
    s = synth :dsaw, note: :e3, sustain: 8, note_slide: t, release: 0
    64.times do
      sleep 0.125
      control s, note: notes.tick
    end
  end
  sleep t
end
live_loop :industry do
  sample :loop_industrial, beat_stretch: 1
  sleep 1
end
live_loop :drive do
  sample :bd_haus, amp: 3
  sleep 0.5
end`;

const synths = (sent) => sent.flatMap((s) => s.msgs.filter((m) => m[0] === "/s_new").map((m) => ({ def: m[1], node: m[2], time: s.time })));

test("retime moves a bundle's timetag on, and leaves an immediate one", () => {
  const b = new Uint8Array(16); const v = new DataView(b.buffer);
  v.setUint32(8, 1000); v.setUint32(12, 2 ** 31);
  retime(b, 2.25);
  assert.equal(v.getUint32(8) + v.getUint32(12) / 2 ** 32, 1002.75);
  const now = new Uint8Array(16); new DataView(now.buffer).setUint32(12, 1);
  retime(now, 5);
  assert.deepEqual([...now.subarray(8, 16)], [0, 0, 0, 0, 0, 0, 0, 1]);
});

test("a run whose sounds wait on loads starts, all of it together, once they are in", { skip, timeout: 10000 }, async () => {
  const loader = slowLoader();
  const { core, sent, advance, now } = await session(loader);
  const start = now();
  let started = false;
  const run = core.run(REREZZED).then(() => (started = true));
  await sleep(20);
  // every loop's first sounds asked for what they need, and nothing has gone
  for (const want of ["sonic-pi-fx_bitcrusher", "sonic-pi-dsaw"]) assert.ok(loader.asked.has(want), want);
  assert.ok([...loader.asked].filter((k) => k.startsWith("b")).length >= 2, "both samples asked for");
  assert.equal(sent.filter((s) => s.time != null).length, 0, "nothing timed goes before the loads are in");
  assert.equal(started, false);
  advance(2);                   // the loads take two seconds
  loader.come();
  await run;
  const released = now();
  const s = synths(sent);
  const first = (def) => s.find((x) => x.def === def)?.time;
  // rerezzed's synth starts 0.04 earlier than the rest (its sleep -t), all of them a schedule-ahead after the release
  for (const def of ["sonic-pi-fx_bitcrusher", "sonic-pi-dsaw", "sonic-pi-stereo_player", "sonic-pi-mono_player"].filter((d) => first(d) != null)) {
    assert.ok(Math.abs(first(def) - (released + core.schedAhead)) < 0.05, `${def} at ${first(def) - released} after the release`);
  }
  assert.ok(first("sonic-pi-dsaw") != null && first("sonic-pi-fx_bitcrusher") != null);
  assert.ok(released - start >= 2);
  core.stop();
});

test("a run with everything loaded starts a schedule-ahead from now, without waiting", { skip, timeout: 10000 }, async () => {
  const loader = slowLoader(new Set(["sonic-pi-beep", "sonic-pi-mixer", "sonic-pi-basic_mixer", "sonic-pi-fx_scope_out"]));
  const { core, sent, now } = await session(loader);
  const t = now();
  await core.run("play 60");
  const s = synths(sent).find((x) => x.def === "sonic-pi-beep");
  assert.ok(s, "the beep went");
  assert.ok(Math.abs(s.time - (t + core.schedAhead)) < 1e-6);
  core.stop();
});

test("nothing overtakes a sound held back for a load: a control goes after its synth", { skip, timeout: 10000 }, async () => {
  // a synth already playing, then one whose synthdef is not in: the controls that follow it wait behind it
  const loader = slowLoader(new Set(["sonic-pi-beep", "sonic-pi-mixer", "sonic-pi-basic_mixer", "sonic-pi-fx_scope_out"]));
  const { core, sent, advance } = await session(loader);
  await core.run("live_loop :keep do\n  play 60\n  sleep 1\nend");
  advance(0.2);
  await core.run("s = synth :dsaw, note: 50, sustain: 4\nsleep 0.01\ncontrol s, note: 52\nsleep 0.01\ncontrol s, note: 54");
  for (let i = 0; i < 5; i++) { advance(0.01); core.tick(); }
  const beforeLoad = sent.flatMap((s) => s.msgs).filter((m) => m[0] === "/n_set" || (m[0] === "/s_new" && m[1] === "sonic-pi-dsaw"));
  assert.deepEqual(beforeLoad, [], "the controls wait behind the synth they control");
  loader.come();
  await sleep(10);
  const order = sent.flatMap((s) => s.msgs).filter((m) => m[0] === "/n_set" || (m[0] === "/s_new" && m[1] === "sonic-pi-dsaw")).map((m) => m[0]);
  assert.deepEqual(order.slice(0, 3), ["/s_new", "/n_set", "/n_set"]);
  core.stop();
});

test("a Run after the clock has moved on with nothing playing starts now, not held back by the move", { skip, timeout: 10000 }, async () => {
  // A phone, 2026-09-28: the audio away for six seconds, the engine rebuilt on the tap, and its clock six seconds on.
  // The rebuilt engine's first word (its controllers) is a cue, which ticks the idle core on the clock from before;
  // the Run after it took the move for time lost, and started six seconds late.
  const loader = slowLoader(new Set(["sonic-pi-beep", "sonic-pi-mixer", "sonic-pi-basic_mixer", "sonic-pi-fx_scope_out"]));
  const { core, sent, advance, now } = await session(loader);
  await core.run("play 60");
  core.stop();
  core.cue("/gamepad/devices", []);
  advance(6);
  const t = now();
  await core.run("live_loop :a do\n  play 62\n  sleep 0.25\nend");
  const s = synths(sent).filter((x) => x.def === "sonic-pi-beep" && x.time > t);
  assert.ok(s.length, "the loop's beep went");
  assert.ok(Math.abs(s[0].time - (t + core.schedAhead)) < 1e-6, `the loop's first beep ${(s[0].time - t).toFixed(3)} s after the Run`);
  core.stop();
});

// ── Flashes: the lines the runtime lights as their sounds play, as native's __delayed_flash ──
// The runtime says which lines flash, as native's does: a sound's, a control's and a MIDI message's line; a with_fx's
// header and its end once, as it starts; a live_loop's header and its end each time round. Not a kill, a cue, a puts,
// a set or a sound that is off. The page lights what it is told (main.js onRecord), the editor and a card alike
// (#3587). A block's end comes from the compiler, patched to do as CRuby does (runtime/mruby-patches).

// a program run and played on for `seconds`, the clock ticked as the page ticks it, everything it needs loaded
async function played(code, seconds) {
  const records = [];
  const { core, advance } = await session(slowLoader({ has: () => true, add() {} }), records);
  await core.run(code);
  for (let t = 0; t < seconds; t += 0.05) { advance(0.05); core.tick(); }
  core.stop();
  const errors = records.filter((r) => r.kind === "error");
  assert.deepEqual(errors, [], "the program ran");
  return records;
}
const flashLines = (records) => records.filter((r) => r.kind === "flash").map((r) => r.line);
// each moment something flashed, and the lines that flashed then, in order
const flashesByTime = (records) => {
  const at = new Map();
  for (const r of records.filter((x) => x.kind === "flash")) at.set(r.time, [...(at.get(r.time) ?? []), r.line].sort((a, b) => a - b));
  return [...at.values()];
};

test("a sound flashes its line as it plays, a chord once", { skip, timeout: 10000 }, async () => {
  const records = await played("play 60\nsleep 0.5\nsample :bd_haus\nsleep 0.5\nsynth :dsaw, note: 50\nsleep 0.5\nplay chord(:e3, :minor)", 2);
  assert.deepEqual(flashLines(records), [1, 3, 5, 7]);
  // each at the moment its sound plays
  for (const f of records.filter((r) => r.kind === "flash")) {
    assert.ok(records.some((r) => r.kind === "synth" && r.line === f.line && r.time === f.time), `line ${f.line} flashes with its sound`);
  }
});

test("a live_loop flashes its header and its end each time round, with its sound", { skip, timeout: 10000 }, async () => {
  const records = await played("live_loop :a do\n  play 60\n  sleep 1\nend", 2.5);
  const rounds = flashesByTime(records);
  assert.ok(rounds.length >= 2, `${rounds.length} rounds`);
  for (const lines of rounds) assert.deepEqual(lines, [1, 2, 4]);   // the scope_out every loop plays through is the runtime's, not a line's
});

test("a with_fx flashes its header and its end once, as it starts", { skip, timeout: 10000 }, async () => {
  const records = await played("with_fx :reverb do\n  play 60\n  sleep 1\n  play 62\nend", 2);
  assert.deepEqual(flashLines(records), [1, 5, 2, 4]);
});

test("a control and a MIDI message flash their lines; a kill, a cue, a puts, a set and a sound that is off do not", { skip, timeout: 10000 }, async () => {
  const records = await played([
    "s = synth :dsaw, note: 50, sustain: 4",
    "sleep 0.25",
    "control s, note: 52",
    "sleep 0.25",
    "kill s",
    "cue :x",
    "puts 1",
    "play 60, on: false",
    "midi_note_on 60",
    "set :y, 1",
  ].join("\n"), 1);
  assert.deepEqual(flashLines(records), [1, 3, 9]);
});

test("blocks inside each other light their own ends; a block on one line has no end of its own to light", { skip, timeout: 10000 }, async () => {
  const nested = await played("live_loop :a do\n  with_fx :reverb do\n    play 60\n    sleep 1\n  end\nend", 2.5);
  const rounds = flashesByTime(nested);
  assert.ok(rounds.length >= 2, `${rounds.length} rounds`);
  for (const lines of rounds) assert.deepEqual(lines, [1, 2, 3, 5, 6]);
  const oneLine = await played("live_loop :b do play 60; sleep 1 end", 1.5);
  assert.ok(flashLines(oneLine).length && flashLines(oneLine).every((n) => n === 1), String(flashLines(oneLine)));
});
