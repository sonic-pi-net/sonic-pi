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

async function session(loader) {
  const runtime = await RUNTIME;
  let clock = 1000;
  const sent = [];
  const core = new LiveCore(runtime, { now: () => clock, send: (b) => sent.push({ at: clock, ...unbundle(b) }), loader });
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
