#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// Judges RT mode: every spec run as a live job with a simulated clock that
// jumps to each wake time, its emitted records gathered and compared with
// the oracle's recording exactly as check.rb compares a trace. RT and NRT
// must agree on every spec, since they are one scheduler. And the audio
// stream (the OSC the page hands the engine) must be those records.
//
//   node runtime/bin/live-check.mjs [specs/dir ...]
//   SP_RUNTIME=path/to/sp_runtime.mjs node runtime/bin/live-check.mjs   (another build)
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { flacInfo } from "../../scripts/lib/flac-info.mjs";
import { decode, forEachFrame, FRAME_HOST, FRAME_GUI } from "../../web/osc.js";
import { createRecordReader } from "../../web/gui-stream.js";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../..");
const { default: load } = await import(path.resolve(process.env.SP_RUNTIME ?? path.join(ROOT, "build/runtime/sp_runtime.mjs")));
const m = await load();
if (m._sp_init() !== 0) process.exit(1);
const TABLES = { white: "rand-stream.wav", pink: "rand-stream-pink.wav", light_pink: "rand-stream-light-pink.wav", dark_pink: "rand-stream-dark-pink.wav", perlin: "rand-stream-perlin.wav" };
for (const [source, file] of Object.entries(TABLES)) {
  const bytes = fs.readFileSync(path.join(ROOT, "../../etc/buffers", file));
  const ptr = m._malloc(bytes.length); m.HEAPU8.set(bytes, ptr);
  m.ccall("sp_install_table", "number", ["string", "number", "number"], [source, ptr, bytes.length]); m._free(ptr);
}
const samplesDir = path.join(ROOT, "../../etc/samples");
m.ccall("sp_set_samples_dir", "number", ["string"], [samplesDir]);
for (const f of fs.readdirSync(samplesDir).sort()) {
  if (!f.endsWith(".flac")) continue;
  const { rate, chans, frames } = flacInfo(fs.readFileSync(path.join(samplesDir, f)));
  m.ccall("sp_install_sample", "number", ["string", "number", "number", "number", "number", "number"], [path.join(samplesDir, f), frames, chans, rate, 0, 0]);
}

let records = [];
const reader = createRecordReader();

// The outbox after each call: records for the page (the GUI stream, read back
// by web/gui-stream.js) and the audio stream (web/osc.js).
// It must be the records, as the engine's OSC: every synth, control and kill
// the page plays, in order, at its time, with its values, its synthdef and
// sample buffer announced before it. The numbering outlives a session, as
// the page's does.
let audio = [];
let drainAt = null;   // the clock at this drain: when an immediate message reaches the engine (engineTree)
const drain = () => {
  const len = m._sp_out_len();
  if (!len) return;
  const heap = m.HEAPU8;
  forEachFrame(heap, m._sp_out_ptr(), len, (kind, start, size) => {
    if (kind === FRAME_GUI) records.push(reader.read(decode(heap.subarray(start, start + size))));
    else audio.push({ kind, msg: decode(heap.slice(start, start + size)), at: drainAt });
  });
};
const NODE_BASE = 10000;
const STUDIO = new Set([1000, 1001, 1002, 1003]);   // the studio's groups and sonic-pi-mixer (Scheduler::STUDIO_SYNTHS …), made once
const synthdefs = new Map(), buffers = new Map();
let soundsHeard = 0;
function audioDiff(records, frames) {
  const sounds = [];
  for (const { kind, msg } of frames) {
    if (kind === FRAME_HOST) {
      const [address, n, name] = msg;
      if (address === "/sonic-pi/synthdef") synthdefs.set(n, name);
      else if (address === "/sonic-pi/sample") buffers.set(n, name);
      else if (address === "/sonic-pi/sample_free") buffers.delete(n);
      continue;
    }
    const [address, def, buf, bytes] = msg;
    const bundle = decode(bytes);
    sounds.push({ address, def: def < 0 ? null : synthdefs.get(def) ?? `unannounced ${def}`, buf, file: buf < 0 ? null : buffers.get(buf) ?? `unannounced ${buf}`, time: bundle.timeTag, packet: bundle.packets[0] });
  }
  // with_fx's own node tree is no record's: its groups, a group freed, a
  // live loop's fx moved to another block (re-routed and reordered); nor is the
  // studio, nor a run's place in the engine (its group and its basic_mixer)
  const groups = new Set(sounds.filter((s) => s.packet[0] === "/g_new").map((s) => s.packet[1]));
  const isMixer = (s) => s.packet[0] === "/s_new" && (s.packet[1] === "sonic-pi-mixer" || s.packet[1] === "sonic-pi-basic_mixer");
  const studio = (s) => {
    const [address, id, ...rest] = s.packet;
    return address === "/g_new" || address === "/n_order" || address === "/g_freeAll" || (address === "/n_free" && groups.has(id))
      || (address === "/n_set" && rest.length === 2 && rest[0] === "out_bus")
      || isMixer(s);
  };
  // the studio is made before the engine's first sound, once: its groups and sonic-pi-mixer; and each run that
  // sounds has its own basic_mixer, at the tail of its group, after the group its sounds go in
  const mixers = sounds.filter(isMixer).map((s) => s.packet[1]);
  const studioProblems = sounds.length && !(mixers.filter((m) => m === "sonic-pi-mixer").length === 1 && mixers.includes("sonic-pi-basic_mixer"))
    ? [`the studio's mixer, and a run's, were not made once each (${JSON.stringify(mixers)})`] : [];
  const runGroups = new Set(sounds.filter((s) => s.packet[1] === "sonic-pi-basic_mixer").map((s) => s.packet[4]));
  const runSynths = new Set(sounds.filter((s) => s.packet[0] === "/g_new" && s.packet[2] === 0 && runGroups.has(s.packet[3])).map((s) => s.packet[1]));
  // a live loop's own fx handed over to another block (Scheduler#handover_fx): a second
  // scope_out made silent, the two crossfaded by amp slides, then the old one freed
  const handedOver = new Set();
  for (const s of sounds) { const [a, id, ...rest] = s.packet; if (a === "/n_set" && rest.length === 4 && rest[0] === "amp" && rest[1] === 0 && rest[2] === "amp_slide") handedOver.add(id); }
  const handover = (s) => {
    const [address, id, ...rest] = s.packet;
    return (address === "/s_new" && id === "sonic-pi-fx_scope_out" && rest.includes("amp") && rest[rest.indexOf("amp") + 1] === 0)
      || (address === "/n_set" && rest.length === 4 && rest[0] === "amp" && rest[2] === "amp_slide")
      || (address === "/n_free" && handedOver.has(id));
  };
  sounds.splice(0, sounds.length, ...sounds.filter((s) => !studio(s) && !handover(s)));
  // the buffers a sound names by their files, a player's sample (buf) and the random stream a synth tosses its coins
  // with (rand_buf): either goes as the number of the buffer the sound waits on (Scheduler#audio_buffer)
  const BUFS = new Set(["buf", "rand_buf"]);
  const opts = (args) => Object.entries(args || {}).filter(([k, v]) => (BUFS.has(k) ? typeof v === "string" : typeof v === "number"));
  const want = records.filter((r) => ["synth", "control", "kill"].includes(r.kind) && (r.kind !== "control" || opts(r.args).length));
  const problems = [...studioProblems];
  soundsHeard += sounds.length;
  if (want.length !== sounds.length) problems.push(`${want.length} sounds recorded, ${sounds.length} in the audio stream`);
  for (let i = 0; i < Math.min(want.length, sounds.length) && problems.length < 4; i++) {
    const r = want[i], s = sounds[i], [address, ...args] = s.packet;
    const say = (what) => problems.push(`sound ${i}, ${r.kind} ${r.synth} at ${r.time}: ${what}`);
    const id = NODE_BASE + r.node;
    if (s.address !== "/sonic-pi/sound") say(`framed as ${s.address}`);
    // an fx that starts now goes immediately (OSC's timetag 1, Scheduler::IMMEDIATE), as Sonic Pi sends it, and so
    // does a real-time thread's every sound (Scheduler#audio_time): its record says so (immediate)
    if (r.now || r.immediate ? s.time >= 1 : Math.abs(s.time - r.time) > 1e-9) say(`time ${s.time}`);
    if (r.kind === "synth") {
      const where = (args[2] === 0 && runSynths.has(args[3])) || (r.fx && args[2] === 1) || (r.synth.startsWith("sonic-pi-fx_") && args[2] === 1);
      if (address !== "/s_new" || args[0] !== r.synth || args[1] !== id || !where) say(`message ${JSON.stringify(s.packet)}`);
      if (s.def !== r.synth) say(`needs synthdef ${s.def}`);
    } else if (r.kind === "control") {
      if (address !== "/n_set" || args[0] !== id || s.def !== null) say(`message ${JSON.stringify(s.packet)}`);
    } else if (address !== "/n_free" || args[0] !== id || args.length !== 1 || s.def !== null || s.buf !== -1) {
      say(`message ${JSON.stringify(s.packet)}`);
    }
    if (r.kind === "kill") continue;
    const got = new Map();
    const pairs = r.kind === "synth" ? args.slice(4) : args.slice(1);
    for (let j = 0; j + 1 < pairs.length; j += 2) if (!String(pairs[j]).endsWith("_bus")) got.set(pairs[j], pairs[j + 1]);   // busses are the studio's
    const expect = opts(r.args);
    if (got.size !== expect.length) say(`${got.size} opts sent, ${expect.length} recorded`);
    for (const [k, v] of expect) {
      if (BUFS.has(k)) { if (s.file !== v || got.get(k) !== s.buf) say(`${k} ${got.get(k)} holds ${s.file}, recorded ${v}`); }
      else if (got.get(k) !== (Number.isInteger(v) ? v : Math.fround(v))) say(`${k} ${got.get(k)}, recorded ${v}`);
    }
  }
  return problems;
}

const targets = process.argv.length > 2 ? process.argv.slice(2) : [path.join(ROOT, "specs")];
const specs = targets.flatMap((t) => fs.statSync(t).isDirectory()
  ? fs.readdirSync(t, { recursive: true }).filter((f) => f.endsWith(".rb")).map((f) => path.join(t, f)) : [t]).sort();
const STRICT = ["events", "errors", "output"];
// the default schedule-ahead, from the one place it is said (runtime/lib/sonic_pi/defaults.rb)
const DEFAULT_SCHED_AHEAD = Number(/DEFAULT_SCHED_AHEAD\s*=\s*([\d.]+)/.exec(fs.readFileSync(path.join(ROOT, "runtime/lib/sonic_pi/defaults.rb"), "utf8"))[1]);
const round6 = (x) => Math.round(x * 1e6) / 1e6;
// A live record says its thread's own time (the log shows it); a trace says what is heard in the logical frame: when
// it sounds, less the job's start and the default schedule-ahead, as the oracle records it (scheduler.rb trace_t). A
// thread with a schedule-ahead of its own is where the two differ. What a record names (a control's synth, a sound's
// fx) moves with the record it names: by node, or an fx by its trigger's time, thread and synth.
function inTraceFrame(records, start) {
  const heard = new Set(["synth", "control", "kill", "midi"]);
  const byNode = new Map(), byTrigger = new Map();
  const key = (t, thread, synth) => `${t}|${thread}|${synth}`;
  for (const r of records) {
    if (r.kind !== "synth" || r.time == null) continue;
    const t = round6(r.time - start - DEFAULT_SCHED_AHEAD);
    if (r.node != null) byNode.set(r.node, t);
    byTrigger.set(key(r.t, r.thread, r.synth), t);
  }
  const ref = (o) => (o ? { ...o, t: byTrigger.get(key(o.t, o.thread, o.synth)) ?? o.t } : o);
  return records.map((r) => {
    if (!heard.has(r.kind) || r.time == null) return r;
    const out = { ...r, t: round6(r.time - start - DEFAULT_SCHED_AHEAD) };
    if (r.fx) out.fx = ref(r.fx);
    if (r.of && (r.kind === "control" || r.kind === "kill")) out.of = { ...r.of, t: byNode.get(r.node) ?? r.of.t };
    return out;
  });
}
const ordered = (items) => items.map((e, i) => [e, i]).sort(([a, i], [b, j]) => a.t - b.t || String(a.thread).localeCompare(String(b.thread)) || i - j).map(([e]) => e);
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
// as check.rb: an fx's free matches within a tolerance, the oracle's wall clock being late
const FREE_TOLERANCE = 0.03;
const sameItem = (x, y) => {
  if (x?.kind !== "fx_free" || y?.kind !== "fx_free") return same(x, y);
  const { t: a, ...xr } = x, { t: b, ...yr } = y;
  return Math.abs(a - b) <= FREE_TOLERANCE && same(xr, yr);
};
const sameList = (xs, ys) => xs.length === ys.length && xs.every((x) => ys.some((y) => sameItem(x, y))) && ys.every((y) => xs.some((x) => sameItem(x, y)));
let pass = 0, fail = 0, logDiffers = 0;
for (const spec of specs) {
  const expPath = spec.replace(/\.rb$/, ".expected.json");
  if (!fs.existsSync(expPath)) continue;
  const expected = JSON.parse(fs.readFileSync(expPath, "utf8"));
  records = [];
  audio = [];
  m._sp_live_boot();
  const src = fs.readFileSync(spec, "utf8");
  let horizon = -1;                                  // a spec's `# horizon: N`, as the trace honours it
  for (const line of src.split("\n")) {
    if (!line.startsWith("#")) break;
    if (line.startsWith("# horizon:")) horizon = parseFloat(line.slice(10));
  }
  m._sp_live_stop_after(horizon);
  const START = 100.0;
  let now = START;
  m.ccall("sp_run", "number", ["string", "number"], [src, now]);
  drain();
  for (let i = 0; i < 100000; i++) {
    const next = m._sp_tick(now);
    drain();
    if (next < 0 || now - START > 60) break;
    now = Math.max(now, next);
  }
  m._sp_stop_all();
  drain();
  // a live record also says its node and the line it came from; an error's line is part of the trace. An opt that
  // broke a rule carries the rule itself (fault), for the error card to build on: that is the page's, not the
  // trace's, and a run without a card is no different for it.
  const strip = (r) => { const { kind, time, job, node, fault, ...rest } = r; if (kind !== "error") delete rest.line; return rest; };
  const actual = {
    events: ordered(inTraceFrame(records, START).filter((r) => ["synth", "sample_load", "control", "kill", "midi", "fx_free", "loop_move"].includes(r.kind)).map((r) => { const { time, job, node, line, loopUid, parentUid, immediate, ...rest } = r; return rest; })),
    // a program the preparser refuses: live, its error names the refused line for the error card to point at
    // (scheduler.rb record_error); the trace, as native, names no line and no thread but the run's
    errors: records.filter((r) => r.kind === "error").map(strip).map((e) => (e.class === "SonicPi::PreParser::PreParseError" ? { ...e, line: -1, thread: "0" } : e)).sort((a, b) => a.line - b.line || String(a.thread).localeCompare(b.thread) || a.class.localeCompare(b.class)),
    output: ordered(records.filter((r) => r.kind === "output").map(strip)),
    log: ordered(records.filter((r) => r.kind === "log").map(strip)),
  };
  const hard = STRICT.filter((k) => (k === "events" ? !sameList(expected[k], actual[k]) : !same(expected[k], actual[k])));
  const heard = audioDiff(records, audio);
  const name = path.relative(ROOT, spec);
  if (hard.length || heard.length) {
    fail++;
    console.log(`FAIL ${name}: ${[...hard, ...(heard.length ? ["audio"] : [])].join(", ")} differ`);
    for (const h of heard) console.log("    audio: " + h);
    for (const k of hard) {
      for (const x of expected[k]) if (!actual[k].some((y) => sameItem(x, y))) console.log("    - " + JSON.stringify(x));
      for (const y of actual[k]) if (!expected[k].some((x) => sameItem(x, y))) console.log("    + " + JSON.stringify(y));
    }
  } else {
    pass++;
    if (!same(expected.log, actual.log)) logDiffers++;
  }
}
// Runs are jobs on one clock: a sync in a new run must see the cues from now
// on, not race through the cues a stopped run left behind.
{
  records = [];
  m._sp_live_boot();
  m._sp_live_stop_after(-1);
  let now = 500;
  let worst = 0;
  const tickFor = (seconds) => {
    const until = now + seconds;
    for (let i = 0; i < 100000 && now < until; i++) {
      const t0 = performance.now();
      const next = m._sp_tick(now);
      worst = Math.max(worst, performance.now() - t0);
      drain();
      now = next < 0 ? until : Math.max(now, Math.min(next, until));
      if (next >= until || next < 0) now = until;
    }
  };
  const clock = "live_loop :clock do\n  cue :tick\n  sleep 0.5\nend\n";
  m.ccall("sp_run", "number", ["string", "number"], [clock, now]);
  drain();
  tickFor(20);
  m._sp_stop_all();
  records = [];
  worst = 0;
  m.ccall("sp_run", "number", ["string", "number"], [clock + "live_loop :bells do\n  sync :tick\n  play 60\nend\n", now]);
  drain();
  tickFor(2);
  m._sp_stop_all();
  const plays = records.filter((r) => r.kind === "synth" && r.synth === "sonic-pi-beep").length;
  const ok = plays >= 3 && plays <= 6 && worst < 50;
  console.log(`${ok ? "pass" : "FAIL"} a sync in a new run waits for new cues: ${plays} plays in 2 s, worst tick ${worst.toFixed(1)} ms`);
  if (ok) pass++; else fail++;
}
// A second Run adds a loop that syncs on one the first Run is playing
// (`live_loop :foo` running, then Run again with `live_loop :bar, sync: :foo`).
// The clock keeps moving, both loops keep playing, and :bar lands on :foo's
// beat grid.
{
  records = [];
  audio = [];
  m._sp_live_boot();
  m._sp_live_stop_after(-1);
  let now = 700, calls = 0;
  const tickUntil = (until) => {
    for (let i = 0; i < 100000 && now < until; i++) {
      calls++;
      const next = m._sp_tick(now);
      drain();
      now = next < 0 ? until : Math.min(Math.max(now, next), until);
    }
  };
  const foo = "live_loop :foo do\n  sample :loop_amen, onset: pick\n  sleep 0.125\nend\n";
  m.ccall("sp_run", "number", ["string", "number"], [foo, now]);
  drain();
  tickUntil(now + 2.3);
  const second = now;
  calls = 0;
  m.ccall("sp_run", "number", ["string", "number"], [foo + "\nlive_loop :bar, sync: :foo do\n  sample :bd_haus, lpf: 70\n  sleep 0.5\nend\n", now]);
  drain();
  tickUntil(second + 4);
  m._sp_stop_all();
  drain();
  const sounded = (name) => records.filter((r) => r.kind === "synth" && r.name === name && r.time > second + 1).map((r) => r.time);
  const foos = sounded("live_loop_foo"), bars = sounded("live_loop_bar");
  const problems = [];
  if (calls > 2000) problems.push(`the host was called ${calls} times in 4 s: the clock stopped`);
  if (foos.length < 20) problems.push(`:foo played ${foos.length} times in the last 3 s`);
  if (bars.length < 5) problems.push(`:bar played ${bars.length} times in the last 3 s`);
  const offGrid = bars.filter((t) => !foos.some((f) => Math.abs(f - t) < 1e-6));
  if (offGrid.length) problems.push(`:bar off :foo's beats at ${offGrid.map((t) => (t - second).toFixed(3)).join(" ")}`);
  const ok = problems.length === 0;
  console.log(`${ok ? "pass" : "FAIL"} a second Run's loop syncs on the first Run's and both play on one beat grid${ok ? ` (${foos.length} foo, ${bars.length} bar, ${calls} ticks)` : ": " + problems.join("; ")}`);
  if (ok) pass++; else fail++;
}
// Link's tempo from the page, mid-run: a loop sleeping a beat at a time
// carries on from its beat at the new tempo. Its sounds never go back in time
// and no gap is lost or doubled: 1 s apart at 60 bpm, then 0.5 s at 120.
{
  records = [];
  audio = [];
  m._sp_live_boot();
  m._sp_live_stop_after(-1);
  let now = 800;
  const tickUntil = (until) => {
    for (let i = 0; i < 100000 && now < until; i++) {
      const next = m._sp_tick(now);
      drain();
      now = next < 0 ? until : Math.min(Math.max(now, next), until);
    }
  };
  const start = now;
  m.ccall("sp_run", "number", ["string", "number"], ["live_loop :t do\n  play 60, release: 0.1\n  sleep 1\nend\n", now]);
  drain();
  tickUntil(start + 3.2);
  m._sp_set_link_bpm(120, now + 0.5);                     // as LiveSession#setLinkBpm: a schedule-ahead from now
  tickUntil(start + 6.2);
  m._sp_stop_all();
  drain();
  const times = records.filter((r) => r.kind === "synth" && r.synth === "sonic-pi-beep").map((r) => r.time - start);
  const gaps = times.slice(1).map((t, i) => t - times[i]);
  const problems = [];
  if (gaps.some((g) => g <= 0)) problems.push(`a sound went back in time: ${times.map((t) => t.toFixed(3)).join(" ")}`);
  if (Math.abs(gaps[0] - 1) > 1e-6 || Math.abs(gaps[1] - 1) > 1e-6) problems.push(`before the change not 1 s apart: ${gaps.map((g) => g.toFixed(3)).join(" ")}`);
  const after = gaps.slice(-3);
  if (after.length < 3 || after.some((g) => Math.abs(g - 0.5) > 1e-6)) problems.push(`after the change not 0.5 s apart: ${gaps.map((g) => g.toFixed(3)).join(" ")}`);
  const ok = problems.length === 0;
  console.log(`${ok ? "pass" : "FAIL"} Link's tempo changed mid-run: a loop carries on from its beat at the new tempo${ok ? ` (gaps ${gaps.map((g) => g.toFixed(2)).join(" ")})` : ": " + problems.join("; ")}`);
  if (ok) pass++; else fail++;
}
// The global time warp: every sound, and its record, that much later.
{
  const soundTimes = (warpMs) => {
    records = [];
    audio = [];
    m._sp_live_boot();
    m._sp_live_stop_after(-1);
    m._sp_set_time_warp(warpMs / 1000);
    const at = 900;
    m.ccall("sp_run", "number", ["string", "number"], ["play 60\nsleep 0.5\nsample :bd_haus\n", at]);
    drain();
    let now = at;
    for (let i = 0; i < 1000; i++) { const next = m._sp_tick(now); drain(); if (next < 0) break; now = Math.max(now, next); }
    m._sp_set_time_warp(0);
    const bundles = audio.filter((a) => a.kind !== FRAME_HOST).map((a) => decode(a.msg[3])).filter((b) => b.packets[0][0] === "/s_new" && b.packets[0][2] >= NODE_BASE && b.packets[0][1] !== "sonic-pi-basic_mixer").map((b) => b.timeTag);
    return { bundles, records: records.filter((r) => r.kind === "synth").map((r) => r.time) };
  };
  const plain = soundTimes(0), warped = soundTimes(250);
  const shift = (a, b) => b.map((t, i) => t - a[i]);
  const ok = plain.bundles.length === 2 && warped.bundles.length === 2
    && shift(plain.bundles, warped.bundles).every((d) => Math.abs(d - 0.25) < 1e-6)
    && shift(plain.records, warped.records).every((d) => Math.abs(d - 0.25) < 1e-6);
  console.log(`${ok ? "pass" : "FAIL"} the global time warp moves every sound and its record by its amount${ok ? "" : `: ${JSON.stringify({ plain, warped })}`}`);
  if (ok) pass++; else fail++;
}
// A live loop run again from another run's with_fx moves into it: the fx it
// leaves (the first run's) is freed after the move plus its kill_delay and the
// schedule-ahead, on the session's clock, never before. And the engine is never asked to move
// or play into what it has freed.
{
  records = [];
  audio = [];
  m._sp_live_boot();
  m._sp_live_stop_after(-1);
  let now = 700;
  const tickUntil = (until) => {
    for (let i = 0; i < 100000 && now < until; i++) {
      const next = m._sp_tick(now);
      drain();
      now = next < 0 ? until : Math.min(Math.max(now, next), until);
    }
  };
  m.ccall("sp_run", "number", ["string", "number"], ["with_fx :lpf do\n  live_loop :d do\n    play 60, release: 0.1\n    sleep 0.5\n  end\nend\n", now]);
  drain();
  tickUntil(now + 3);
  const moveAt = now;
  m.ccall("sp_run", "number", ["string", "number"], ["with_fx :level do\n  live_loop :d do\n    play 62, release: 0.1\n    sleep 0.5\n  end\nend\n", now]);
  drain();
  tickUntil(now + 4);
  m._sp_stop_all();
  drain();
  const freed = records.filter((r) => r.kind === "fx_free" && r.synth === "sonic-pi-fx_lpf");
  const problems = [];
  if (freed.length !== 1) problems.push(`${freed.length} frees of the lpf`);
  else if (freed[0].time < moveAt + 1 + DEFAULT_SCHED_AHEAD - 1e-6) problems.push(`the lpf freed at ${(freed[0].time - moveAt).toFixed(3)}s after the move`);
  const gone = new Set();
  for (const { kind, msg } of audio) {
    if (kind === FRAME_HOST) continue;
    const [address, , , bytes] = msg;
    const packet = decode(bytes).packets[0];
    if (packet[0] === "/n_free") gone.add(packet[1]);
    else if (packet[0] === "/n_order" && (gone.has(packet[2]) || gone.has(packet[3]))) problems.push(`moves into or out of freed node ${packet[2]}/${packet[3]}`);
    else if (packet[0] === "/s_new" && packet[3] === 1 && gone.has(packet[4])) problems.push(`plays into freed group ${packet[4]}`);
  }
  const ok = problems.length === 0;
  console.log(`${ok ? "pass" : "FAIL"} a live loop moved into another run's with_fx keeps sounding, and its old fx goes after the move${ok ? "" : ": " + problems.slice(0, 4).join("; ")}`);
  if (ok) pass++; else fail++;
}
// ── Groups: the unit the GUI plays and stops by (Scheduler#stop_group). The cards' runs are one group, a buffer's
// another. Each scenario drives the runtime as the page does — sp_run_group, ticks, sp_stop_group — and reads the
// process table (a group column, the 15th) and the OSC the engine gets.
{
  const FIELDS = 15, GROUP = 14, KIND = 3, STATE = 4;
  const table = (now) => { const ptr = m._sp_process_table(now), n = m._sp_process_table_len(); return Array.from(m.HEAPF64.subarray(ptr / 8, ptr / 8 + n)); };
  // a group is live while a thread of it runs, sleeps or waits, an fx of it is open, or a sound of it sounds
  const live = (now, g) => { const t = table(now); const out = []; for (let i = 0; i + FIELDS <= t.length; i += FIELDS) { if (t[i + GROUP] !== g) continue; const k = t[i + KIND], st = t[i + STATE]; if ((k >= 1 && k <= 5 && st <= 2) || (k === 6 && st < 3) || ((k === 7 || k === 8) && st === 0)) out.push(k); } return out; };
  // the group's own row: live (0) while anything of it or under it is; a run's row hangs from it
  const groupRow = (now, g) => { const t = table(now); for (let i = 0; i + FIELDS <= t.length; i += FIELDS) if (t[i + KIND] === 9 && t[i + GROUP] === g) return { uid: t[i], parent: t[i + 1], state: t[i + STATE] }; return null; };
  const run = (code, now, g) => { drainAt = now; const job = m.ccall("sp_run_group", "number", ["string", "number", "number"], [code, now, g]); drain(); return job; };
  const osc = () => audio.map(({ msg }) => { const [, , , bytes] = msg; const b = decode(bytes); const pk = b.packets?.[0] ?? b; return { t: b.timeTag, msg: pk }; });
  const scenario = (name, body) => { records = []; audio = []; m._sp_live_boot(); m._sp_live_stop_after(-1); let now = 1000; const tickUntil = (until) => { for (let i = 0; i < 100000 && now < until; i++) { drainAt = now; const next = m._sp_tick(now); drain(); now = next < 0 ? until : Math.min(Math.max(now, next), until); } }; const problems = []; body({ run, tickUntil, get now() { return now; }, set now(v) { now = v; }, live, osc, problems, stop: (g, fade) => { drainAt = now; m._sp_stop_group(g, fade, now); drain(); }, stopRun: (job, fade) => { if (!m._sp_stop_run) { problems.push("the runtime has no sp_stop_run"); return; } drainAt = now; m._sp_stop_run(job, fade, now); drain(); } }); m._sp_stop_all(); drain(); const ok = problems.length === 0; console.log(`${ok ? "pass" : "FAIL"} ${name}${ok ? "" : ": " + problems.join("; ")}`); if (ok) pass++; else fail++; };
  const CARD = (code, slot = 1) => `with_fx :scope_out, scope_num: ${slot} do\n${code}\nend\n`;
  const LOOP = CARD("live_loop :flibble do\n  sample :bd_haus\n  sleep 0.5\nend");

  // A knob on a MIDI controller is a cue from outside, arriving between ticks, dozens a second. Each one wakes a
  // sync and plays a note: one message to the engine per cue, and nothing said twice. The outbox is what makes
  // this delicate — it empties at the start of every call that fills it (sp_host.c), so a cue that records
  // itself must clear it too, or the last tick's sounds are sent a second time and scsynth refuses them as
  // duplicate node IDs. That is heard as notes dropping out, and nothing else here would catch it.
  scenario("a cue from outside plays one note, once, however fast they come", (t) => {
    t.run(CARD('live_loop :knob do\n  use_real_time\n  n, v = sync "/midi:t:1/cc"\n  play v, release: 0.05\nend'), t.now, 1);
    t.tickUntil(t.now + 0.5);
    const before = t.osc().length;
    for (let i = 0; i < 60; i++) {
      m.ccall("sp_cue", null, ["string", "string", "number"], ["/midi:t:1/cc", `i5\x1fi${60 + (i % 12)}`, t.now]);
      drain();                       // as LiveCore.cue does: the cue's own record, before the tick refills
      t.now += 0.011;                // a controller's rate
      t.tickUntil(t.now);
    }
    t.tickUntil(t.now + 0.2);
    const news = t.osc().slice(before).filter((x) => x.msg[0] === "/s_new");
    const ids = news.map((x) => x.msg[2]);   // /s_new name, nodeID, addAction, target
    const dupes = ids.length - new Set(ids).size;
    if (news.length !== 60) t.problems.push(`60 cues should play 60 notes: ${news.length} /s_new`);
    if (dupes) t.problems.push(`${dupes} sounds sent twice: scsynth refuses those as duplicate node IDs`);
  });

  scenario("a group stops: its loop, its fx and its sounds go, the others' stay", (t) => {
    t.run(LOOP, t.now, 1);
    t.run(CARD("live_loop :other do\n  play 60, release: 0.1\n  sleep 0.5\nend", 2), t.now, 2);
    t.tickUntil(t.now + 2);
    if (!t.live(t.now, 1).length || !t.live(t.now, 2).length) t.problems.push("both groups should be live before the stop");
    t.stop(1, 0);
    t.tickUntil(t.now + 1.5);
    if (t.live(t.now, 1).length) t.problems.push(`group 1 still live: kinds ${t.live(t.now, 1)}`);
    if (!t.live(t.now, 2).length) t.problems.push("group 2 should still be live");
    const hits = records.filter((r) => r.kind === "synth" && r.name === "live_loop_flibble" && r.time > t.now - 1);
    if (hits.length) t.problems.push(`:flibble still triggered ${hits.length} times after the stop`);
  });

  scenario("a second run of the group redefines its loop, and the group's stop still reaches it", (t) => {
    t.run(LOOP, t.now, 1);
    t.tickUntil(t.now + 2);
    t.run(LOOP.replace("sleep 0.5", "sleep 0.25"), t.now, 1);   // the card's Play again: same group
    t.tickUntil(t.now + 2);
    const before = records.filter((r) => r.kind === "synth" && r.name === "live_loop_flibble" && r.time > t.now - 1).length;
    if (before < 3) t.problems.push(`the redefined loop should be playing faster: ${before} hits in the last second`);
    t.stop(1, 0);
    t.tickUntil(t.now + 1.5);
    if (t.live(t.now, 1).length) t.problems.push(`group 1 still live after the stop: kinds ${t.live(t.now, 1)}`);
    const after = records.filter((r) => r.kind === "synth" && r.name === "live_loop_flibble" && r.time > t.now - 1).length;
    if (after) t.problems.push(`:flibble still triggered ${after} times after the stop`);
  });

  scenario("a loop redefined from another group moves to it: the old group's stop leaves it, the new one's takes it", (t) => {
    t.run(LOOP, t.now, 1);
    t.tickUntil(t.now + 1.5);
    t.run("live_loop :flibble do\n  sample :bd_haus\n  sleep 0.25\nend\n", t.now, 2);   // the editor, in its own group
    t.tickUntil(t.now + 1.5);
    t.stop(1, 0);
    t.tickUntil(t.now + 1.5);
    const kept = records.filter((r) => r.kind === "synth" && r.name === "live_loop_flibble" && r.time > t.now - 1).length;
    if (kept < 3) t.problems.push(`the loop should play on under group 2 after group 1's stop: ${kept} hits in the last second`);
    if (!t.live(t.now, 2).length) t.problems.push("group 2 should be live");
    t.stop(2, 0);
    t.tickUntil(t.now + 1.5);
    if (t.live(t.now, 2).length) t.problems.push("group 2 still live after its stop");
    const after = records.filter((r) => r.kind === "synth" && r.name === "live_loop_flibble" && r.time > t.now - 1).length;
    if (after) t.problems.push(`:flibble still triggered ${after} times after group 2's stop`);
  });

  scenario("a stop with a fade turns the group's fx down first and frees them as the fade ends", (t) => {
    t.run(LOOP, t.now, 1);
    t.tickUntil(t.now + 2);
    audio = [];
    const at = t.now;
    t.stop(1, 0.25);
    t.tickUntil(t.now + 1);
    const msgs = t.osc();
    const fades = msgs.filter((x) => x.msg[0] === "/n_set" && x.msg.includes("amp_slide"));
    const frees = msgs.filter((x) => x.msg[0] === "/n_free");
    if (!fades.length) t.problems.push("no amp fade was sent");
    if (!frees.length) t.problems.push("no free was sent");
    if (frees.some((f) => f.t < at + 0.25)) t.problems.push(`a free left before the fade's end: ${frees.map((f) => (f.t - at).toFixed(3)).join(" ")}`);
    if (t.live(t.now, 1).length) t.problems.push("group 1 still live after the faded stop");
  });

  scenario("a group under another goes with it: stopping the parent stops the child, the child's stop leaves the parent", (t) => {
    m._sp_group_under(11, 10);
    m._sp_group_under(12, 10);
    t.run(LOOP, t.now, 11);
    t.run(CARD("live_loop :other do\n  play 60, release: 0.1\n  sleep 0.5\nend", 2), t.now, 12);
    t.run("live_loop :parent do\n  play 64, release: 0.1\n  sleep 0.5\nend\n", t.now, 10);
    t.tickUntil(t.now + 1.5);
    t.stop(12, 0);
    t.tickUntil(t.now + 1);
    if (t.live(t.now, 12).length) t.problems.push("group 12 still live after its stop");
    if (!t.live(t.now, 11).length || !t.live(t.now, 10).length) t.problems.push("groups 10 and 11 should still be live after 12's stop");
    t.stop(10, 0);
    t.tickUntil(t.now + 1.5);
    for (const g of [10, 11]) if (t.live(t.now, g).length) t.problems.push(`group ${g} still live after the parent's stop`);
    const after = records.filter((r) => r.kind === "synth" && r.time > t.now - 1).length;
    if (after) t.problems.push(`${after} sounds still triggered after the parent's stop`);
  });

  // ── Runs: a card is its runs (ui/deck.js). Each Play of a card is a run in the cards' one group, and the card's
  // Stop stops each run it started, as a subtree: the run's threads and every thread under them, its fx and its
  // sounds (Scheduler#stop_run). A loop a later run redefined has moved under that run (Scheduler#move_loop), so
  // it goes with the run that last said what it plays, whichever run it was born in. No card has a group of its
  // own, so none is left behind it: fourteen plays of a card were fourteen groups in the table for good.
  const CARDS = 2000;
  const hits = (t, name, within = 1) => records.filter((r) => r.kind === "synth" && r.name === name && r.time > t.now - within).length;
  const sounding = (t, job) => { const tb = table(t.now); let n = 0; for (let i = 0; i + FIELDS <= tb.length; i += FIELDS) if ((tb[i + KIND] === 7 || tb[i + KIND] === 8) && tb[i + STATE] === 0 && tb[i + 2] === job) n++; return n; };
  const groupsIn = (t) => { const tb = table(t.now); const out = []; for (let i = 0; i + FIELDS <= tb.length; i += FIELDS) if (tb[i + KIND] === 9) out.push(tb[i + GROUP]); return out.sort((a, b) => a - b); };

  scenario("a run stops as one by its job: its loop and its sounds go, another run of the same group plays on", (t) => {
    const a = t.run(LOOP, t.now, CARDS);
    const b = t.run(CARD("live_loop :other do\n  play 60, release: 0.1\n  sleep 0.5\nend", 2), t.now, CARDS);
    t.tickUntil(t.now + 2);
    if (hits(t, "live_loop_flibble") < 1 || hits(t, "live_loop_other") < 1) t.problems.push("both loops should be playing before the stop");
    t.stopRun(a, 0);
    t.tickUntil(t.now + 1.5);
    if (hits(t, "live_loop_flibble")) t.problems.push(`:flibble still triggered ${hits(t, "live_loop_flibble")} times after its run's stop`);
    if (hits(t, "live_loop_other") < 1) t.problems.push(":other, the other run's, should play on");
    if (!t.live(t.now, CARDS).length) t.problems.push("the group both are in should still be live");
    t.stopRun(b, 0);
    t.tickUntil(t.now + 1.5);
    if (hits(t, "live_loop_other")) t.problems.push(":other still plays after its own run's stop");
  });

  scenario("a card played again: the loop goes with the run that redefined it, a note still sounding with the run that played it", (t) => {
    const first = t.run(CARD("play 50, release: 8\nlive_loop :l do\n  play 60, release: 0.1\n  sleep 0.5\nend"), t.now, CARDS);
    t.tickUntil(t.now + 1.5);
    const second = t.run(CARD("live_loop :l do\n  play 72, release: 0.1\n  sleep 0.25\nend"), t.now, CARDS);   // Play again: the loop takes the new code
    t.tickUntil(t.now + 1.5);
    if (hits(t, "live_loop_l") < 3) t.problems.push(`the redefined loop should be playing faster: ${hits(t, "live_loop_l")} hits in the last second`);
    if (!sounding(t, first)) t.problems.push("the first run's long note should still be sounding");
    t.stopRun(first, 0);
    t.tickUntil(t.now + 1.5);
    if (sounding(t, first)) t.problems.push("the first run's note still sounds after that run's stop");
    if (hits(t, "live_loop_l") < 3) t.problems.push(`the loop is the second run's now, and should play on after the first's stop: ${hits(t, "live_loop_l")} hits in the last second`);
    t.stopRun(second, 0);
    t.tickUntil(t.now + 1.5);
    if (hits(t, "live_loop_l")) t.problems.push(`the loop still triggered ${hits(t, "live_loop_l")} times after the stop of the run that redefined it`);
    if (t.live(t.now, CARDS).length) t.problems.push(`the group should be over with both runs stopped: kinds ${t.live(t.now, CARDS)}`);
  });

  scenario("a run's stop takes a loop it redefined from a buffer's run; the buffer's run's stop, and its group's, leave it", (t) => {
    const buffer = t.run("live_loop :m do\n  play 60, release: 0.1\n  sleep 0.5\nend\n", t.now, 1001);   // the editor's, in its buffer's group
    t.tickUntil(t.now + 1.5);
    const card = t.run(CARD("live_loop :m do\n  play 72, release: 0.1\n  sleep 0.25\nend"), t.now, CARDS);
    t.tickUntil(t.now + 1.5);
    t.stopRun(buffer, 0);
    t.stop(1001, 0);
    t.tickUntil(t.now + 1.5);
    if (hits(t, "live_loop_m") < 3) t.problems.push(`the loop is the card's run's now, and should play on: ${hits(t, "live_loop_m")} hits in the last second`);
    t.stopRun(card, 0);
    t.tickUntil(t.now + 1.5);
    if (hits(t, "live_loop_m")) t.problems.push(`the loop still triggered ${hits(t, "live_loop_m")} times after the stop of the run that redefined it`);
  });

  scenario("a run's stop with a fade turns its fx down first and frees them as the fade ends", (t) => {
    const job = t.run(LOOP, t.now, CARDS);
    t.tickUntil(t.now + 2);
    audio = [];
    const at = t.now;
    t.stopRun(job, 0.25);
    t.tickUntil(t.now + 1);
    const msgs = t.osc();
    const fades = msgs.filter((x) => x.msg[0] === "/n_set" && x.msg.includes("amp_slide"));
    const frees = msgs.filter((x) => x.msg[0] === "/n_free");
    if (!fades.length) t.problems.push("no amp fade was sent");
    if (!frees.length) t.problems.push("no free was sent");
    if (frees.some((f) => f.t < at + 0.25)) t.problems.push(`a free left before the fade's end: ${frees.map((f) => (f.t - at).toFixed(3)).join(" ")}`);
    if (hits(t, "live_loop_flibble", 0.7)) t.problems.push("the loop still plays after the faded stop");
  });

  scenario("the stop of a run that is over, or of one there never was, does nothing and says nothing", (t) => {
    const done = t.run(CARD("play 60, release: 0.1"), t.now, CARDS);
    const loop = t.run(CARD("live_loop :on do\n  play 64, release: 0.1\n  sleep 0.5\nend", 2), t.now, CARDS);
    t.tickUntil(t.now + 5);   // the one-shot is over, and gone from the table
    audio = [];
    const errorsBefore = records.filter((r) => r.kind === "error").length;
    t.stopRun(done, 0.25);
    t.stopRun(4242, 0);
    t.tickUntil(t.now + 1);
    if (records.filter((r) => r.kind === "error").length !== errorsBefore) t.problems.push("the stop raised an error");
    if (t.osc().some((x) => x.msg[0] === "/n_free" && x.t < 1)) t.problems.push("the stop freed something at once");
    if (hits(t, "live_loop_on") < 1) t.problems.push("the run beside it should play on");
    t.stopRun(loop, 0);
  });

  scenario("fourteen plays of a card leave no group behind them: the table has the cards' group, and no other", (t) => {
    for (let i = 0; i < 14; i++) {
      const job = t.run(CARD(`live_loop :card do\n  play ${60 + i}, release: 0.1\n  sleep 0.25\nend`), t.now, CARDS);
      t.tickUntil(t.now + 0.5);
      if (i === 0 && groupsIn(t).join() !== String(CARDS)) t.problems.push(`the groups in the table as the first plays: ${groupsIn(t).join()}`);
      t.stopRun(job, 0);
      t.tickUntil(t.now + 0.3);
    }
    t.tickUntil(t.now + 4);
    if (groupsIn(t).join() !== String(CARDS)) t.problems.push(`the groups in the table after fourteen plays: ${groupsIn(t).join()}`);
    const tb = table(t.now); let rows = 0; for (let i = 0; i + FIELDS <= tb.length; i += FIELDS) if (tb[i + KIND] !== 9) rows++;
    if (rows) t.problems.push(`${rows} rows of the runs are still in the table once they are over`);
  });

  scenario("a subtree stops: one live loop by its uid, its sibling plays on; an fx block by its uid takes the loops inside it", (t) => {
    t.run("live_loop :a do\n  play 60, release: 0.1\n  sleep 0.5\nend\nlive_loop :b do\n  play 64, release: 0.1\n  sleep 0.5\nend\nwith_fx :reverb do\n  live_loop :c do\n    play 67, release: 0.1\n    sleep 0.5\n  end\nend\n", t.now, 1);
    t.tickUntil(t.now + 1.5);
    const uidOf = (name) => { const tb = table(t.now); for (let i = 0; i + FIELDS <= tb.length; i += FIELDS) { const r = records.find((x) => x.kind === "thread" && x.event === "start" && x.name === name); if (r) { const th = [...records].filter((x) => x.kind === "thread" && x.event === "start" && x.name === name); } } const start = records.find((x) => x.kind === "thread" && x.event === "start" && x.name === name); return start ? start.uid : null; };
    const rows = () => { const tb = table(t.now); const out = []; for (let i = 0; i + FIELDS <= tb.length; i += FIELDS) out.push({ uid: tb[i], kind: tb[i + KIND], state: tb[i + STATE] }); return out; };
    const loopUid = (name) => { const r = reader.thread ? null : null; const entry = [...rows()].find((row) => row.kind === 2 && row.state <= 2 && reader.thread(row.uid)?.name === name); return entry?.uid; };
    const a = loopUid("live_loop_a"), c = loopUid("live_loop_c");
    if (a == null || c == null) { t.problems.push(`could not find the loops' uids (${a}, ${c})`); return; }
    m._sp_stop_subtree(a, 0, t.now); drain();
    t.tickUntil(t.now + 1);
    const late = (name) => records.filter((r) => r.kind === "synth" && r.name === name && r.time > t.now - 0.9).length;
    if (late("live_loop_a")) t.problems.push(":a still plays after its stop");
    if (!late("live_loop_b")) t.problems.push(":b should play on after :a's stop");
    const fx = rows().find((row) => row.kind === 6 && row.state < 3);
    if (!fx) { t.problems.push("no open fx row"); return; }
    m._sp_stop_subtree(fx.uid, 0, t.now); drain();
    t.tickUntil(t.now + 1);
    if (late("live_loop_c")) t.problems.push(":c still plays after its fx block's stop");
    if (!late("live_loop_b")) t.problems.push(":b should play on after the fx block's stop");
    if (rows().some((row) => row.kind === 6 && row.state < 3)) t.problems.push("the fx block is still open");
  });

  scenario("the table shows the groups as nodes: a run hangs from its group, a group from its parent, live until nothing of it is", (t) => {
    m._sp_group_under(21, 20);
    t.run(LOOP, t.now, 21);
    t.tickUntil(t.now + 1);
    const child = groupRow(t.now, 21), parent = groupRow(t.now, 20);
    if (!child || !parent) { t.problems.push("group rows missing"); return; }
    if (child.parent !== parent.uid) t.problems.push("the child group should hang from its parent");
    if (child.state !== 0 || parent.state !== 0) t.problems.push("both groups should be live");
    const tb = table(t.now); let runParent = null; for (let i = 0; i + FIELDS <= tb.length; i += FIELDS) if (tb[i + KIND] === 0 && tb[i + GROUP] === 21) runParent = tb[i + 1];
    if (runParent !== child.uid) t.problems.push(`the run should hang from its group's row (parent ${runParent}, group uid ${child.uid})`);
    t.stop(20, 0);
    t.tickUntil(t.now + 1);
    if (groupRow(t.now, 21).state !== 3 || groupRow(t.now, 20).state !== 3) t.problems.push("both groups should be over after the parent's stop");
  });

  scenario("a one-shot's group is live while its sound sounds, and not after", (t) => {
    t.run(CARD("play 60, release: 1"), t.now, 1);
    t.tickUntil(t.now + 0.8);
    if (!t.live(t.now, 1).length) t.problems.push("the group should be live while the note sounds");
    t.tickUntil(t.now + 3);
    if (t.live(t.now, 1).length) t.problems.push(`the group should be over once the note has ended: kinds ${t.live(t.now, 1)}`);
  });

  // ── Stop, as native's: the runs fade and go, the studio stays ───────────────────────────────────────────────
  // The engine's node tree as the OSC builds it, each message at its time (an immediate one as it arrives): what a
  // message names must be there, as scsynth would refuse it otherwise. `at` is the clock at each drain.
  const engineTree = (msgs) => {
    const parent = new Map([[0, null]]), kids = new Map([[0, []]]), problems = [];
    const gone = (id) => { for (const k of kids.get(id) ?? []) gone(k); kids.delete(id); const p = parent.get(id); parent.delete(id); if (p != null) kids.set(p, kids.get(p).filter((x) => x !== id)); };
    const place = (id, action, target) => {
      const into = action <= 1 ? target : parent.get(target);
      const list = kids.get(into), i = list.indexOf(target);
      if (action === 0) list.unshift(id); else if (action === 1) list.push(id); else list.splice(action === 2 ? i : i + 1, 0, id);
      parent.set(id, into);
      if (!kids.has(id)) kids.set(id, []);
    };
    const order = msgs.map((x, i) => ({ ...x, when: x.t < 1 ? x.at : x.t, i })).sort((a, b) => a.when - b.when || a.i - b.i);
    for (const { msg: [address, ...args], when } of order) {
      const need = (...ids) => ids.every((id) => parent.has(id) || (problems.push(`${address} ${args.slice(0, 4).join(" ")} at ${when.toFixed(3)}: no node ${id}`), false));
      if (address === "/g_new") { if (parent.has(args[0])) problems.push(`/g_new ${args[0]}: already there`); else if (need(args[2])) place(args[0], args[1], args[2]); }
      else if (address === "/s_new") { if (parent.has(args[1])) problems.push(`/s_new ${args[0]} ${args[1]}: already there`); else if (need(args[3])) place(args[1], args[2], args[3]); }
      else if (address === "/n_free") { if (need(args[0])) gone(args[0]); }
      else if (address === "/g_freeAll") { if (need(args[0])) for (const k of [...kids.get(args[0])]) gone(k); }
      else if (address === "/n_order") { if (need(args[1], args[2])) { const p = parent.get(args[2]); kids.set(p, kids.get(p).filter((x) => x !== args[2])); parent.delete(args[2]); place(args[2], args[0], args[1]); } }
      else if (address === "/n_set" || address === "/n_run") need(args[0]);
    }
    return { problems, parent, kids };
  };
  const timed = () => audio.filter((a) => a.kind !== FRAME_HOST).map(({ msg, at }) => { const b = decode(msg[3]); return { t: b.timeTag, at, msg: b.packets[0] }; });
  const silence = (t, fade = 1) => { drainAt = t.now; m._sp_silence(fade, t.now, t.now); drain(); };
  const PLAYING = CARD("live_loop :a do\n  with_fx :reverb do\n    play 60, release: 0.2\n  end\n  sleep 0.25\nend\nlive_loop :b do\n  sample :bd_haus\n  sleep 0.5\nend");

  scenario("Stop fades each run's mixer and frees the runs as the fade ends; the studio stays, and the next Run makes none", (t) => {
    t.run(PLAYING, t.now, 0)
    t.run("live_loop :c do\n  play 72, release: 0.1\n  sleep 0.5\nend", t.now, 0)
    t.tickUntil(t.now + 2);
    m._sp_stop_all(); drain()
    const stoppedAt = t.now, before = timed().length;
    silence(t)
    const stop = timed().slice(before);
    const fades = stop.filter((x) => x.msg[0] === "/n_set" && x.msg.includes("amp_slide"));
    const moved = stop.filter((x) => x.msg[0] === "/n_order" && x.msg[2] === 1003);
    const freed = stop.filter((x) => x.msg[0] === "/g_freeAll" && x.msg[1] === 1003);
    if (fades.length !== 2) t.problems.push(`${fades.length} mixers faded, not the two runs'`);
    if (moved.length !== 1) t.problems.push(`the runs were not moved into DYING as one (${moved.length})`);
    if (freed.length !== 1 || freed[0].t < stoppedAt + 1) t.problems.push(`DYING not emptied as the fade ends: ${freed.map((f) => (f.t - stoppedAt).toFixed(3)).join(" ")}`);
    if (stop.some((x) => x.msg[0] === "/n_free" || (x.msg[0] === "/g_freeAll" && x.msg[1] !== 1003))) t.problems.push("the stop freed more than DYING");
    t.now += 0.3;   // a Run during the fade: its own place, beside the fading ones
    t.run(PLAYING, t.now, 0)
    t.tickUntil(t.now + 2);
    const all = timed();
    const made = (id) => all.filter((x) => (x.msg[0] === "/g_new" && x.msg[1] === id) || (x.msg[0] === "/s_new" && x.msg[2] === id)).length;
    for (const id of STUDIO) if (made(id) !== 1) t.problems.push(`studio node ${id} made ${made(id)} times`);
    const tree = engineTree(all);
    if (tree.problems.length) t.problems.push(...tree.problems.slice(0, 3));
    if ((tree.kids.get(1003) ?? []).length) t.problems.push(`DYING still holds ${tree.kids.get(1003)}`);
    const after = all.slice(before).filter((x) => x.msg[1] === "sonic-pi-basic_mixer").length;
    if (after !== 1) t.problems.push(`the Run after the Stop made ${after} run mixers, not its own one`);
  });

  scenario("a run that is over goes from the engine a second after its last sound, and its bus comes back", (t) => {
    t.run("with_fx :echo, decay: 1 do\n  play 60, release: 0.5\nend", t.now, 0)
    const started = t.now;
    t.tickUntil(t.now + 1);
    t.run("live_loop :keep do\n  sleep 0.5\n  play 50, release: 0.1\nend", t.now, 0)   // the host ticks on
    t.tickUntil(t.now + 6);
    const all = timed();
    const place = all.find((x) => x.msg[1] === "sonic-pi-basic_mixer"), group = place?.msg[4];
    const fxFree = all.find((x) => x.msg[0] === "/n_free" && x.msg[1] !== group && x.t > 1);
    const gone = all.find((x) => x.msg[0] === "/n_free" && x.msg[1] === group);
    if (!gone) t.problems.push("the first run's group was never freed");
    else if (fxFree && gone.at < fxFree.t + 1 - 1e-6) t.problems.push(`freed ${(gone.at - fxFree.t).toFixed(3)}s after its fx, not a second`);
    const tree = engineTree(all);
    if (tree.problems.length) t.problems.push(...tree.problems.slice(0, 3));
    // the next run, while the loop plays on: its bus and its fx's are the first run's and its echo's, back for use
    const firsts = new Set(all.filter((x) => x.msg[0] === "/s_new" && (x.t < 1 ? x.at : x.t) < started + 0.5 && x.msg.includes("in_bus")).map((x) => x.msg[x.msg.indexOf("in_bus") + 1]));
    const before = timed().length;
    t.run("with_fx :lpf do\n  play 60, release: 0.1\nend", t.now, 0);
    const next = timed().slice(before).filter((x) => x.msg[0] === "/s_new" && x.msg.includes("in_bus")).map((x) => x.msg[x.msg.indexOf("in_bus") + 1]);
    if (next.length !== 2 || !next.every((b) => firsts.has(b))) t.problems.push(`the next run's busses ${next.join(" ")}, not the first run's ${[...firsts].join(" ")} back`);
  });

  scenario("two Stops in one fade: DYING is emptied again after the second, whatever the first's free met", (t) => {
    t.run(PLAYING, t.now, 0)
    t.tickUntil(t.now + 1);
    m._sp_stop_all(); drain(); silence(t)
    t.now += 0.4;
    t.run(PLAYING, t.now, 0)
    t.tickUntil(t.now + 0.5);
    const at = t.now, before = timed().length;
    m._sp_stop_all(); drain(); silence(t)   // the host emptied the schedule first: the first free went with it
    const frees = timed().slice(before).filter((x) => x.msg[0] === "/g_freeAll" && x.msg[1] === 1003);
    if (frees.length !== 1 || frees[0].t < at + 1) t.problems.push(`the second Stop's free of DYING: ${frees.map((f) => (f.t - at).toFixed(3)).join(" ")}`);
    const tree = engineTree(timed().filter((x) => !(x.msg[0] === "/g_freeAll" && x.t > 1 && x.t < at + 1)));   // as though the first free was dropped
    if (tree.problems.length) t.problems.push(...tree.problems.slice(0, 3));
    if ((tree.kids.get(1003) ?? []).length) t.problems.push(`DYING still holds ${tree.kids.get(1003)}`);
  });

  scenario("a loop moved between runs keeps its first run's place; runs come and go, and nothing names a node that is gone", (t) => {
    t.run("with_fx :lpf do\n  live_loop :d do\n    play 60, release: 0.1\n    sleep 0.25\n  end\nend", t.now, 0);
    t.tickUntil(t.now + 1.5);
    t.run("with_fx :level do\n  live_loop :d do\n    play 62, release: 0.1\n    sleep 0.25\n  end\nend", t.now, 0);   // into this run's fx
    t.tickUntil(t.now + 1.5);
    t.run("with_fx :echo, decay: 0.5 do\n  play 72, release: 0.2\nend", t.now, 0);                                     // a one-shot, over soon
    t.tickUntil(t.now + 1.5);
    t.run("live_loop :d do\n  play 64, release: 0.1\n  sleep 0.25\nend", t.now, 0);                                   // out of every fx: its own run's place
    t.tickUntil(t.now + 6);
    const all = timed(), tree = engineTree(all);
    if (tree.problems.length) t.problems.push(...tree.problems.slice(0, 3));
    const mixers = all.filter((x) => x.msg[1] === "sonic-pi-basic_mixer").map((x) => x.msg[4]);
    const standing = mixers.filter((g) => tree.parent.has(g));
    if (standing.length !== 1 || standing[0] !== mixers[0]) t.problems.push(`the runs standing ${standing}, not only the loop's first run ${mixers[0]} (of ${mixers})`);
    const late = records.filter((r) => r.kind === "synth" && r.name === "live_loop_d" && r.time > t.now - 1).length;
    if (late < 3) t.problems.push(`the loop should play on: ${late} in the last second`);
  });

  scenario("a card's faded stop, then its run goes from the engine too", (t) => {
    t.run(PLAYING, t.now, 1);
    t.run("live_loop :other do\n  play 50, release: 0.1\n  sleep 0.5\nend", t.now, 2);
    t.tickUntil(t.now + 2);
    t.stop(1, 0.25);
    t.tickUntil(t.now + 4);
    const all = timed(), tree = engineTree(all);
    if (tree.problems.length) t.problems.push(...tree.problems.slice(0, 3));
    const mixers = all.filter((x) => x.msg[1] === "sonic-pi-basic_mixer").map((x) => x.msg[4]);
    if (tree.parent.has(mixers[0])) t.problems.push("the stopped card's run is still in the engine");
    if (!tree.parent.has(mixers[1])) t.problems.push("the other card's run went too");
  });

  scenario("set_volume! reaches the studio's mixer at once, the studio made or not", (t) => {
    t.run("set_volume! 0.5\nplay 60, release: 0.1\nsleep 0.5\nset_volume! 0.25\nplay 60, release: 0.1", t.now, 0);
    t.tickUntil(t.now + 2);
    const all = timed();
    const mixer = all.find((x) => x.msg[1] === "sonic-pi-mixer");
    const set = all.filter((x) => x.msg[0] === "/n_set" && x.msg[1] === 1002);
    if (!mixer || mixer.msg[mixer.msg.indexOf("amp") + 1] !== 0.5) t.problems.push(`the studio was made at ${mixer && mixer.msg[mixer.msg.indexOf("amp") + 1]}, not 0.5`);
    if (set.length !== 1 || set[0].msg[3] !== 0.25) t.problems.push(`the second set_volume! reached the mixer as ${JSON.stringify(set.map((x) => x.msg))}`);
  });

  scenario("a group's stop leaves a bare sound of another group alone and fades its own", (t) => {
    t.run("play 60, sustain: 4, release: 1\n", t.now, 1);
    t.run("play 64, sustain: 4, release: 1\n", t.now, 2);
    t.tickUntil(t.now + 0.5);
    audio = [];
    t.stop(1, 0.2);
    t.tickUntil(t.now + 1);
    const msgs = t.osc();
    const nsets = msgs.filter((x) => x.msg[0] === "/n_set"), frees = msgs.filter((x) => x.msg[0] === "/n_free");
    if (nsets.length !== 1 || frees.length !== 1) t.problems.push(`expected one fade and one free for group 1's sound: ${nsets.length} n_set, ${frees.length} n_free`);
    if (t.live(t.now, 1).length) t.problems.push("group 1 still live");
    if (!t.live(t.now, 2).length) t.problems.push("group 2's note should still sound");
  });
}
console.log(`RT: ${pass} pass, ${fail} fail` + (logDiffers ? ` (${logDiffers} with the log differing)` : ""));
console.log(`audio: ${soundsHeard} sounds in the OSC stream checked against their records`);
process.exit(fail ? 1 : 0);
