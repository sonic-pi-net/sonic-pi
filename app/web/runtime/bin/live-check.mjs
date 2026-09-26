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
const drain = () => {
  const len = m._sp_out_len();
  if (!len) return;
  const heap = m.HEAPU8;
  forEachFrame(heap, m._sp_out_ptr(), len, (kind, start, size) => {
    if (kind === FRAME_GUI) records.push(reader.read(decode(heap.subarray(start, start + size))));
    else audio.push({ kind, msg: decode(heap.slice(start, start + size)) });
  });
};
const NODE_BASE = 10000;
const STUDIO_SYNTHS = 1001;     // the group every sound outside a with_fx starts at the head of (Scheduler::STUDIO_SYNTHS)
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
  // live loop's fx moved to another block (re-routed and reordered)
  const groups = new Set(sounds.filter((s) => s.packet[0] === "/g_new").map((s) => s.packet[1]));
  const studio = (s) => {
    const [address, id, ...rest] = s.packet;
    return address === "/g_new" || address === "/n_order" || (address === "/n_free" && groups.has(id))
      || (address === "/n_set" && rest.length === 2 && rest[0] === "out_bus")
      || (address === "/s_new" && rest[0] < NODE_BASE);        // the studio's own mixers
  };
  // the studio is made before a session's first sound: its groups, then sonic-pi-mixer and basic_mixer into it
  const mixers = sounds.filter((s) => s.packet[0] === "/s_new" && s.packet[2] < NODE_BASE).map((s) => s.packet[1]);
  const studioProblems = sounds.length && !(mixers.includes("sonic-pi-mixer") && mixers.includes("sonic-pi-basic_mixer"))
    ? [`the studio's mixers were not made (${JSON.stringify(mixers)})`] : [];
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
      const where = (args[2] === 0 && args[3] === STUDIO_SYNTHS) || (r.fx && args[2] === 1) || (r.synth.startsWith("sonic-pi-fx_") && args[2] === 1);
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
    const bundles = audio.filter((a) => a.kind !== FRAME_HOST).map((a) => decode(a.msg[3])).filter((b) => b.packets[0][0] === "/s_new" && b.packets[0][2] >= NODE_BASE).map((b) => b.timeTag);
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
// ── Groups: the unit the GUI plays and stops by (Scheduler#stop_group). A card's runs are one group, a buffer's
// another. Each scenario drives the runtime as the page does — sp_run_group, ticks, sp_stop_group — and reads the
// process table (a group column, the 15th) and the OSC the engine gets.
{
  const FIELDS = 15, GROUP = 14, KIND = 3, STATE = 4;
  const table = (now) => { const ptr = m._sp_process_table(now), n = m._sp_process_table_len(); return Array.from(m.HEAPF64.subarray(ptr / 8, ptr / 8 + n)); };
  // a group is live while a thread of it runs, sleeps or waits, an fx of it is open, or a sound of it sounds
  const live = (now, g) => { const t = table(now); const out = []; for (let i = 0; i + FIELDS <= t.length; i += FIELDS) { if (t[i + GROUP] !== g) continue; const k = t[i + KIND], st = t[i + STATE]; if ((k >= 1 && k <= 5 && st <= 2) || (k === 6 && st < 3) || ((k === 7 || k === 8) && st === 0)) out.push(k); } return out; };
  // the group's own row: live (0) while anything of it or under it is; a run's row hangs from it
  const groupRow = (now, g) => { const t = table(now); for (let i = 0; i + FIELDS <= t.length; i += FIELDS) if (t[i + KIND] === 9 && t[i + GROUP] === g) return { uid: t[i], parent: t[i + 1], state: t[i + STATE] }; return null; };
  const run = (code, now, g) => { const job = m.ccall("sp_run_group", "number", ["string", "number", "number"], [code, now, g]); drain(); return job; };
  const osc = () => audio.map(({ msg }) => { const [, , , bytes] = msg; const b = decode(bytes); const pk = b.packets?.[0] ?? b; return { t: b.timeTag, msg: pk }; });
  const scenario = (name, body) => { records = []; audio = []; m._sp_live_boot(); m._sp_live_stop_after(-1); let now = 1000; const tickUntil = (until) => { for (let i = 0; i < 100000 && now < until; i++) { const next = m._sp_tick(now); drain(); now = next < 0 ? until : Math.min(Math.max(now, next), until); } }; const problems = []; body({ run, tickUntil, get now() { return now; }, set now(v) { now = v; }, live, osc, problems, stop: (g, fade) => { m._sp_stop_group(g, fade, now); drain(); } }); m._sp_stop_all(); drain(); const ok = problems.length === 0; console.log(`${ok ? "pass" : "FAIL"} ${name}${ok ? "" : ": " + problems.join("; ")}`); if (ok) pass++; else fail++; };
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
