// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
/**
 * The live loop, wherever it runs: on the page (sonic_pi.js LiveSession) or
 * in a worker of its own (live-worker.js, behind sonic_pi.js WorkerSession).
 * It ticks the runtime against the engine's clock, hands each sound's bundle
 * over as it is once what it needs has loaded, and passes on the runtime's
 * records and its word on what to load and free. The scheduler never sleeps;
 * this loop does, until the next wake time the runtime reports.
 */
import { decode, forEachFrame, FRAME_SOUND, FRAME_HOST, FRAME_GUI } from "./osc.js";
import { programNeeds } from "./runtime.js";

/** Timings since the last takePerf(). */
export const freshPerf = () => ({
  ticks: 0, tickMs: 0, tickMsMax: 0,         // a tick: the runtime's sp_tick, and its sounds handed to the engine
  statusMsMax: 0,                            // what follows a tick: the page's status, or a worker's batch
  wakeLateMsMax: 0,                          // how much later than asked a tick's timer fired
  records: 0, sounds: 0,
  headroomMin: Infinity, headroomSum: 0,     // a sound's time less the clock when it left, in seconds
  sentLate: 0, tight: 0,                     // sounds sent after their time; with under 50 ms to spare
  recordMsMax: 0,                            // the page's handling of one record (flashes, views)
});

/** One set of timings added into another: a worker's, gathered on the page. */
export function addPerf(into, p) {
  for (const k of ["ticks", "tickMs", "records", "sounds", "headroomSum", "sentLate", "tight"]) into[k] += p[k];
  for (const k of ["tickMsMax", "statusMsMax", "wakeLateMsMax", "recordMsMax"]) into[k] = Math.max(into[k], p[k]);
  into.headroomMin = Math.min(into.headroomMin, p.headroomMin);
  return into;
}

/** How long before its time a sound (or a MIDI message) left. */
export function countHeadroom(p, time, now) {
  const headroom = time - now;
  p.sounds++;
  p.headroomSum += headroom;
  if (headroom < p.headroomMin) p.headroomMin = headroom;
  if (headroom < 0) p.sentLate++;
  else if (headroom < 0.05) p.tight++;
}

const HOLD_SECS = 1;   // a gap this long is a hold, not jitter
const SETTLE_SECS = 0.1;   // once a run's first loads are in, how long it waits for any more to be asked for before it plays

/** A bundle's time moved on by `secs`: its NTP timetag (bytes 8 to 16) rewritten; an immediate one (1) left as it is. */
export function retime(bundle, secs) {
  const v = new DataView(bundle.buffer, bundle.byteOffset, bundle.byteLength);
  const hi = v.getUint32(8), lo = v.getUint32(12);
  if (hi === 0) return bundle;
  const t = hi + lo / 4294967296 + secs, whole = Math.floor(t);
  v.setUint32(8, whole);
  v.setUint32(12, Math.min(4294967295, Math.round((t - whole) * 4294967296)));
  return bundle;
}

export class LiveCore {
  #runtime; #deps; #timer = null; #expectedWake = null; #lastNow = null; #lastWall = null;
  #stops = 0;               // a sound held back for a load does not outlive a Stop
  // Sounds wait here, in the order the runtime made them, while one ahead of them waits on a load: nothing
  // overtakes a sound held back (a control before its synth, an fx's free before the fx), so they go as they
  // were made, only later.
  #queue = [];              // { bundle, starts, name, buf }
  #pumping = false;
  // A run started with nothing playing, until a schedule-ahead has passed (a run's first sounds are that far
  // ahead): if what it plays is still loading, it does not start in pieces, each part as its load comes in. The
  // clock stops, everything loads, and it starts late, all of it together, as though Run were pressed then.
  // { start: the engine's clock at the run, waiting: loads being waited on, released: the promise of the wait }
  #gate = null;
  /** Timings since the last takePerf(): see freshPerf. */
  perf = freshPerf();
  running = false;
  /** The engine's clock when the session's first run started: records count their times from it. */
  t0 = null;

  /**
   * @param runtime loadRuntime's
   * @param deps
   *   now(): the engine's clock, in NTP seconds
   *   send(bundle): a sound's OSC bundle (a Uint8Array of its own) for the engine
   *   loader: what bundles need loaded, the Bridge's way (synthDef, synthDefReady,
   *     loadBuffer, bufferReady, bufferLoaded, freeBuffer)
   *   record(bytes): one record's OSC message, for the page (a view: copy it to keep it)
   *   host(address, number, name): the runtime's word on what to load and free
   *   started(): a synth started (an /s_new went)
   *   state(running): a run started, or everything stopped or ended
   *   ticked(): after a tick, a stop or a job stopping: the process table may have moved
   */
  constructor(runtime, deps) {
    this.#runtime = runtime;
    this.#deps = deps;
    // the runtime numbers synthdefs and buffers for as long as it lives, across sessions
    runtime.synthdefs ??= [];
    if (runtime.module._sp_live_boot() !== 0) throw new Error("the live session did not boot");
  }

  /** The buffer the runtime numbers a sample file with, or null. */
  bufferFor(file) {
    const n = this.#runtime.module.ccall("sp_buffer_for", "number", ["string"], [file]);
    return n >= 0 ? n : null;
  }

  /** What a program names that it will need loaded: synthdefs, and samples with the buffers the runtime gives them. */
  needs(code) {
    const { synthdefs, samples } = programNeeds(code, this.#runtime.samples, this.#runtime.synthdefFor);
    return { synthdefs, samples: samples.map((file) => ({ file, bufnum: this.bufferFor(file) })).filter((s) => s.bufnum != null) };
  }

  /**
   * Runs a program as a new job: what it names starts loading (preload(needs)), and its head runs now, up to its
   * first sleep. Earlier runs carry on; live loops with the same name take over. Run with nothing playing, it
   * waits for what its first sounds need, and starts when it has it (#gate).
   */
  async run(code, { preload = null, group = 0 } = {}) {
    while (this.#gate?.waiting) await this.#gate.released;   // a run pressed during another's wait joins its start
    preload?.(this.needs(code));
    const now = this.#deps.now();
    if (this.#gate && now > this.#gate.start + this.schedAhead) this.#gate = null;   // its window has passed
    if (!this.running && !this.#gate) this.#gate = { start: now, waiting: false, released: null, first: this.t0 == null };
    this.t0 ??= now;
    const job = this.#runtime.module.ccall("sp_run_group", "number", ["string", "number", "number"], [code, now, group]);
    this.#drain();
    if (job < 0) throw new Error("the program did not start");
    this.running = true;
    this.#deps.state?.(true);
    this.tick();
    if (this.#gate?.waiting) await this.#gate.released;   // it has started when it returns
    return job;
  }

  tick() {
    if (!this.running || this.#gate?.waiting) return;   // a gate's wait: its release ticks again
    clearTimeout(this.#timer);
    const p = this.perf;
    const t0 = performance.now();
    const late = this.#expectedWake != null ? (t0 - this.#expectedWake) / 1000 : 0;
    if (this.#expectedWake != null) p.wakeLateMsMax = Math.max(p.wakeLateMsMax, t0 - this.#expectedWake);
    this.#expectedWake = null;
    const now = this.#deps.now();
    // time lost since the last tick — this thread held (a dialog, a hidden tab), or the engine's clock
    // jumped (audio suspended and resumed) — over a second of it moves the schedule on by that much,
    // so the music carries on from where it was instead of racing through everything it missed
    const jumped = this.#lastNow != null ? (now - this.#lastNow) - (t0 - this.#lastWall) / 1000 : 0;
    const lost = Math.max(late, jumped);
    if (lost > HOLD_SECS) { this.#runtime.module._sp_hold(lost); p.holds = (p.holds ?? 0) + 1; p.heldSecs = (p.heldSecs ?? 0) + lost; this.#deps.held?.(lost); }
    this.#lastNow = now; this.#lastWall = t0;
    if (this.#gate && !this.#gate.waiting && now > this.#gate.start + this.schedAhead) this.#gate = null;   // started
    const next = this.#runtime.module._sp_tick(now);
    this.#drain();
    if (this.#gate?.waiting) return;   // the drain found the run's sounds waiting on loads: the clock stops here
    const t1 = performance.now();
    this.#deps.ticked?.();
    const t2 = performance.now();
    p.ticks++;
    p.tickMs += t1 - t0;
    p.tickMsMax = Math.max(p.tickMsMax, t1 - t0);
    p.statusMsMax = Math.max(p.statusMsMax, t2 - t1);
    if (next < 0) { this.running = false; this.#deps.state?.(false); return; }
    const delay = Math.max(0, (next - now) * 1000);
    this.#expectedWake = performance.now() + delay;
    this.#timer = setTimeout(() => this.tick(), delay);
  }

  /** The timings gathered since the last call, and a fresh start. */
  takePerf() {
    const p = this.perf;
    this.perf = freshPerf();
    return p;
  }

  /** One job stops where it stands; the others carry on. */
  stopJob(job) {
    this.#runtime.module._sp_stop_job(job);
    this.#drain();
    this.#deps.ticked?.();
  }

  /** A subtree stops (Scheduler#stop_subtree): a thread with everything under it, or an fx block's threads and sounds. */
  stopSubtree(uid, fade = 0) {
    this.#runtime.module._sp_stop_subtree(uid, fade, this.#deps.now());
    this.#drain();
    this.#deps.ticked?.();
    if (!this.running) { this.running = true; this.#deps.state?.(true); this.tick(); }
  }

  /**
   * A cue from outside the program (MIDI in, a game controller): into the Time State now, waking the syncs that
   * wait on it.
   */
  // The moment is read HERE, on the clock this side keeps (now(), anchored to the engine's). It is tempting to
  // stamp a cue where it arrived instead, so it keeps its own time when a burst queues up — but the page reads
  // SuperSonic's clock and this reads its own anchor, and the two are not the same base: a stamp from the page
  // would put every cue out by the difference, and the engine would play late. Worth doing, once both sides share
  // one clock; not before.
  cue(address, args = []) {
    // each value tagged with its kind, so a string (a controller's name) arrives as one: i an integer, f a float,
    // b a boolean, s a string; a unit separator between (the runtime's adapter.rb host_values reads them)
    const vals = args.map((a) => (typeof a === "number" ? `${Number.isInteger(a) ? "i" : "f"}${a}` : typeof a === "boolean" ? `b${a ? 1 : 0}` : `s${a}`)).join("\x1f");
    this.#runtime.module.ccall("sp_cue", null, ["string", "string", "number"], [address, vals, this.#deps.now()]);
    this.#drain();   // the cue's own record, before the tick empties the outbox to fill it again
    // a woken sync runs now, not at the next wake the schedule had; a program whose threads all wait on syncs
    // has stopped ticking, so the cue starts it again
    if (this.running) this.tick();
    else { this.running = true; this.#deps.state?.(true); this.tick(); }
  }

  /** A group sits under another (Scheduler#group_under): the parent's stop takes it too. */
  groupUnder(group, parent) { this.#runtime.module._sp_group_under(group, parent); }

  /** A group stops (Scheduler#stop_group): its threads now, its sounds turned down over `fade` seconds and freed after. */
  stopGroup(group, fade = 0) {
    this.#runtime.module._sp_stop_group(group, fade, this.#deps.now());
    this.#drain();
    this.#deps.ticked?.();
    if (!this.running) { this.running = true; this.#deps.state?.(true); this.tick(); }   // the frees at the fade's end are the runtime's to send
  }

  /** Every job stops where it stands; a sound still waiting on a load never goes. The session silences the engine. */
  stop() {
    clearTimeout(this.#timer);
    this.#expectedWake = null;
    this.#lastNow = null;
    this.#runtime.module._sp_stop_all();
    this.#queue.length = 0;   // what the queue held never goes, and the stop's own frees need nothing loaded
    this.#gate = null;
    this.#drain();
    this.#stops++;
    this.running = false;
    this.#deps.ticked?.();
    this.#deps.state?.(false);
  }

  /** Link's tempo, changing a schedule-ahead from now, where the next sounds are made. */
  setLinkBpm(bpm) {
    this.#runtime.module._sp_set_link_bpm(bpm, this.#deps.now() + this.schedAhead);
    if (this.running) this.tick();          // a sleeping thread may now be due sooner
  }

  /** The global time warp, in ms. */
  setTimeWarp(ms) { this.#runtime.module._sp_set_time_warp(ms / 1000); }

  /** The host lost this many seconds: the schedule moves on by them (see tick). */
  hold(seconds) { if (seconds > 0) { this.#runtime.module._sp_hold(seconds); this.#lastNow = null; if (this.running) this.tick(); } }

  /**
   * The process table, read straight out of the runtime's memory: a view,
   * not a copy, of PROCESS_FIELDS.length doubles per thread. Read it before
   * the next call rewrites it.
   */
  processTable(now = this.#deps.now()) {
    const m = this.#runtime.module;
    const ptr = m._sp_process_table(now);
    const len = m._sp_process_table_len();
    return m.HEAPF64.subarray(ptr >> 3, (ptr >> 3) + len);
  }

  /** The session's schedule-ahead, in seconds: the runtime's (defaults.rb, or set_sched_ahead_time!). */
  get schedAhead() { return this.#runtime.module._sp_sched_ahead(); }

  /** Bytes the runtime's wasm heap holds. */
  heapBytes() { return this.#runtime.module.HEAPU8.length; }

  /**
   * What the runtime's last call left in its outbox (web/osc.js): each
   * sound's bundle, handed over as it is once what it needs has loaded; the
   * runtime's word on what to load and free; and each record for the page.
   */
  #drain() {
    const m = this.#runtime.module;
    const len = m._sp_out_len();
    if (!len) return;
    // a copy: a record's handler may call the runtime, which empties its outbox
    const ptr = m._sp_out_ptr(), bytes = m.HEAPU8.slice(ptr, ptr + len), now = this.#deps.now();
    // what to load first, then the sounds: a sound's synthdef is numbered by the word to load it, and in a gate's
    // window a sound whose own things are ready still waits when a sound after it in the same tick waits on a load
    const sounds = [];
    forEachFrame(bytes, 0, len, (kind, start, size, view, offset) => {
      if (kind === FRAME_SOUND) sounds.push(this.#sound(bytes, view, start, offset));
      else if (kind === FRAME_GUI) this.#deps.record?.(bytes.subarray(start, start + size));
      else if (kind === FRAME_HOST) this.#host(decode(bytes.subarray(start, start + size)));
    });
    if (!sounds.length) return;
    if (this.#gate && !this.#gate.waiting && now > this.#gate.start + this.schedAhead) this.#gate = null;   // its window has passed
    const gating = this.#gate && !this.#gate.waiting;
    if (gating && sounds.some((q) => !this.#ready(q))) { this.#queue.push(...sounds); this.#wait(); return; }
    for (const q of sounds) {
      if (!this.#queue.length && this.#ready(q)) this.#send(q);
      else { this.#queue.push(q); this.#pump(); }
    }
  }

  // /sonic-pi/sound ,iib synthdef buffer bundle: at fixed offsets, read in place
  #sound(heap, view, start, offset) {
    const def = view.getInt32(offset + 24), buf = view.getInt32(offset + 28), size = view.getUint32(offset + 32);
    const bundle = heap.slice(start + 36, start + 36 + size);
    const starts = def >= 0;                       // an /s_new; a control or kill needs no synthdef
    return { bundle, starts, name: starts ? this.#runtime.synthdefs[def] : null, buf };
  }

  #ready({ name, buf }) {
    const loader = this.#deps.loader;
    return (name == null || loader.synthDefReady(name)) && (buf < 0 || loader.bufferReady(buf));
  }

  // true once what a sound needs has loaded; false when a load of it failed
  async #loaded({ name, buf }) {
    const loader = this.#deps.loader;
    const [def, loaded] = await Promise.all([name == null || loader.synthDef(name), buf < 0 || loader.bufferLoaded(buf)]);
    return !!(def && loaded);
  }

  // the queue, in order: each sound goes once what it needs has loaded, or is dropped when a load failed; late
  #pump() {
    if (this.#pumping || this.#gate?.waiting) return;
    this.#pumping = true;
    const stops = this.#stops;
    (async () => {
      while (this.#queue.length && stops === this.#stops && !this.#gate?.waiting) {
        const q = this.#queue[0];
        const ok = this.#ready(q) || (await this.#loaded(q));
        if (stops !== this.#stops || this.#gate?.waiting) break;
        this.#queue.shift();
        if (ok) this.#send(q);
      }
    })().finally(() => { this.#pumping = false; if (this.#queue.length && stops !== this.#stops) this.#pump(); });
  }

  // A gate's wait (see #gate): the clock stops with the run's first sounds queued; everything they need loads; a
  // settle for anything more to be asked for; then the whole schedule, and the sounds queued, move on by the time
  // waited, and the run starts, a schedule-ahead from now.
  #wait() {
    const gate = this.#gate, stops = this.#stops;
    gate.waiting = true;
    clearTimeout(this.#timer);
    gate.released = (async () => {
      // a load that failed is not asked for again here (the Bridge would try it afresh each time): its sound is dropped
      do {
        await Promise.all(this.#queue.filter((q) => !q.failed).map(async (q) => { q.failed = !(await this.#loaded(q)); }));
        await new Promise((r) => setTimeout(r, SETTLE_SECS * 1000));
      } while (stops === this.#stops && !this.#queue.every((q) => q.failed || this.#ready(q)));
    })().then(() => {
      if (stops !== this.#stops || this.#gate !== gate) return;
      const now = this.#deps.now(), shift = Math.max(0, now - gate.start);
      this.#runtime.module._sp_hold(shift);
      if (gate.first) this.t0 += shift;   // the session's first run: its records count from when it started
      const queued = this.#queue.splice(0);
      for (const q of queued) if (!q.failed && this.#ready(q)) this.#send({ ...q, bundle: retime(q.bundle, shift) });
      gate.start = now;          // the run starts now: a schedule-ahead more of the window, in case it asks for more
      gate.waiting = false;
      this.#lastNow = null; this.#expectedWake = null;   // the wait is not time lost (tick's hold)
      this.tick();
    });
  }

  #send({ bundle, starts }) {
    // its headroom as it leaves, at the time it leaves with (a gate's wait moves it on); an immediate bundle (a
    // with_fx block's groups, a loop moving) has no time to be late for
    const v = new DataView(bundle.buffer, bundle.byteOffset, bundle.byteLength), secs = v.getUint32(8);
    if (secs) countHeadroom(this.perf, secs + v.getUint32(12) / 4294967296, this.#deps.now());
    this.#deps.send(bundle);
    if (starts) this.#deps.started?.();
  }

  #host([address, number, name]) {
    this.#deps.host?.(address, number, name);
    const loader = this.#deps.loader;
    if (address === "/sonic-pi/synthdef") { this.#runtime.synthdefs[number] = name; loader.synthDef(name); }
    else if (address === "/sonic-pi/synthdef-url") loader.synthDefUrl(name);   // load_synthdef: name is the URL
    else if (address === "/sonic-pi/sample") loader.loadBuffer(number, name);
    else if (address === "/sonic-pi/sample_free") loader.freeBuffer(number, name);
  }
}
