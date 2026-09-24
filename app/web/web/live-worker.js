// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
/**
 * The live runtime in a worker of its own, so the page's main thread (its
 * drawing and layout, a busy or throttled tab) never stands between a program
 * and its sounds. The mruby runtime ticks here on the worker's own timers,
 * which browsers do not throttle as they do a page's, and each sound goes straight to SuperSonic's
 * AudioWorklet through the OscChannel the page hands over. The page keeps what
 * only it can do: loading synthdefs and samples, which this worker asks for; the
 * engine's clock, which it posts here as anchors, since a worker cannot read an
 * AudioContext; and every view of the records, which it gets in batches.
 * sonic_pi.js WorkerSession is the page's half.
 *
 * Page → worker: live {channel, clock}, clock {clock}, run {code, group},
 * stop, stopJob {job}, stopGroup {group, fade}, cue {address, args}, groupUnder {group, parent}, stopSubtree {uid, fade}, linkBpm {bpm}, timeWarp {ms}, loaded {synthdef|bufnum, ok}.
 * Worker → page: ready {version, samples} or failed {error}; reply {id, value|error};
 * batch {records, table, heap, perf, started, host, running, t0};
 * load {ops: [["synthdef", name, url?] | ["sample", bufnum, file] | ["free", bufnum, file]]};
 * error {error}.
 */
import { loadRuntime, programTables, programSynthdefUrls, SUPERSONIC_BASE } from "./runtime.js";
import { LiveCore } from "./live-core.js";
import { decode } from "./osc.js";

// What the page says before the runtime is up (the engine's egress: SuperSonic hands it over as it boots, beside
// this worker's own start rather than after it) is held here, and handled in order once it is (the foot)
const early = [];
self.onmessage = (e) => early.push(e);
postMessage({ type: "alive" });   // a worker that runs modules: the engine may hand its egress here (main.js)

const fail = (e) => postMessage({ type: "failed", error: String(e?.message ?? e) });
let OscChannel, runOscInPump, midiInDecode, runtime;
try {
  // all at once: over a slow link each is a round trip or two, and one after another they are seconds.
  // No random tables yet: :white arrives with the engine (the "live" message), the rest only if a program asks
  // for them. They are 840 kB each, and most programs draw from :white alone.
  [{ OscChannel }, { runOscInPump }, { midiInDecode }, runtime] = await Promise.all([
    import(`${SUPERSONIC_BASE}osc_channel.js`),
    import(`${SUPERSONIC_BASE}osc_in_pump.js`),
    import(`${SUPERSONIC_BASE}midi_event.js`),
    loadRuntime("./", { sources: [] }),
  ]);
} catch (e) {
  fail(e);
  throw e;
}

// ── The engine's clock, counted from the page's last anchor ──────────────
// performance.timeOrigin + performance.now() is the same clock on the page
// and here, so an anchor stays true however late its message arrives.
let anchor = null;
const wall = () => performance.timeOrigin + performance.now();
const now = () => anchor.ntp + (wall() - anchor.wall) / 1000;

// ── Loads, asked of the page: the Bridge's interface, answered by message ──
class RemoteLoader {
  #defs = new Map();          // synthdef name → promise of loaded
  #defsReady = new Set();
  #urls = new Map();          // an external synthdef's name → its URL (load_synthdef)
  #loads = new Map();         // bufnum → promise of loaded
  #buffersReady = new Set();
  #waiting = new Map();       // "d:name" / "b:bufnum" → resolve
  #ops = [];

  #ask(op) {
    if (!this.#ops.length) queueMicrotask(() => { postMessage({ type: "load", ops: this.#ops }); this.#ops = []; });
    this.#ops.push(op);
  }

  synthDef(name) {
    if (!this.#defs.has(name)) {
      this.#defs.set(name, new Promise((resolve) => this.#waiting.set(`d:${name}`, resolve))
        .then((ok) => { if (ok) this.#defsReady.add(name); return ok; }));
      this.#ask(["synthdef", name]);
    }
    return this.#defs.get(name);
  }

  synthDefReady(name) { return this.#defsReady.has(name); }

  /** load_synthdef: a synthdef from a URL, named for its file; a URL loaded again replaces the last. */
  synthDefUrl(url) {
    const name = (String(url).split(/[?#]/, 1)[0].split("/").pop() || "").replace(/\.scsyndef$/i, "");
    if (this.#urls.get(name) !== url) {
      this.#urls.set(name, url);
      this.#defsReady.delete(name);
      this.#defs.set(name, new Promise((resolve) => this.#waiting.set(`d:${name}`, resolve))
        .then((ok) => { if (ok) this.#defsReady.add(name); return ok; }));
      this.#ask(["synthdef", name, url]);
    }
    return this.#defs.get(name);
  }

  loadBuffer(bufnum, file) {
    if (!this.#loads.has(bufnum)) {
      this.#loads.set(bufnum, new Promise((resolve) => this.#waiting.set(`b:${bufnum}`, resolve))
        .then((ok) => { if (ok) this.#buffersReady.add(bufnum); return ok; }));
      this.#ask(["sample", bufnum, file]);
    }
    return this.#loads.get(bufnum);
  }

  // a buffer never asked for here was loaded before this worker knew of it: ready
  bufferReady(bufnum) { return !this.#loads.has(bufnum) || this.#buffersReady.has(bufnum); }
  bufferLoaded(bufnum) { return this.#loads.get(bufnum) ?? Promise.resolve(true); }

  freeBuffer(bufnum, file) {
    this.#ask(["free", bufnum, file]);
    this.#loads.delete(bufnum);
    this.#buffersReady.delete(bufnum);
  }

  /** What a program needs (LiveCore#needs), all asked for at once. */
  preload({ synthdefs, samples }) {
    return Promise.all([...synthdefs.map((n) => this.synthDef(n)), ...samples.map((s) => this.loadBuffer(s.bufnum, s.file))]);
  }

  /** The page's answer. */
  loaded({ synthdef, bufnum, ok }) {
    const key = synthdef != null ? `d:${synthdef}` : `b:${bufnum}`;
    const resolve = this.#waiting.get(key);
    this.#waiting.delete(key);
    resolve?.(ok);
  }
}

// ── The page's batches: records, the process table when it moved, timings ──
let records = [], recordBytes = 0, host = [], started = 0, lastTable = null, flushing = false;
const sameTable = (a, b) => a && b && a.length === b.length && a.every((v, i) => v === b[i] || (v !== v && b[i] !== b[i]));

function flush() {
  flushing = false;
  if (!core) return;
  const msg = { type: "batch", t0: core.t0, heap: core.heapBytes(), perf: core.takePerf(), running: core.running };
  const transfer = [];
  if (records.length) {
    const out = new Uint8Array(recordBytes + 4 * records.length), view = new DataView(out.buffer);
    let p = 0;
    for (const r of records) { view.setUint32(p, r.length); out.set(r, p + 4); p += 4 + r.length; }
    msg.records = out;
    transfer.push(out.buffer);
    records = [];
    recordBytes = 0;
  }
  const table = core.processTable(now()).slice();
  if (!sameTable(table, lastTable)) {
    lastTable = table.slice();
    msg.table = table;
    transfer.push(table.buffer);
  }
  if (host.length) { msg.host = host; host = []; }
  if (started) { msg.started = started; started = 0; }
  postMessage(msg, transfer);
}
// after the tick that asked, once it has counted itself
const flushSoon = () => { if (!flushing) { flushing = true; queueMicrotask(flush); } };

// ── The engine's egress, drained here ─────────────────────────────────────
// Everything the engine says comes off one ring, and that ring has one reader. This is that reader, run where
// the acting happens, so a controller's message never touches the thread that draws on its way to the thread
// that acts on it: a MIDI event becomes a cue without leaving this worker, and the rest — the engine's replies,
// its pushes, what the page's client is waiting for — is passed on to the page.
const MIDI_IN = "/clockwork/midi/in/";
const PAD_IN = "/clockwork/gamepad/in/", PAD_DEVICES = "/clockwork/gamepad/devices";
const addressOf = (bytes) => { let n = 0; while (n < bytes.length && bytes[n] !== 0) n++; return n > 64 ? "" : String.fromCharCode.apply(null, bytes.subarray(0, n)); };

function startEgress(port) {
  runOscInPump({
    name: "SonicPiEgress",
    endpoint: port,
    onFrames: (messages) => {
      // Split first, and send the page's on their way BEFORE running any of ours. Nothing the engine says is
      // for the runtime — its channel only writes — so these are the page's: a refusal to log, the ports, and
      // the sends that make MIDI leave a port. Handling a cue means a tick, and a tick can be long; the page
      // should not wait behind it for something that was never ours.
      let keep = null, mine = null, pads = null;
      for (const m of messages) {
        const address = m.oscData ? addressOf(m.oscData) : "";
        if (address.startsWith(MIDI_IN)) (mine ??= []).push(m.oscData);
        else if (address.startsWith(PAD_IN)) (pads ??= []).push(m.oscData);
        else {
          if (address === PAD_DEVICES) padDevices(m.oscData);   // a cue here, and the page's list of controllers
          (keep ??= []).push(m);
        }
      }
      if (keep) port.postMessage({ type: "messages", messages: keep });
      if (pads) { for (const b of pads) padIn(b); }
      // A cue from outside, at the address Sonic Pi knows it by. What midiIn will not take (a tempo from a
      // clock) goes on to the page after all.
      if (mine) { let late = null; for (const b of mine) if (!midiIn(b)) (late ??= []).push({ oscData: b }); if (late) port.postMessage({ type: "messages", messages: late }); }
    },
  });
}

// True when this worker has taken the message: only what it can act on. A tempo from a clock, and the ports,
// are the page's business and go on to it.
function midiIn(bytes) {
  if (!core) return false;
  // clockwork's own reading of its own event: the fields, without the arrival timetag, exactly as a consumer
  // on a native host reads them.
  const msg = midiInDecode(bytes, decode);
  if (!msg) return false;
  const [kind, port, channel, ...values] = msg;
  // A tempo is the page's to show, and a message with no channel (transport, sysex) has no Sonic Pi address
  if (kind === "clock_bpm" || typeof port !== "string" || typeof channel !== "number") return false;
  core.cue(`/midi:${port}:${channel}/${kind}`, values);
  flushSoon();
  return true;
}

// ── Game controllers, as native's (app/server/ruby/lib/sonicpi/gamepad_api.rb) ─────────────────────────
// The engine's front reads the Gamepad API and puts every change on the egress, deduplicated, deadzoned and
// quantised: "/clockwork/gamepad/in/button <pad> <button> <pressed> <value>" and "…/in/axis <pad> <axis> <value>".
// Each becomes the cue native makes of it, so a controller is sync-able as a MIDI device is:
//   /gamepad:<pad>/button/<name>        [pressed, value]  every change
//   /gamepad:<pad>/button/<name>/down   [value]           the press
//   /gamepad:<pad>/button/<name>/up     [value]           the release
//   /gamepad:<pad>/axis/<name>          [value]           a stick moving (-1..1, up and right positive)
//   /clockwork/gamepad/devices          [name, …]         a controller connecting or leaving
const padPressed = new Map();   // "pad\0button" → held, for the edges
function padIn(bytes) {
  if (!core) return;
  const [address, ...args] = decode(bytes);
  if (address === `${PAD_IN}button`) {
    const [pad, button, pressed, value] = args;
    const base = `/gamepad:${pad}/button/${button}`;
    core.cue(base, [pressed, value]);
    const key = `${pad}\0${button}`, was = padPressed.get(key);
    padPressed.set(key, pressed === 1);
    if (pressed === 1 && was !== true) core.cue(`${base}/down`, [value]);
    else if (pressed === 0 && was) core.cue(`${base}/up`, [value]);
  } else if (address === `${PAD_IN}axis`) {
    const [pad, axis, value] = args;
    core.cue(`/gamepad:${pad}/axis/${axis}`, [value]);
  } else return;
  flushSoon();
}
// "<n> [name enabled]*": the pads there now. A pad unplugged with a button held gets no release, so its held
// buttons are forgotten, or the first press after it came back would make no /down
function padDevices(bytes) {
  if (!core) return;
  const [, , ...pairs] = decode(bytes);
  const names = [];
  for (let i = 0; i + 1 < pairs.length; i += 2) names.push(String(pairs[i]));
  for (const key of padPressed.keys()) if (!names.includes(key.split("\0")[0])) padPressed.delete(key);
  core.cue(PAD_DEVICES, names);
  flushSoon();
}

const loader = new RemoteLoader();
let core = null, channel = null;
const deps = {
  now,
  send: (bundle) => { if (!channel.send(bundle)) postMessage({ type: "error", error: "the engine did not take a bundle (its ring is full?)" }); },
  loader,
  record: (bytes) => { records.push(bytes.slice()); recordBytes += bytes.length; },
  host: (address, number, name) => host.push([address, number, name]),
  started: () => { started++; },
  ticked: flushSoon,
};

const reply = (id, value, error) => postMessage({ type: "reply", id, value, error });

// A program's own synths: before the run that loads them, each .scsyndef's metadata installed (runtime.js), so the
// synth is a standard one from its first note, and the page told what it now knows of each (its docs, completion and
// dials), once per change of what it knows
const toldSynths = new Map();   // url → what the page was last told
async function installSynths(code) {
  const got = await Promise.all(programSynthdefUrls(code).map((u) => runtime.installSynthMeta(u)));
  const fresh = got.filter((x) => { const said = JSON.stringify(x); if (toldSynths.get(x.url) === said) return false; toldSynths.set(x.url, said); return true; });
  if (fresh.length) postMessage({ type: "synthMeta", metas: fresh.map(({ text, ...x }) => x) });
}

// the engine, once the channel the page handed over is open (SuperSonic opens
// its shared memory first: fromTransferable resolves, though its typings say it returns)
let live = null;

const handle = async ({ data: d }) => {
  try {
    switch (d.type) {
      case "egress":   // the engine's reader lives here (bootEngine); the engine is booting, so :white is wanted
        runtime.installTable("white").catch(() => {});   // on its way now, beside the engine's own start ("live" awaits it)
        return startEgress(d.port);
      case "live":
        anchor = d.clock;
        core = new LiveCore(runtime, deps);
        // :white, the source every program starts on, arrives with the engine — and is part of what `live`
        // means, so a message that lands while it is still coming waits for it rather than running without it
        live = Promise.all([
          Promise.resolve(OscChannel.fromTransferable(d.channel)).then((c) => { channel = c; }),
          runtime.installTable("white"),
        ]);
        setInterval(flushSoon, 250);          // the process table lingers and prunes as time goes on
        await live;
        return reply(d.id, true);
      case "clock": anchor = d.clock; return;
      case "loaded": return loader.loaded(d);
    }
    await live;                               // everything else waits for the engine
    switch (d.type) {
      case "run": {
        // a source other than :white is fetched before the run starts, never in the middle of it
        await Promise.all(programTables(d.code).map((t) => runtime.installTable(t)));
        await installSynths(d.code);
        const job = await core.run(d.code, { group: d.group, preload: (needs) => loader.preload(needs) });
        flush();
        return reply(d.id, { job, t0: core.t0 });
      }
      case "stop": core.stop(); flush(); return reply(d.id, true);
      case "stopJob": core.stopJob(d.job); return;
      case "stopGroup": core.stopGroup(d.group, d.fade); flush(); return;
      // flushSoon, not flush: a flush builds the whole process table, and doing that for every message a
      // controller sends would put the worker behind its own inbox — messages queue up unseen and the sounds keep
      // coming seconds after the knob stops. The table is a view; it goes out with the next frame.
      case "cue": core.cue(d.address, d.args); flushSoon(); return;
      case "groupUnder": core.groupUnder(d.group, d.parent); return;
      case "stopSubtree": core.stopSubtree(d.uid, d.fade); flush(); return;
      case "linkBpm": core.setLinkBpm(d.bpm); return;
      case "timeWarp": core.setTimeWarp(d.ms); return;
      case "hold": core.hold(d.seconds); return;
    }
  } catch (e) {
    if (d.id != null) reply(d.id, undefined, String(e?.message ?? e));
    else postMessage({ type: "error", error: String(e?.stack ?? e) });
  }
};

self.onmessage = handle;
for (const e of early.splice(0)) handle(e);   // what came while the runtime was loading, in the order it came
postMessage({ type: "ready", version: runtime.version, samples: runtime.samples });
