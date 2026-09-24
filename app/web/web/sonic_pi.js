// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
/**
 * Sonic Pi in the browser, the two halves and the bridge between them:
 * the mruby runtime (the language, as wasm) and SuperSonic (scsynth, in an
 * AudioWorklet). The runtime encodes each sound as the OSC bundle the engine
 * plays; the bridge loads what the bundles need first, and the session carries
 * the runtime's other records to the page.
 *
 *   const runtime = await loadLiveRuntime();         // the language, in a worker of its own where it can be
 *   const engine  = await bootEngine();              // the audio
 *   const live    = createLiveSession(runtime, engine, { output, log, error, status });
 *   await live.run(code);  await live.run(other);  live.stop();   // runs are jobs, as in Sonic Pi
 *
 *   (await loadRuntime()).trace(code)                // NRT, on the page: the whole schedule at once
 *
 * Shared by the app (index.html) and the spec browser (specs.html).
 */
import { SuperSonic } from "./supersonic/supersonic.js";
import { decode } from "./osc.js";
import { createRecordReader } from "./gui-stream.js";
import { SUPERSONIC_BASE, supersonicInfo, PROCESS_FIELDS, programNeeds, workerSettled } from "./runtime.js";
import { LiveCore, freshPerf, addPerf, countHeadroom } from "./live-core.js";

const JOB_MIXER_NODE = 1003;   // the run mixer, sonic-pi-basic_mixer (Scheduler::JOB_MIXER_NODE): what a Stop fades
const STOP_FADE = 1;           // seconds

export { loadRuntime, programNeeds, PROCESS_FIELDS, SAMPLES_DIR, SUPERSONIC_VERSION, supersonicVersion } from "./runtime.js";

/**
 * A clockwork verb for the engine to carry out at a time on its clock.
 *
 * A bundle is NOT the way, though it looks like it should be: a bundle has no single address to route on
 * (clockwork_prefix.h), so clockwork hands every bundle to the DSP — and a "/clockwork/…" verb inside one is
 * given to scsynth, which has nothing to do with it, and is never seen again. The flat twin is what carries a
 * time: "/clockwork/schedule <timetag> <blob>", which clockwork holds and delivers itself. It was written for
 * this exact case — audio_processor.cpp says handing it to a DSP "would move timed MIDI output into an engine
 * that has no MIDI port".
 */
export function oscSchedule(time, address, args) {
  const secs = Math.floor(time);
  const timetag = (BigInt(secs) << 32n) | BigInt(Math.round((time - secs) * 4294967296));   // NTP: seconds, then a fraction of 2^32
  return SuperSonic.osc.encodeMessage("/clockwork/schedule", [
    { type: "int64", value: timetag },
    SuperSonic.osc.encodeMessage(address, args),   // a Uint8Array rides as the blob
  ]);
}

// Sonic Pi's synthdefs served beside the app, and which of them are loaded from here rather than the CDN (bootEngine)
const OWN_SYNTHDEFS = new URL("./synthdefs/", import.meta.url).href;
let ownSynthdefs = null;

/** SuperSonic, from where version.json says it is (runtime.js supersonicInfo), playing Sonic Pi's own synthdefs. */
/** beforeInit(engine): listen before it boots, to hear what it says while booting. */
export async function bootEngine(opts = {}, { beforeInit, runtime } = {}) {
  const info = await supersonicInfo();
  // The synths are Sonic Pi's own (etc/synthdefs/compiled), always: what the desktop app plays, the web plays. From
  // the CDN where its copy is that one byte for byte, which the build checked (scripts/lib/runtime-assets.mjs); the
  // rest (ownSynthdefs: one SuperSonic leaves out, one that has drifted) and all of them without that check, beside
  // the app. A stale copy plays differently or not at all: 0.85.0's autotuner predates the pitch tracker's fix, and
  // is silent.
  const cdnDefs = info.synthdefs && Array.isArray(info.ownSynthdefs);
  ownSynthdefs = cdnDefs ? new Set(info.ownSynthdefs) : null;
  // ONE READER for the engine's OUT ring, and it is the runtime's worker when there is one. The transport
  // speaks its init/start/stop protocol down this port rather than spawning the pump worker it would
  // otherwise own, so a cue from a controller is acted on in the worker instead of being handed to this
  // thread and passed straight back out again. What the worker does not keep comes here over the same port.
  // It is given now, not with the session: the transport waits for its reader while it boots.
  const egress = runtime?.worker && globalThis.MessageChannel ? new MessageChannel() : null;
  if (egress) runtime.worker.postMessage({ type: "egress", port: egress.port2 }, [egress.port2]);
  const engine = new SuperSonic({
    ...(egress ? { oscInEndpoint: egress.port1 } : {}),
    // the dist/ layout: workers/, wasm/, synthdefs/ and samples/ under the one base,
    // unless version.json names others: the CDN's client and core packages, or its assets
    baseURL: info.base ?? SUPERSONIC_BASE,
    // the core package holds the wasm and the worklet; the client derives its wasm path from baseURL, so it is named too
    ...(info.core ? { coreBaseURL: info.core, wasmBaseURL: `${info.core}wasm/` } : {}),
    ...(info.samples ? { sampleBaseURL: info.samples } : {}),
    synthdefBaseURL: cdnDefs ? info.synthdefs : OWN_SYNTHDEFS,   // above; Bridge#synthDef names the app's own
    // SAB mode where the page is cross-origin isolated (scripts/serve.mjs sends the
    // headers): the scope streams and audio capture read the engine's shared memory,
    // as native's client does. Elsewhere postMessage, which needs no headers.
    mode: globalThis.crossOriginIsolated ? "sab" : "postMessage",
    // a GET alone for each synthdef and sample: the HEAD beside it (for a size to show while it loads, which nothing
    // here shows) is never answered from the browser's cache, so it costs a round trip on every load of every asset
    skipHeadRequests: true,
    // game controllers, on as they are natively: the Gamepad API asks for no permission (every browser has it,
    // Safari too, which has no Web MIDI). Their events are the runtime worker's cues (live-worker.js padIn)
    gamepad: !!globalThis.navigator?.getGamepads,
    ...opts,
    // with_fx blocks take a pair each (the runtime's Scheduler::BUS_LAST)
    scsynthOptions: { numAudioBusChannels: 1024, ...opts.scsynthOptions },
  });
  beforeInit?.(engine);
  await engine.init();
  return engine;
}

// ── A trace, as HTML ─────────────────────────────────────────────────────

export function escapeHTML(s) {
  return String(s).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
}

function fmtArgs(args) {
  return Object.entries(args || {}).map(([k, v]) => `${k}: ${typeof v === "number" ? +v.toFixed(4) : JSON.stringify(v)}`).join(", ");
}

/** The events as a table, then errors, output and log. Rows are `${id}-ev-${i}`. */
export function traceHTML(trace, id = "trace") {
  const esc = escapeHTML;
  const rows = trace.events.map((e, i) => {
    const what = e.kind === "synth" ? e.synth : `load ${e.path}`;
    const skipped = e.kind === "synth" && e.synth.startsWith("sonic-pi-fx_");
    return `<tr id="${id}-ev-${i}" class="${skipped ? "skipped" : ""}"><td class="t">${e.t.toFixed(3)}</td><td>${e.beat ?? ""}</td><td>${esc(e.thread)}${e.name ? ` <span class="kv">${esc(e.name)}</span>` : ""}</td><td>${esc(what)}${skipped ? ' <span class="kv">(fx: not played here)</span>' : ""}</td><td>${esc(fmtArgs(e.args))}</td></tr>`;
  }).join("");
  const list = (title, items, f) => items.length ? `<h3 style="margin-top:10px">${title}</h3><pre tabindex="0">${items.map(f).map(esc).join("\n")}</pre>` : "";
  return `
    ${trace.events.length ? `<div class="trace-scroll" style="overflow-x:auto" tabindex="0" role="region" aria-label="${id} events"><table><thead><tr><th>t (s)</th><th>beat</th><th>thread</th><th>event</th><th>args</th></tr></thead><tbody>${rows}</tbody></table></div>` : '<p class="empty">no events</p>'}
    ${list("Errors", trace.errors, (e) => `${e.class}: ${e.message}  (line ${e.line}, thread ${e.thread}${e.name ? " " + e.name : ""})`)}
    ${list("Output", trace.output, (o) => `${o.t.toFixed(3)}  ${o.thread}${o.name ? " " + o.name : ""}  ${o.text}`)}
    ${list("Log", trace.log, (o) => `${o.t.toFixed(3)}  ${o.thread}${o.name ? " " + o.name : ""}  ${o.text}`)}
  `;
}

// ── The bridge ───────────────────────────────────────────────────────────

// load_synthdef's URL → the synthdef's name: its file's, less .scsyndef (the runtime names it the same way, lang_more.rb)
export const synthdefNameOf = (url) => (String(url).split(/[?#]/, 1)[0].split("/").pop() || "").replace(/\.scsyndef$/i, "");
// the synths every run needs, which no program names: they fail alongside whatever else could not load
const INTERNAL_SYNTHS = new Set(["sonic-pi-mixer", "sonic-pi-basic_mixer", "sonic-pi-fx_scope_out"]);

/** A synthdef's name as a program writes it: sonic-pi-fx_reverb is `with_fx :reverb`, sonic-pi-beep is `:beep`. */
const synthLabel = (name) => {
  const bare = String(name).replace(/^sonic-pi-/, "");
  return bare.startsWith("fx_") ? `FX :${bare.slice(3)}` : `:${bare}`;
};

// why a URL's synthdef did not load, in words a person can act on
function loadProblem(url, e) {
  const m = String(e?.message ?? e);
  const status = /:\s(\d{3})\b\s*(.*)$/.exec(m);   // the loader's "Failed to fetch <url>: 404 Not Found"
  if (status?.[1] === "404") return `nothing found at ${url} (404)`;
  if (status) return `${url} answered ${status[1]}${status[2] ? ` ${status[2]}` : ""}`;
  if (/failed to fetch|load failed|networkerror|cors/i.test(m)) return `could not fetch ${url}: the site may not allow it to be loaded from another site (it needs to allow cross-origin requests; a GitHub raw link does), or it could not be reached`;
  if (/name|scsyndef|parse/i.test(m)) return `${url} is not a synthdef (.scsyndef) file`;
  return `could not load ${url}: ${m}`;
}

/**
 * What the runtime emits becomes what the engine plays. Shared by live runs
 * and trace playback, so a synthdef or a sample is loaded once.
 */
export class Bridge {
  #engine;
  #defs = new Map();          // synthdef name → promise of loaded
  #defsReady = new Set();
  #urls = new Map();         // an external synthdef's name → the URL it loads from (load_synthdef)
  #errors = new Map();       // name → what went wrong loading it, for the error pane
  #buffers = new Map();       // sample file → bufnum
  #loads = new Map();         // bufnum → promise of loaded
  #buffersReady = new Set();
  #nextOwnBuffer = 1023;      // numbered here, counting down, only when no runtime numbers them
  #piano = null;              // the :piano synth's sample table, handed to the engine once (pianoTable)
  #fade = null;               // a Stop's fade in progress: { started, done, cut } (fadeOut)
  /** file → bufnum from a live runtime on the page (LiveSession sets it), so a preload and the runtime's sounds agree. */
  numberBuffer = null;
  sent = 0;

  constructor(engine) {
    this.#engine = engine;
    // A reload (recover() finding the worklet gone) tears the engine down and
    // builds it again; restoreClientState is its one restore path, and
    // SuperSonic's own puts the synthdefs and sample buffers back. What is
    // this bridge's — the piano table, in the plugin's memory — goes back after.
    const base = engine.restoreClientState?.bind(engine);
    engine.restoreClientState = async () => { await base?.(); await this.restorePiano(); };
  }

  synthDef(name) {
    if (!this.#defs.has(name)) {
      // :piano's synthdef is not enough on its own: its plugin needs its table too (below); one from a URL
      // (load_synthdef) is fetched from there, and its own name read from it
      const url = this.#urls.get(name);
      // one of Sonic Pi's the CDN does not hold as it is here: from beside the app (bootEngine)
      const own = !url && ownSynthdefs?.has(name) ? `${OWN_SYNTHDEFS}${name}.scsyndef` : null;
      this.#defs.set(name, this.#engine.loadSynthDef(url ?? own ?? name).then(async (r) => {
        if (name === "sonic-pi-piano") await this.pianoTable();
        if (url && r?.name && r.name !== name) this.#errors.set(name, `${url} holds a synthdef named :${r.name}, not :${name}: play :${r.name}, or name the file ${r.name}.scsyndef`);
        return true;
      })
        .catch((e) => {
          if (/already/i.test(e?.message || "")) return true;
          console.warn("synthdef", name, e);
          // A built-in that will not load is silence with no reason given — the sound is sent, the scheduler is happy,
          // and nothing comes out. Its URL is the loader's to choose, so it is taken from the message it threw.
          const from = url ?? /failed to fetch (.+?):\s*\d{3}\b/i.exec(String(e?.message ?? e))?.[1] ?? `${name}.scsyndef`;
          this.#errors.set(name, url ? loadProblem(url, e)
            : `the synth ${synthLabel(name)} could not be loaded — ${loadProblem(from, e)}. Nothing will sound until it does.`);
          return false;
        })
        .then((ok) => {
          if (ok) this.#defsReady.add(name);
          else this.#defs.delete(name);   // not cached as failed for the life of the page: the next Run asks again, so a server or network that comes back is heard
          return ok;
        }));
    }
    return this.#defs.get(name);
  }

  /** A synthdef from a URL (load_synthdef): named for its file, as SuperCollider's writeDefFile names it. A URL
   *  loaded again (a new version) replaces the last. */
  synthDefUrl(url) {
    const name = synthdefNameOf(url);
    if (this.#urls.get(name) !== url) { this.#urls.set(name, url); this.#defs.delete(name); this.#defsReady.delete(name); this.#errors.delete(name); }
    return this.synthDef(name);
  }

  /** What went wrong loading a URL's synthdef, said for a person, or null. */
  synthDefProblem(name) { return this.#errors.get(name) ?? null; }

  synthDefReady(name) { return this.#defsReady.has(name); }

  /** After the engine reloads: its plugin memory is new, so a piano table it had is handed over again. */
  restorePiano() {
    if (!this.#piano) return Promise.resolve(false);
    this.#piano = null;
    return this.pianoTable();
  }

  /**
   * The :piano synth's sample table, as native's Studio#load_piano_wavetable:
   * MdaPiano's ~1.1MB table lives outside the engine, so it goes in the way a
   * sample does — the raw 16-bit integers given a WAV header, into a buffer —
   * and /supersonic/piano/wavetable points the plugin at that buffer, which
   * copies it; the buffer goes again once the engine says /done. Until then
   * :piano is silent. Loaded once, the first time a program wants the synth.
   */
  pianoTable() {
    this.#piano ??= (async () => {
      const res = await fetch("data/piano_wavetable.dat");
      if (!res.ok) throw new Error(`piano_wavetable.dat: ${res.status}`);
      const data = new Uint8Array(await res.arrayBuffer());
      // the header rate is nominal (the plugin indexes the table itself), but the browser decodes a
      // sample to the context rate: at any other rate it would resample, and stretch the table
      const rate = Math.round(this.#engine.audioContext?.sampleRate ?? 44100);
      const wav = new Uint8Array(44 + data.length), v = new DataView(wav.buffer);
      const str = (o, t) => { for (let i = 0; i < t.length; i++) wav[o + i] = t.charCodeAt(i); };
      str(0, "RIFF"); v.setUint32(4, 36 + data.length, true); str(8, "WAVE");
      str(12, "fmt "); v.setUint32(16, 16, true); v.setUint16(20, 1, true); v.setUint16(22, 1, true);
      v.setUint32(24, rate, true); v.setUint32(28, rate * 2, true); v.setUint16(32, 2, true); v.setUint16(34, 16, true);
      str(36, "data"); v.setUint32(40, data.length, true);
      wav.set(data, 44);
      const bufnum = this.#nextOwnBuffer--;
      await this.#engine.loadSample(bufnum, wav.buffer);
      const answered = new Promise((resolve) => {
        const h = (m) => { if ((m?.[0] === "/done" || m?.[0] === "/fail") && m?.[1] === "/supersonic/piano/wavetable") { this.#engine.off?.("in", h); resolve(m[0] === "/done"); } };
        this.#engine.on?.("in", h);
        setTimeout(() => { this.#engine.off?.("in", h); resolve(true); }, 5000);
      });
      this.#engine.send("/supersonic/piano/wavetable", bufnum);
      const ok = await answered;
      this.#engine.send("/b_free", bufnum);
      if (!ok) console.warn("the engine refused the piano wavetable: :piano will be silent");
      return ok;
    })().catch((e) => { console.warn("piano wavetable", e); return false; });
    return this.#piano;
  }

  /** A sample file into a buffer number, once. */
  loadBuffer(bufnum, file) {
    this.#buffers.set(file, bufnum);
    if (!this.#loads.has(bufnum)) {
      this.#loads.set(bufnum, this.#engine.loadSample(bufnum, file)
        .then(() => { this.#buffersReady.add(bufnum); return true; })
        .catch((e) => { console.warn("sample", file, e); return false; }));
    }
    return this.#loads.get(bufnum);
  }

  // a buffer this bridge never loaded was loaded before it was made: ready
  bufferReady(bufnum) { return !this.#loads.has(bufnum) || this.#buffersReady.has(bufnum); }
  bufferLoaded(bufnum) { return this.#loads.get(bufnum) ?? Promise.resolve(true); }

  /** A sample by file: numbered by the live runtime when there is one, loaded once. */
  sample(file) {
    const bufnum = this.#buffers.get(file) ?? this.numberBuffer?.(file) ?? this.#nextOwnBuffer--;
    return this.loadBuffer(bufnum, file);
  }

  /** A buffer freed: the engine lets it go, and the next use loads it again. */
  freeBuffer(bufnum, file) {
    this.#engine.send("/b_free", bufnum);
    if (this.#buffers.get(file) === bufnum) this.#buffers.delete(file);
    this.#loads.delete(bufnum);
    this.#buffersReady.delete(bufnum);
  }

  /** Starts loading what a program is likely to need before it runs (runtime.js programNeeds). */
  preload(code, sampleFiles, synthdefFor) {
    const { synthdefs, samples } = programNeeds(code, sampleFiles, synthdefFor);
    return Promise.all([...synthdefs.map((n) => this.synthDef(n)), ...samples.map((f) => this.sample(f))]);
  }

  /** One synth of a trace, sent as a bundle for its time (an NTP instant). */
  async synth(ev, time) {
    if (ev.synth.startsWith("sonic-pi-fx_")) return false;      // fx routing is the studio's, not the trace's
    if (!(await this.synthDef(ev.synth))) return false;
    const args = [];
    for (const [k, v] of Object.entries(ev.args || {})) {
      if (k === "buf") { await this.sample(v); args.push("buf", this.#buffers.get(v) ?? 0); }
      else if (typeof v === "number") args.push(k, v);
    }
    this.#engine.sendOSC(SuperSonic.osc.encodeBundle(time, [["/s_new", ev.synth, -1, 0, 0, ...args]]));
    this.sent++;
    return true;
  }

  /** A whole trace, from now plus a lead. Returns the number of synths sent. */
  async playTrace(trace, lead = 0.3, onEvent = null) {
    await Promise.all(trace.events.filter((e) => e.kind === "sample_load").map((e) => this.sample(e.path)));
    await Promise.all([...new Set(trace.events.filter((e) => e.kind === "synth").map((e) => e.synth))].map((s) => this.synthDef(s)));
    const start = this.#engine.clock.now() + lead;
    let n = 0;
    for (const [i, e] of trace.events.entries()) {
      if (e.kind !== "synth") continue;
      const t = Math.max(0, e.t);
      if (onEvent) setTimeout(() => onEvent(e, i), (lead + t) * 1000);
      if (await this.synth(e, start + t)) n++;
    }
    return n;
  }

  /**
   * Everything stops: what the engine still holds scheduled, then what is
   * sounding. In that order, and awaited: the purge clears the scheduler and
   * the IN ring, and in SAB mode a free-all sent before it has landed goes
   * into that ring and is wiped with it (the old job mixer then lives on, and
   * the next Run's first bundles can go the same way). Resolves once the
   * free-all is sent.
   */
  silence() {
    return this.purge().then(() => { try { this.#engine.send("/g_freeAll", 0); } catch { /* being rebuilt: nothing to free */ } });
  }

  /** What the engine still holds scheduled is dropped; what sounds carries on. */
  purge() {
    // an engine mid-reload has no worklet to purge nor a channel to send on: the runtime still stops
    return Promise.resolve().then(() => this.#engine.purge?.()).catch(() => {});
  }

  /**
   * A Stop as the ear wants it: nothing more is scheduled, what is sounding
   * fades to silence through the run mixer over STOP_FADE seconds, and then
   * everything is freed. started resolves once the fade is under way, done
   * once the free-all is sent; cut() frees at once instead (a Run mid-fade
   * takes the silence now, so its studio is not made beside the old one).
   */
  fadeOut(seconds = STOP_FADE) {
    if (this.#fade) return this.#fade;
    let settle, timer;
    const done = new Promise((r) => (settle = r));
    const fade = { done, cut: () => { if (this.#fade !== fade) return; clearTimeout(timer); this.#fade = null; this.silence().then(settle, settle); } };
    fade.started = this.purge().then(() => { try { this.#engine.send("/n_set", JOB_MIXER_NODE, "amp", 0, "amp_slide", seconds); } catch { /* being rebuilt */ } });
    timer = setTimeout(fade.cut, seconds * 1000 + 80);
    return (this.#fade = fade);
  }

  /** A fade in progress ends now: silence at once. */
  cutFade() { this.#fade?.cut(); }
}

// ── Live ─────────────────────────────────────────────────────────────────

/**
 * Which runs have threads waiting, the named threads alive, and where each
 * waiting thread is: from the process table, named from the records.
 */
function statusFrom(table, records) {
  const width = PROCESS_FIELDS.length, GROUP = PROCESS_FIELDS.indexOf("group");
  const jobs = new Set(), named = [], threads = [], groups = new Set();
  let sleeping = 0, waiting = 0;
  for (let i = 0; i + width <= table.length; i += width) {
    const uid = table[i], job = table[i + 2], kind = table[i + 3], state = table[i + 4], line = table[i + 5], group = table[i + GROUP];
    // a group is live while a thread of it runs, sleeps or waits, an fx of it is open, or a sound of it sounds (Scheduler#stop_group)
    if (group > 0 && ((kind >= 1 && kind <= 5 && state <= 2) || (kind === 6 && state < 3) || ((kind === 7 || kind === 8) && state === 0))) groups.add(group);   // a group's own row (9) says the same, from the runtime
    if (kind === 0 || kind >= 6 || state > 2) continue;   // a run's own row; an fx or a sound; a thread done, failed or stopped
    const t = records.thread(uid);
    if (t?.name) named.push(t.name);
    if (state === 0) continue;
    jobs.add(job);
    if (state === 1) sleeping++; else waiting++;
    threads.push({ id: t?.id ?? "", name: t?.name ?? "", job, state: state === 1 ? "sleeping" : "waiting",
      beat: table[i + 7], bpm: table[i + 8], wake: state === 1 ? table[i + 6] : undefined, on: state === 2 ? t?.on ?? undefined : undefined, line: line >= 0 ? line : undefined });
  }
  return { jobs: [...jobs].sort((a, b) => a - b), named: named.sort(), sleeping, waiting, threads, groups: [...groups].sort((a, b) => a - b) };
}

// A record more than two seconds behind the engine's clock is the past: the page was held (a dialog, a
// background tab) and the runtime's batches queued up. The page keeps what it keeps for later (the recorder, the
// threads view: `stale` says so) but paints nothing for it, so the catch-up costs a moment, not a freeze. Errors show.
const STALE_SECS = 2;
/** One record to the page's handlers, timed into perf. */
function deliver(r, perf, on, clockNow, at) {
  perf.records++;
  if (r.kind === "midi") countHeadroom(perf, r.time, clockNow());
  const stale = r.kind !== "error" && r.time != null && clockNow() - r.time > STALE_SECS;
  const t0 = performance.now();
  on.record?.(r, at(r), stale);
  perf.recordMsMax = Math.max(perf.recordMsMax, performance.now() - t0);
  if (r.kind === "midi") return void on.midi?.(r);
  if (stale) return;
  if (r.kind === "output") return on.output?.(r, at(r));
  if (r.kind === "log") return on.log?.(r, at(r));
  if (r.kind === "error") return on.error?.(r, at(r));
}

const clampBpm = (bpm) => Math.min(999, Math.max(20, bpm));   // native's BPMScrubWidget range

/**
 * The language for live runs, in a worker of its own when the browser can
 * (live-worker.js: the page's main thread then never delays a sound), else
 * on the page (loadRuntime). Either way: {version, samples}, and a worker or
 * the runtime's module for createLiveSession.
 */
export async function loadLiveRuntime(base = "./", { worker: started = null, settled = null } = {}) {
  if (started || typeof Worker === "function") {
    let worker = started;
    try {
      // the page's worker, started at the first sign of sound, before this module had arrived (main.js), with
      // `settled` its first word (ready or failed), heard from its start: a worker quicker than this module would
      // otherwise say "ready" to no one, and the runtime would load for ever. Else a worker now, heard from now.
      worker ??= new Worker(new URL("live-worker.js", new URL(base, location.href)), { type: "module", name: "sonic-pi runtime" });
      const info = await (started && settled ? settled : workerSettled(worker))
        .then((d) => (d.type === "ready" ? d : Promise.reject(new Error(d.error))));
      return { worker, version: info.version, samples: info.samples };
    } catch (e) {
      worker?.terminate();
      console.warn(`the runtime could not run in a worker, so it runs on the page: ${e.message}`);
    }
  }
  const { loadRuntime } = await import("./runtime.js");
  return loadRuntime(base, { sources: ["white"] });   // as in the worker: the other tables wait until a program asks
}

/** A live session for loadLiveRuntime's runtime: in its worker when it has one, else on the page. */
export function createLiveSession(runtime, engine, on = {}, bridge = null) {
  return runtime.worker && typeof engine.createOscChannel === "function"
    ? new WorkerSession(runtime, engine, on, bridge)
    : new LiveSession(runtime, engine, on, bridge);
}

/**
 * A live session on the page: the runtime against the engine's clock, on the
 * page's own timers (live-core.js). The spec browser's, and the app's where a
 * worker cannot be had.
 */
export class LiveSession {
  #runtime; #engine; #on; #core; #records = createRecordReader(); #lastStatus = "";
  /** scsynth's /fail replies since the session began: messages the engine refused. */
  failures = 0;
  bridge;
  /** Link's tempo (the strip's, tap tempo's, or a program's set_link_bpm!). */
  linkBpm = 60;

  /**
   * @param {{output?, log?, error?, sent?, status?, state?, record?, midi?, fail?, host?}} on handlers:
   *   output/log/error(record, secondsSinceFirstRun), sent(count),
   *   status({jobs, named, sleeping, waiting, threads}), state(running),
   *   record(r): every record as it is made (synths, controls, sleeps, syncs,
   *   cues, threads starting and ending, with the line each came from),
   *   midi(r): a MIDI message to send at r.time, fail(msg): scsynth refused one,
   *   host(address, number, name): the runtime's word on what to load and free
   * @param {Bridge} [bridge] share one with trace playback
   */
  constructor(runtime, engine, on = {}, bridge = null) {
    this.#runtime = runtime;
    this.#engine = engine;
    this.#on = on;
    this.bridge = bridge ?? new Bridge(engine);
    this.#core = new LiveCore(runtime, {
      now: () => engine.clock.now(),
      send: (bundle) => engine.sendOSC(bundle),
      loader: this.bridge,
      record: (bytes) => deliver(this.#records.read(decode(bytes)), this.#core.perf, on, () => this.clockNow(), (r) => this.at(r)),
      host: (address, number, name) => on.host?.(address, number, name),
      started: () => { this.bridge.sent++; on.sent?.(this.bridge.sent); },
      state: (running) => on.state?.(running),
      ticked: () => this.#report(),
    });
    this.bridge.numberBuffer = (file) => this.#core.bufferFor(file);
    engine.on?.("in", (msg) => { if (msg?.[0] === "/fail") { this.failures++; on.fail?.(msg); } });
  }

  get running() { return this.#core.running; }
  get perf() { return this.#core.perf; }

  /** Runs a program as a new job (LiveCore#run), in a group (0: none in particular); the job id. */
  async run(code, { group = 0 } = {}) {
    this.bridge.cutFade();   // a Run mid-fade takes the silence now
    // a random source other than :white is fetched before the run, never during it
    const { programTables, programSynthdefUrls } = await import("./runtime.js");
    await Promise.all(programTables(code).map((t) => this.#runtime.installTable(t)));
    // a program's own synths, their metadata installed before it runs (as the worker does, live-worker.js installSynths)
    const metas = await Promise.all(programSynthdefUrls(code).map((u) => this.#runtime.installSynthMeta(u)));
    if (metas.length) this.#on.synthMeta?.(metas.map(({ text, ...x }) => x));
    return this.#core.run(code, { group, preload: () => this.bridge.preload(code, this.#runtime.samples, this.#runtime.synthdefFor) });
  }

  status() { return statusFrom(this.processTable(), this.#records); }

  #report() {
    const s = this.status();
    const key = JSON.stringify(s);
    if (key === this.#lastStatus) return;
    this.#lastStatus = key;
    this.#on.status?.(s);
  }

  /** The timings gathered since the last call, and a fresh start. */
  takePerf() { return this.#core.takePerf(); }

  /** SuperSonic's own metrics (getMetrics), and the audio context they come from. */
  engineMetrics() { return this.#engine.getMetrics(); }
  get audioContext() { return this.#engine.audioContext; }

  /** Bytes the runtime's wasm heap holds. */
  runtimeHeapBytes() { return this.#core.heapBytes(); }

  /** One job stops where it stands; the others carry on. */
  stopJob(job) { this.#core.stopJob(job); }

  /** A group stops (LiveCore#stopGroup): its threads now, its sounds faded over `fade` seconds and freed after. */
  stopGroup(group, fade = 0) { this.#core.stopGroup(group, fade); }
  /** A cue from outside the program (MIDI in): LiveCore#cue. */
  cue(address, args = []) { this.#core.cue(address, args); }

  /** A group sits under another: the parent's stop takes it too. */
  groupUnder(group, parent) { this.#core.groupUnder(group, parent); }

  /** A subtree stops (LiveCore#stopSubtree): a thread and everything under it, or an fx block's threads and sounds. */
  stopSubtree(uid, fade = 0) { this.#core.stopSubtree(uid, fade); }

  /** Every job stops where it stands, and what is sounding fades out (Bridge#fadeOut). */
  stop() {
    this.#core.stop();
    this.bridge.fadeOut();
  }

  /** The process table (LiveCore#processTable): a view, read it before the next call. */
  processTable() { return this.#core.processTable(this.#engine.clock.now()); }

  /** A thread's id and name by its uid (the process table's), from the records: {id, name, on} or null. */
  threadName(uid) { return this.#records.thread(uid); }

  /** What an fx or a sound row's node is, from its record: {synth, buf}, or null. */
  nodeName(node) { return this.#records.node(node); }

  /** The engine's clock now, the one records' times are on. */
  clockNow() { return this.#engine.clock.now(); }

  /**
   * Sets Link's tempo, as the Link strip and tap tempo do. It changes a
   * schedule-ahead from now, where the next sounds are made; running :link
   * threads carry on from their beat. SuperSonic's clock, the session's
   * timeline, takes the same tempo. Native's range: 20 to 999.
   */
  setLinkBpm(bpm) {
    this.linkBpm = clampBpm(bpm);
    this.#core.setLinkBpm(this.linkBpm);
    this.#engine.clock.setBpm?.(this.linkBpm);
  }

  /** A program changed Link's tempo (its studio record): SuperSonic's clock follows. */
  followLinkBpm(bpm) {
    this.linkBpm = bpm;
    this.#engine.clock.setBpm?.(bpm);
  }

  /** The global time warp, in ms: every sound, OSC and MIDI message that much later (earlier when negative). */
  setTimeWarp(ms) { this.#core.setTimeWarp(ms); }
  /** The page lost this many seconds: the schedule moves on by them. */
  hold(seconds) { this.#core.hold(seconds); }

  /** Seconds since the session's first run, for a record's time. */
  at(r) { return r.time - (this.#core.t0 ?? r.time); }
}

/**
 * A live session whose runtime runs in a worker (live-worker.js), with the
 * same face as LiveSession. The worker ticks the runtime on its own timers
 * and sends every sound straight to the engine; this half loads what it asks
 * for, posts it the engine's clock, silences the engine on a Stop, and turns
 * its batches back into records, status and timings for the page.
 */
export class WorkerSession {
  #worker; #engine; #on; #records = createRecordReader(); #lastStatus = "";
  #table = new Float64Array(0); #heap = 0; #t0 = null;
  #calls = new Map(); #nextCall = 1; #clock = null;
  #stopping = Promise.resolve();   // a Run waits for the Stop before it to be silenced
  #runWaiting = false;             // a Run is waiting on that: the Stop's fade is cut, not played out
  /** Timings since the last takePerf(): the worker's ticks and sounds, the page's records. */
  perf = freshPerf();
  /** scsynth's /fail replies since the session began: messages the engine refused. */
  failures = 0;
  bridge;
  running = false;
  /** Link's tempo (the strip's, tap tempo's, or a program's set_link_bpm!). */
  linkBpm = 60;

  /** As LiveSession's; runtime is loadLiveRuntime's, with its worker. */
  constructor(runtime, engine, on = {}, bridge = null) {
    this.#worker = runtime.worker;
    this.#engine = engine;
    this.#on = on;
    this.bridge = bridge ?? new Bridge(engine);
    this.#worker.onmessage = ({ data }) => this.#message(data);
    this.#worker.onerror = (e) => console.error(`the runtime's worker: ${e.message ?? e}`);
    const channel = engine.createOscChannel();
    this.#call("live", { channel: channel.transferable, clock: this.#anchor() }, channel.transferList)
      .catch((e) => console.error(`the runtime's worker did not take the engine: ${e.message}`));
    // the engine's clock for the worker to count from: it cannot read the audio context
    this.#clock = setInterval(() => this.#worker.postMessage({ type: "clock", clock: this.#anchor() }), 1000);
    engine.on?.("in", (msg) => { if (msg?.[0] === "/fail") { this.failures++; on.fail?.(msg); } });
  }

  #anchor() { return { ntp: this.#engine.clock.now(), wall: performance.timeOrigin + performance.now() }; }

  #call(type, data = {}, transfer = []) {
    const id = this.#nextCall++;
    return new Promise((resolve, reject) => {
      this.#calls.set(id, { resolve, reject });
      this.#worker.postMessage({ type, id, ...data }, transfer);
    });
  }

  #message(d) {
    if (d.type === "reply") {
      const call = this.#calls.get(d.id);
      this.#calls.delete(d.id);
      if (d.error != null) call?.reject(new Error(d.error));
      else call?.resolve(d.value);
    } else if (d.type === "batch") this.#batch(d);
    else if (d.type === "load") this.#load(d.ops);
    else if (d.type === "synthMeta") this.#on.synthMeta?.(d.metas);   // a program's own synths, as the runtime now knows them
    else if (d.type === "error") console.error(`the runtime's worker: ${d.error}`);
  }

  // what the worker's runtime needs loaded, in the order it asked
  #load(ops) {
    const answer = (loaded) => this.#worker.postMessage({ type: "loaded", ...loaded });
    // A synth that will not load is silence, so it is said in the error pane and not only the console — but once for
    // the batch, and naming the right thing. A run loads its mixers and its default synth alongside whatever the
    // program plays, so when the whole directory is gone they all fail together, and naming any one of them (":beep
    // could not be loaded", under a card playing :pluck) tells the player something untrue. `builtIn` keeps the
    // preloader's guesses out of it: `synth :whoosh` has it ask for a built-in of that name before load_synthdef has
    // said where the synth really comes from, and that 404 is expected.
    const mine = ([op, a, b]) => op === "synthdef" && (b || this.#on.builtIn?.(a) !== false);
    const asked = [];
    for (const [op, a, b] of ops) {
      if (op === "synthdef") {
        const load = (b ? this.bridge.synthDefUrl(b) : this.bridge.synthDef(a)).then((ok) => { answer({ synthdef: a, ok }); return ok; });
        if (mine([op, a, b])) asked.push(load.then((ok) => (ok ? null : { name: a, url: b, problem: this.bridge.synthDefProblem(a) })));
      }
      else if (op === "sample") this.bridge.loadBuffer(a, b).then((ok) => answer({ bufnum: a, ok }));
      else if (op === "free") this.bridge.freeBuffer(a, b);
    }
    if (asked.length) Promise.all(asked).then((rs) => this.#sayLoadFailed(rs.filter((r) => r?.problem)));
  }

  /** One error for a batch of synths that would not load: the one that failed, or all of them at once. */
  #sayLoadFailed(bad) {
    if (!bad.length) return;
    const own = bad.filter((r) => !INTERNAL_SYNTHS.has(r.name));   // the mixers fail with everything; they are never the story
    const say = (message) => this.#on.error?.({ class: "LoadError", message });
    if (own.length === 1) return say(own[0].url ? `load_synthdef: ${own[0].problem}` : own[0].problem);
    // several at once: one place is unreachable, so name the place rather than an arbitrary synth
    const where = /\bat (\S+)\/[^/\s]+\.scsyndef/.exec(bad[0].problem)?.[1];
    return say(where ? `Sonic Pi's synths could not be loaded — nothing found at ${where}/ (404). Nothing will sound until they do.`
      : (own[0] ?? bad[0]).problem);
  }

  #batch({ records, table, heap, perf, started, host, running, t0 }) {
    this.#t0 ??= t0 ?? null;
    if (perf) addPerf(this.perf, perf);
    if (host) for (const [address, number, name] of host) this.#on.host?.(address, number, name);
    if (records) {
      const view = new DataView(records.buffer, records.byteOffset, records.byteLength);
      for (let p = 0; p + 4 <= records.length;) {
        const n = view.getUint32(p);
        deliver(this.#records.read(decode(records.subarray(p + 4, p + 4 + n))), this.perf, this.#on, () => this.clockNow(), (r) => this.at(r));
        p += 4 + n;
      }
    }
    if (started) { this.bridge.sent += started; this.#on.sent?.(this.bridge.sent); }
    if (heap) this.#heap = heap;
    if (table) this.#table = table;
    if (running != null && running !== this.running) { this.running = running; this.#on.state?.(running); }
    if (table || records) this.#report();
  }

  /** Runs a program as a new job in the worker; the job id. */
  async run(code, { group = 0 } = {}) {
    // a Run mid-fade takes the silence now; one before the fade has begun asks for the same
    this.#runWaiting = true;
    this.bridge.cutFade();
    try { await this.#stopping; } finally { this.#runWaiting = false; }
    const { job, t0 } = await this.#call("run", { code, group });
    this.#t0 ??= t0;
    if (!this.running) { this.running = true; this.#on.state?.(true); }
    return job;
  }

  status() { return statusFrom(this.#table, this.#records); }

  #report() {
    const s = this.status();
    const key = JSON.stringify(s);
    if (key === this.#lastStatus) return;
    this.#lastStatus = key;
    this.#on.status?.(s);
  }

  /** The timings gathered since the last call, and a fresh start. */
  takePerf() {
    const p = this.perf;
    this.perf = freshPerf();
    return p;
  }

  /** SuperSonic's own metrics (getMetrics), and the audio context they come from. */
  engineMetrics() { return this.#engine.getMetrics(); }
  get audioContext() { return this.#engine.audioContext; }

  /** Bytes the runtime's wasm heap holds, as the worker last said. */
  runtimeHeapBytes() { return this.#heap; }

  /** One job stops where it stands; the others carry on. */
  stopJob(job) { this.#worker.postMessage({ type: "stopJob", job }); }

  /** A group stops in the worker: its threads now, its sounds faded over `fade` seconds and freed after. */
  stopGroup(group, fade = 0) { this.#worker.postMessage({ type: "stopGroup", group, fade }); }
  cue(address, args = []) { this.#worker.postMessage({ type: "cue", address, args }); }

  /** A group sits under another: the parent's stop takes it too. */
  groupUnder(group, parent) { this.#worker.postMessage({ type: "groupUnder", group, parent }); }

  /** A subtree stops in the worker: a thread and everything under it, or an fx block's threads and sounds. */
  stopSubtree(uid, fade = 0) { this.#worker.postMessage({ type: "stopSubtree", uid, fade }); }


  /**
   * Every job stops where it stands, and what is sounding fades out
   * (Bridge#fadeOut): the engine's schedule is purged at once, and the fade
   * begins once the worker has stopped — it purges again first, for anything
   * the worker sent before it heard, and only then slides the mixer (a purge
   * wipes the IN ring, a message just sent with it); the free-all comes as
   * the fade ends. A Run after cuts the fade and waits for that free-all.
   */
  stop() {
    const first = this.bridge.purge();
    if (this.running) { this.running = false; this.#on.state?.(false); }
    this.#stopping = Promise.all([first, this.#call("stop").catch(() => {})]).then(() => {
      const fade = this.bridge.fadeOut();
      if (this.#runWaiting) fade.cut();
      return fade.done;
    });
  }

  /** The process table as the worker last posted it: PROCESS_FIELDS.length doubles per row. */
  processTable() { return this.#table; }

  /** A thread's id and name by its uid (the process table's), from the records: {id, name, on} or null. */
  threadName(uid) { return this.#records.thread(uid); }

  /** What an fx or a sound row's node is, from its record: {synth, buf}, or null. */
  nodeName(node) { return this.#records.node(node); }

  /** The engine's clock now, the one records' times are on. */
  clockNow() { return this.#engine.clock.now(); }

  /** As LiveSession#setLinkBpm: the worker's runtime and SuperSonic's clock take the tempo. */
  setLinkBpm(bpm) {
    this.linkBpm = clampBpm(bpm);
    this.#worker.postMessage({ type: "linkBpm", bpm: this.linkBpm });
    this.#engine.clock.setBpm?.(this.linkBpm);
  }

  /** A program changed Link's tempo (its studio record): SuperSonic's clock follows. */
  followLinkBpm(bpm) {
    this.linkBpm = bpm;
    this.#engine.clock.setBpm?.(bpm);
  }

  /** The global time warp, in ms: every sound, OSC and MIDI message that much later (earlier when negative). */
  setTimeWarp(ms) { this.#worker.postMessage({ type: "timeWarp", ms }); }
  /** The page lost this many seconds: the schedule moves on by them. */
  hold(seconds) { this.#worker.postMessage({ type: "hold", seconds }); }

  /** Seconds since the session's first run, for a record's time. */
  at(r) { return r.time - (this.#t0 ?? r.time); }
}
