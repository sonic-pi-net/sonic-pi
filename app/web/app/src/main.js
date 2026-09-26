// SPDX-License-Identifier: AGPL-3.0-or-later
// Sonic Pi for the web: the GUI. Native Sonic Pi v5's themes, highlighting,
// autocomplete, docs and quickstart cards, over the mruby runtime and
// SuperSonic (web/sonic_pi.js).
//
// The web build has no OSC in or out. MIDI comes in through Web MIDI and
// shows as cues. Time is one global timeline every thread shares.
import { SUPERSONIC_VERSION, supersonicVersion, PROCESS_FIELDS, engineWasm, workerSettled } from "./runtime.js";
// the engine's side (sonic_pi.js, and with it SuperSonic): loaded with the runtime (startRuntime), so a page only read
// never fetches it; `sp` is the module once it has arrived, for what runs only with an engine (oscSchedule)
let sp = null;
const needEngineModule = () => import("./sonic_pi.js").then((m) => (sp = m));
import { cardRings } from "./ui/card.js";
import { createCodeKeyboard } from "./keyboard.js";
import * as theme from "./theme.js";
import { LoopScopeState, drawLoopScope, SWEEP_WINDOW, SWEEP_SEARCH } from "./loopscope.js";
import { Scope } from "./scope.js";
import { CompletionAPI, synthAt } from "./completion/api.js";
import { createEditor, NUM_BUFFERS, STARTER, FORMER_STARTERS } from "./editor.js";
import { createWorkspace, NAME_MAX, DESCRIPTION_MAX } from "./workspace.js";
import { createBufferTabs } from "./buffer-tabs.js";
import { createSetsView } from "./sets-view.js";
import { animateWhileShown } from "./ui/shown.js";
import { isSet } from "./set-bundle.js";
import { createDocs, createInstrument } from "./docs.js";
import { createDeck } from "./ui/deck.js";
import { pageFromMeta, pageFromSynthdef } from "./synth-meta.js";
import { createQuickstart } from "./quickstart.js";
import { createInsight, threadLabel } from "./insight.js";
import { createFlightRecorder } from "./flight.js";
import { createLogs } from "./logs.js";
import { createShortcuts, DEF, MODES, parseChord } from "./shortcuts.js";
import { createShortcutEditor } from "./shortcuts-ui.js";
import { createPageBar } from "./ui/pagebar.js";
import { installTooltips } from "./tooltip.js";
import { encodeCode, encodeDigits, decodeCode, loadShareCodec } from "./share.js";
import { createShareMenu } from "./share-menu.js";
import { createInfo } from "./info.js";
import { announce, Announcement, setSpeakTransport } from "./announce.js";
import { origin, deepActive, shadowPane, eachShadowRoot } from "./shadow.js";
import { createLoadingStop } from "./loading-stop.js";
import { icon } from "./icons.js";
import { explainError, makeKnown } from "./friendly.js";

const $ = (id) => document.getElementById(id);
const store = {
  get: (k, d = null) => { try { const v = localStorage.getItem(k); return v === null ? d : JSON.parse(v); } catch { return d; } },
  set: (k, v) => { try { localStorage.setItem(k, JSON.stringify(v)); } catch {} },
};
// the window never scrolls (style.css clips it): a browser without clip, scrolled by a scrollIntoView, is put back
for (const t of [window, document.body]) t.addEventListener("scroll", () => { if (document.scrollingElement.scrollTop || document.body.scrollTop) { document.scrollingElement.scrollTop = 0; document.body.scrollTop = 0; } }, { passive: true });
// native's tooltips, in place of the browser's, for every control with a title (tooltip.js)
const tips = installTooltips(document.body);
eachShadowRoot((root) => tips.watch(root));   // and in the editor's and the panes' shadow roots (shadow.js)
// native's keyboard shortcuts, in the keymap the player picked (shortcuts.js; the keys, at the end)
const keys = createShortcuts({ store });
const json = async (path) => {
  const r = await fetch(path);
  if (!r.ok) throw new Error(`${path}: ${r.status}`);
  return r.json();
};
const el = (tag, cls, text) => {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text != null) e.textContent = text;
  return e;
};

// ── Logs (native's Logs tab): each source says what it did from the moment the page loads ──

const logs = createLogs(shadowPane($("logs-pane")), ["GUI", "Runtime", "Host", "SuperSonic"]);   // in its shadow root (shadow.js)
const describe = (v) => {   // a value as a line of log: a string as is, an error with its stack, anything else as JSON
  if (typeof v === "string") return v;
  if (v instanceof Error) return v.stack || `${v.name}: ${v.message}`;
  try { return JSON.stringify(v); } catch { return String(v); }
};
const bytes = (n) => (n >= 1048576 ? `${(n / 1048576).toFixed(1)} MB` : `${Math.round(n / 1024)} KB`);
// the page's console, as native's GUI log: everything it says still goes to the browser's too
for (const [method, prefix] of [["log", ""], ["info", ""], ["debug", ""], ["warn", "warning: "], ["error", "error: "]]) {
  const original = console[method].bind(console);
  console[method] = (...args) => { original(...args); logs.add("GUI", prefix + args.map(describe).join(" ")); };
}
addEventListener("error", (e) => logs.add("GUI", `error: ${e.error ? describe(e.error) : `${e.message} (${e.filename}:${e.lineno})`}`));
addEventListener("unhandledrejection", (e) => logs.add("GUI", `unhandled rejection: ${describe(e.reason)}`));
logs.add("GUI", `page loaded: ${navigator.userAgent}`);
logs.add("Host", `cross-origin isolated: ${self.crossOriginIsolated === true}, ${navigator.hardwareConcurrency ?? "?"} cores`);
document.addEventListener("visibilitychange", () => logs.add("Host", `page ${document.hidden ? "hidden" : "visible"}`));

// ── Data and themes ───────────────────────────────────────────────────────

// The themes first: the page's colours are the one thing it cannot draw without. The rest is fetched the first time
// something needs it, and a page that never does never fetches it: the reference (the docs pane, the quickstart,
// load_example, a page's live synth: needReference) and, with it, the completion (the editor, once it shows:
// needEditorData). Until then the completion offers nothing and a synth's defaults are unknown.
await theme.loadThemes("./");
let lang = { pages: [] }, synths = { pages: [] }, fx = { pages: [] }, samples = { groups: [] }, examples = { groups: [] }, quickstartData = null, support = null;
const ref = {};
const api = new CompletionAPI();
// each synth's documented defaults, for how long a note it plays lasts
const synthOpts = new Map();
// what the docs say exists, for an error's "did you mean" (friendly.js): nothing, until the editor's data is in
let known = makeKnown({});
const once = (what, fn) => { let p = null; return () => (p ??= fn().catch((e) => { p = null; logs.add("Host", `${what} did not load: ${describe(e)}`); throw e; })); };
const needReference = once("the reference", () => Promise.all([
  json("data/reference/lang.json"),
  json("data/reference/synths.json"),
  json("data/reference/fx.json"),
  json("data/reference/samples.json"),
  json("data/reference/examples.json"),
  json("data/reference/quickstart.json"),
  json("data/runtime-support.json").catch(() => null),
]).then((all) => {
  [lang, synths, fx, samples, examples, quickstartData, support] = all;
  Object.assign(ref, { lang, synths, fx, samples, examples, support });
  for (const p of synths.pages) synthOpts.set(p.key, Object.fromEntries((p.opts || []).map((o) => [o.name, o.default])));
}));
const needEditorData = once("the editor's completion", () => Promise.all([json("data/completion.json"), needReference()]).then(([completion]) => {
  api.load(completion, { lang: lang.pages, synths: synths.pages, fx: fx.pages, samples: samples.groups });
  known = makeKnown(completion);
}));
// the docs pane, made once the reference it is made of has arrived; fn is given it
const withDocs = (fn = null) => needReference().then(() => { docs ??= createDocs($("docs-pane"), ref, paneHooks); fn?.(docs); });

// ── Status ────────────────────────────────────────────────────────────────

// The status bar's left end: the engine's state, and over it for a moment a message (toast), as native's
// statusBar()->showMessage(text, 2000) sits over its permanent widgets and then gives them back
let engineShown = { text: "loading", err: false }, messageTimer = null;
const paintEngine = () => { $("status-engine").textContent = engineShown.text; $("status-engine").classList.toggle("err", engineShown.err); };
const setEngineStatus = (text, err = false) => {
  engineShown = { text, err };
  if (err || !messageTimer) { clearTimeout(messageTimer); messageTimer = null; paintEngine(); }   // a problem shows at once, over a message
  logs.add("Host", err ? `error: ${text}` : text);
  if (err) announce(text, true, Announcement.Error);
};
// native's showStatusAndAnnounce: a screen reader hears what the status bar would say, once Sonic Pi is up
let bootAnnouncementsReady = false;
const status = (text, assertive = false) => { if (bootAnnouncementsReady) announce(text, assertive); };
const versions = { runtime: null };
// what this build is made of: the Info overlay says (native's λ), the status bar does not
// the foot, as native's status bar says it ("Sonic Pi v5.0 on Mac"): the version and where it runs, the rest in About
const paintVersions = () => { $("info-versions").textContent = "Sonic Pi v5.0 on Web"; };
paintVersions();
supersonicVersion().then(paintVersions);   // the local build's version.json

// ── Sound: the runtime loads now, the engine boots on the first gesture ───

let session = null;
let booting = null;
let engineRef = null;    // the engine itself: the inline scopes read the loop taps out of its shared memory
const jobBuffer = new Map(); // job → the buffer it ran from
const programs = new Map();  // job → what it ran, for flight reports
// The runtime's worker and its wasm (1.7 MB, compiled on arrival): started when the editor shows or something is about
// to play (a card pointed at, touched or focused on a page of the site), so a page that is only read never loads it
let runtimeReady = null;
// The runtime's worker, started at once: its module and its wasm on their way while sonic_pi.js (and SuperSonic's
// client with it) is still arriving, not after. `alive` resolves true once the worker's module runs (live-worker.js
// says so first thing), false if it cannot: the engine hands its egress only to a worker that is there to take it.
let liveWorker;
function startWorker() {
  if (liveWorker !== undefined) return liveWorker;
  liveWorker = null;
  if (typeof Worker !== "function") return null;
  try {
    const worker = new Worker(new URL("live-worker.js", location.href), { type: "module", name: "sonic-pi runtime" });
    const alive = new Promise((ok) => {
      worker.addEventListener("message", ({ data }) => { if (data?.type === "alive") ok(true); });
      worker.addEventListener("error", () => ok(false));
    });
    liveWorker = { worker, alive, settled: workerSettled(worker) };   // its ready, heard from now (loadLiveRuntime)
  } catch { /* no module workers: the runtime runs on the page (loadLiveRuntime) */ }
  return liveWorker;
}
const startRuntime = () => runtimeReady ??= (() => {
  engineWasm();   // the engine's wasm too: whatever starts the runtime is a sign that sound is wanted
  const w = startWorker();
  return needEngineModule().then((m) => m.loadLiveRuntime("./", { worker: w?.worker ?? null, settled: w?.settled ?? null }));
})().then((r) => {
  versions.runtime = r.version;
  logs.add("Runtime", `mruby runtime ${r.version} loaded, ${r.worker ? "in a worker of its own" : "on the page"}`);
  paintVersions();
  setEngineStatus("ready");
  announce("Sonic Pi is ready");
  bootAnnouncementsReady = true;
  return r;
}).catch((e) => {
  setEngineStatus("Error - the runtime could not load", true);
  logs.add("Runtime", `error: ${describe(e)}`);
  showError({ class: "RuntimeLoadError", message: String(e.message ?? e) });
  throw e;
});
// the editor showing: its runtime, and its completion, on their way
const wakeEditor = () => { startRuntime().catch(() => {}); needEditorData().catch(() => {}); };

const scopeBox = $("scope-container");
const scope = new Scope($("scope-canvas"), { mode: window.matchMedia("(max-width: 760px)").matches ? "wave" : "bars", ...store.get("sp-scope", {}) });   // a phone's band is a wave: bars at that height read as dashes

// what SuperSonic says, from before it boots
function listenEngine(engine) {
  const on = (event, fn) => engine.on?.(event, fn);
  on("debug", (m) => logs.add("SuperSonic", String(m?.text ?? "").replace(/\n$/, "")));
  on("error", (e) => logs.add("SuperSonic", `error: ${describe(e)}`));
  on("shutdown", () => logs.add("SuperSonic", "shutting down"));
  on("reload:start", () => logs.add("SuperSonic", "reloading"));
  on("reload:complete", (d) => logs.add("SuperSonic", `reloaded${d?.success === false ? ": failed" : ""}`));
  on("ready", (d) => logs.add("Host", `SuperSonic ready: ${describe(d?.bootStats ?? {})}`));
  on("audiocontext:statechange", (d) => logs.add("Host", `audio context ${d?.state}`));
  on("audiocontext:interrupted", () => logs.add("Host", "audio context interrupted"));
  // The browser can suspend the audio context (a backgrounded tab, an iOS
  // interruption) or take the worklet away altogether. Recovery is SuperSonic's
  // recover(): a resume if the worklet still answers, else a reload — the
  // worklet and engine made again, its synthdefs and buffers put back. It
  // needs a gesture, so the status says so and the next tap or key does it.
  on("audiocontext:suspended", () => audioLost("suspended"));
  on("audiocontext:interrupted", () => audioLost("interrupted"));
  on("audiocontext:statechange", (d) => { if (d?.state === "running") audioBack(); });
  on("audiocontext:resumed", audioBack);
  on("resumed", audioBack);
  on("reload:start", audioReloading);
  on("reload:complete", (d) => { if (d?.success !== false) audioReloaded(); else audioBroken(); });
  on("loading:start", (d) => { logs.add("Host", `loading ${d?.type} ${d?.name}`); if (d?.type !== "wasm") loadingStop?.begin(`${d?.type}:${d?.name}`, loadWeight(d)); });
  on("loading:complete", (d) => { logs.add("Host", `loaded ${d?.type} ${d?.name}${d?.size ? ` (${bytes(d.size)})` : ""}`); loadingStop?.end(`${d?.type}:${d?.name}`); });
  on("buffer:pool:grown", (d) => logs.add("Host", `buffer pool grew to ${bytes(d?.totalCapacity ?? 0)}`));
}

// ── Recovery: the audio context suspended, or the worklet gone ───────────
// Only a tap may start a browser's audio again (iOS: a call, another app, the tab away). So the page asks for one, on
// a card over everything, and that tap is the recovery: SuperSonic's recover(), inside it — a resume if the worklet
// still answers, else a reload. One way back, asked for plainly: a tap or key meant for the code is never taken as a
// resume. What the engine itself does on the page's lifecycle is Clockwork's (pageLifecycle, bootEngine).
const RESUME_LABEL = globalThis.matchMedia?.("(pointer: coarse)").matches ? "Tap to resume" : "Resume audio";
$("resume-go").textContent = RESUME_LABEL;
const resumeCard = {
  why: "",
  broken: false,   // a recovery that failed outright: the engine is gone, and starting the page again is the way back
  show(why, { broken = false } = {}) {
    this.why = why;
    this.broken = broken;
    $("resume-why").textContent = why;
    $("resume-title").textContent = broken ? "Audio stopped" : "Audio paused";
    $("resume-go").textContent = broken ? "Restart Sonic Pi" : RESUME_LABEL;
    $("resume-go").disabled = false;
    if ($("resume-overlay").hidden) {
      $("resume-overlay").hidden = false;
      $("resume-go").focus({ preventScroll: true });
      setEngineStatus("audio paused: tap to resume", true);
      logs.add("Host", `audio paused (${why}): asking for a tap`);
    }
  },
  hide() {
    if ($("resume-overlay").hidden) return;
    $("resume-overlay").hidden = true;
    setEngineStatus("ready");
  },
  get shown() { return !$("resume-overlay").hidden; },
};
$("resume-go").addEventListener("click", async () => {
  if (resumeCard.broken) { location.reload(); return; }   // the buffers are kept (the editor's store): the page comes back as it was
  if (!engineRef) return resumeCard.hide();
  const go = $("resume-go");
  go.disabled = true;
  go.textContent = "Resuming…";
  const ok = await engineRef.recover().catch((e) => { logs.add("Host", `recovery failed: ${describe(e)}`); return false; });
  logs.add("Host", `resume on a tap: ${ok ? "sound back" : "failed"}`);
  if (ok && engineRef.audioContext?.state === "running") audioBack();
  else if (engineRef.getEngineState?.() === "error" || engineRef.audioContext?.state === "closed") audioBroken();
  else resumeCard.show("The audio did not come back yet. Try again.");
});
function audioLost(how) {
  if (!engineRef) return;
  resumeCard.show(how === "interrupted" ? "Another app or a call took the audio."
    : how === "stopped" ? "The browser stopped Sonic Pi's audio."
    : "The browser paused Sonic Pi's audio.");
}
// A reload that failed (Clockwork took down what it built): nothing left to resume. Starting the page again is the
// way back, and says so rather than offering a tap that cannot work.
function audioBroken() {
  resumeCard.show("Audio systems restarted.", { broken: true });
}
function audioBack() {
  resumeCard.hide();
  if (replayAfterReload.length) replayLost();   // a reload that came back suspended, now resumed
}
// The engine is reloading: everything that was playing went with the old
// worklet, and the runtime must not keep scheduling into the one being built
// (each sound would be refused, its groups gone), so a Stop now — which also
// has the runtime make its studio again on the next Run.
let audioRestarted = false;
let replayAfterReload = [];   // the buffers' runs that were sounding when the engine went: played again once it is back
function audioReloading() {
  replayAfterReload = [];
  for (let n = 0; n < NUM_BUFFERS; n++) {
    if (!liveGroups.includes(BUFFER_GROUP + n)) continue;
    const last = [...programs.values()].reverse().find((p) => p.buffer === n);
    if (last) replayAfterReload.push(last);
  }
  stop();
  setEngineStatus("audio restarting…", true);
}
// The engine reloaded. Its own state came back through restoreClientState
// (synthdefs, buffers, the piano table); the shared memory and the worker's
// channel into it survive a reload. What is the page's: the scope tapped the
// old worklet node, so it taps the new one.
function audioReloaded() {
  scope.attach(engineRef);
  attachNavScope(engineRef);   // the reload made a new audio context: the bar's scope and the stop's rings tap it
  audioIn.reconnect();          // and live_audio's input goes into the new engine
  // a context made without a gesture (iOS) starts suspended and says nothing: the card asks for the tap
  if (engineRef.audioContext?.state !== "running") { audioLost("suspended"); return; }
  resumeCard.hide();
  replayLost();
}
// What was playing from the buffers, played again: the music carries on rather than the page asking for Run
function replayLost() {
  const again = replayAfterReload;
  replayAfterReload = [];
  if (!again.length) { audioRestarted = true; setEngineStatus("ready"); toast("the browser restarted the audio: press Run", true); return; }   // the status just says ready: the toast says what happened
  for (const p of again) play(p.code, { buffer: p.buffer });
  setEngineStatus("running");
  toast("the browser restarted the audio: playing again");
  logs.add("Host", `audio restarted: ${again.length} buffer${again.length > 1 ? "s" : ""} playing again`);
}
// Back to the tab: if the context says running but the audio thread has stopped counting (the worklet gone, with no
// state change to tell of it), the card asks for the tap that recovers it. Nothing is started here: without a tap it
// would be refused, or build a context born suspended.
document.addEventListener("visibilitychange", async () => {
  if (document.hidden || !engineRef || resumeCard.shown) return;
  if (engineRef.audioContext && engineRef.audioContext.state !== "running") { audioLost(engineRef.audioContext.state); return; }
  if (!(await audioThreadAlive())) { logs.add("Host", "back to the tab: the audio thread has stopped"); audioLost("stopped"); }
});
// Back from the browser's back/forward cache: the engine was shut down when the page went (Clockwork's pagehide, so
// nothing plays on with no page), and this page's session is built on it. The page starts again: the buffers are
// kept, and the next tap boots the sound as a first visit's does.
window.addEventListener("pageshow", (e) => {
  if (e.persisted && engineRef?.getEngineState?.() === "stopped") location.reload();
});
/** Whether the engine's audio thread is running: its process count (Clockwork's metrics, slot 0) moving on. */
async function audioThreadAlive() {
  const count = () => engineRef?.getMetricsArray?.()?.[0] ?? null;
  const a = count();
  if (a == null || engineRef.audioContext?.state !== "running") return engineRef?.audioContext?.state === "running";
  await new Promise((r) => setTimeout(r, 250));
  const b = count();
  return b == null || b !== a;
}

// live_audio's input: the browser's microphone or line in, asked for the first time a program wants it
// (Scheduler#record_audio_in: how many channels, from input 1), with the browser's voice processing off, as a sound
// card's input is. Mono sources are spread across both of the engine's two inputs (live_audio's stereo: reads 1 and 2).
const audioIn = (() => {
  let stream = null, source = null, spread = null, asking = null, refused = false;
  const connect = () => {
    const engine = engineRef, ac = engine?.audioContext ?? engine?.node?.context;
    if (!stream || !ac || !engine.node) return;
    if (source?.context !== ac) {
      source?.disconnect(); spread?.disconnect();
      source = ac.createMediaStreamSource(stream);
      spread = new GainNode(ac, { channelCount: 2, channelCountMode: "explicit", channelInterpretation: "speakers" });
      source.connect(spread);
    }
    spread.disconnect();
    spread.connect(engine.node.input ?? engine.node);   // the engine's node is a wrapper: its input is the worklet itself
  };
  return {
    want(channels) {
      if (stream) return connect();
      if (asking || refused) return;
      asking = navigator.mediaDevices?.getUserMedia?.({ audio: { echoCancellation: false, noiseSuppression: false, autoGainControl: false, channelCount: { ideal: Math.max(2, channels) } } })
        ?.then((s) => {
          stream = s;
          const t = s.getAudioTracks()[0], set = t?.getSettings?.() ?? {};
          const ac = engineRef?.audioContext ?? engineRef?.node?.context;
          const ms = (x) => (x == null ? "?" : `${Math.round(x * 1000)} ms`);
          // where the time goes: the browser's own input and output buffering (the engine adds none: it reads the
          // input in the same 128-frame block it plays), and whether the device's rate differs from the engine's
          logs.add("Host", `live audio: ${t?.label || "input"} (${set.channelCount ?? "?"} ch, ${set.sampleRate ?? "?"} Hz, input latency ${ms(set.latency)}); ` +
            `engine ${ac?.sampleRate ?? "?"} Hz, base latency ${ms(ac?.baseLatency)}, output latency ${ms(ac?.outputLatency)}`);
          connect();
        })
        .catch((e) => {
          refused = true;   // not asked again this session: the browser remembers a refusal, and a prompt a loop repeats is noise
          showError({ class: "LiveAudioError", message: `live_audio: ${e?.name === "NotAllowedError" ? "the browser was not allowed to use the microphone. Allow it in the site's settings (the address bar), then reload" : e?.name === "NotFoundError" ? "no microphone or audio input was found" : `the audio input could not be opened: ${describe(e)}`}` });
        })
        .finally(() => { asking = null; });
      if (!asking) showError({ class: "LiveAudioError", message: "live_audio: this browser gives no access to audio input here (it needs a secure https:// page)" });
    },
    reconnect: connect,
  };
})();

// The audio context, made and started inside the tap that first asks for sound, before anything is awaited: a browser
// (iOS above all) lets audio start only within a gesture, and the engine's code (sonic_pi.js, SuperSonic, the runtime)
// arrives long after the tap has ended. The engine is handed this one (bootEngine's audioContext) rather than making
// its own too late. A tap on a card's Play before the page's script had run made it already (build-site.mjs:
// window.spAudioContext). Its options are the ones SuperSonic's own would have (48 kHz, the latency preference).
let audioContext = null;
function audioInGesture() {
  if (audioContext?.state === "closed") audioContext = null;
  try { audioContext ??= window.spAudioContext ?? new AudioContext({ latencyHint: lowLatency.on ? 0 : "interactive", sampleRate: 48000 }); }
  catch (e) { logs.add("Host", `no audio context: ${describe(e)}`); return null; }
  if (audioContext.state !== "running") audioContext.resume().catch(() => {});
  return audioContext;
}
// A context made in a press that is not yet a gesture to iOS (a touch's pointerdown: a live synth's key plays on it)
// starts on the lift that follows. Only while the engine boots: after that, sound that stops asks for its tap on the
// resume card, and a tap meant for the code is never taken as one.
for (const type of ["pointerup", "touchend", "click", "keydown"]) addEventListener(type, () => {
  if (booting && !engineRef && audioContext && audioContext.state !== "running") audioContext.resume().catch(() => {});
}, { capture: true, passive: true });

// The bar's stop while a run waits on what it needs (loading-stop.js): the engine and the runtime (a weight of 3 each,
// their megabytes), each synth (small) and each sample (by its size, where SuperSonic knows it). Made with the stop's
// rings, below; a Stop pressed while it loads calls off the run it was loading for (play: stops)
let loadingStop = null, stops = 0;
const loadWeight = (d) => (d?.type === "sample" ? (d.size ? Math.min(3, Math.max(0.4, d.size / 400000)) : 1) : 0.3);

// A tap that heads for the code (Launch Sonic Pi, the Code tab) starts the engine while it is a tap: a browser only
// lets audio start inside a gesture, and by the first Run it has booted, so that Run sounds at once
function warmEngine() { if (!session) ensureSession().catch(() => {}); }
async function ensureSession() {
  if (session) return session;
  audioInGesture();   // now, in the press, before anything is awaited
  booting ??= (async () => {
    scopeBox.classList.add("booting");
    loadingStop?.begin("engine", 3);
    loadingStop?.begin("runtime", 3);
    setEngineStatus("booting SuperSonic...");
    status("Starting the audio engine…");   // native's splash says "Sonic Pi is starting": the first Run's wait is not silence
    // The runtime and the engine start side by side: over a slow link each is seconds of downloads and round trips,
    // and neither needs the other until the engine hands its egress to the runtime's worker (bootEngine), which holds
    // it until its runtime is up (live-worker.js). The engine's wasm is fetched now, beside everything else.
    const wasmBytes = engineWasm();
    const w = startWorker();
    const runtimeP = startRuntime();
    runtimeP.then(() => loadingStop?.end("runtime"), () => {});   // awaited below, once the engine is up
    const m = await needEngineModule();
    const worker = w && (await w.alive) ? w.worker : null;
    const bytes = await wasmBytes;
    // SuperSonic reserves its shared memory's whole ceiling up front (maxInboxSize, 768 MB by default, is
    // most of it). A phone's browser gives a page far less, and iOS says "Out of memory" at the reservation
    // — a reload, with the last page's memory not yet let go, the more so. The inbox carries loads in
    // flight, so a phone reserves a slice of that and still loads any sample it has room to play.
    const phone = window.matchMedia("(max-width: 760px)").matches || (navigator.maxTouchPoints > 1 && /iPhone|iPad|Android/.test(navigator.userAgent));
    const engine = await m.bootEngine({
      ...(bytes ? { wasmBytes: bytes } : {}),   // fetched above, beside the runtime (no bytes: SuperSonic fetches its own)
      ...(audioInGesture() ? { audioContext } : {}),   // the one the press started (above)
      ...(phone ? { memory: { maxInboxSize: 96 * 1024 * 1024 } } : {}),
      // a phone's hidden tab is one the system may freeze or kill at any moment: its sound suspends while hidden, and
      // coming back asks for the tap that resumes it (the resume card), rather than playing on with no page to stop it
      ...(phone ? { pageLifecycle: { hidden: "suspend" } } : {}),
      // the smallest output buffer the browser allows, in place of its "interactive" one: less delay, more risk of
      // glitches on a loaded machine
      ...(lowLatency.on ? { audioContextOptions: { latencyHint: 0 } } : {}),
    }, { beforeInit: listenEngine, runtime: worker ? { worker } : null });
    loadingStop?.end("engine");
    const runtime = await runtimeP;
    // the worker's runtime could not load and the page's runs instead: the engine's egress went to a worker with no
    // runtime behind it, so what the engine says back (MIDI in, a controller) does not reach it. Rare; said, not hidden
    if (worker && runtime.worker !== worker) logs.add("Host", "the runtime runs on the page, not in its worker: MIDI and controller input will not arrive");
    const ac = engine.audioContext ?? engine.node?.context;
    const ms = (x) => (x == null ? "?" : `${(x * 1000).toFixed(1)} ms`);
    // outputLatency reads 0 until the audio has run a moment
    setTimeout(() => logs.add("Host", `audio: ${ac?.sampleRate ?? "?"} Hz, ${lowLatency.on ? "low latency" : "interactive"}; base latency ${ms(ac?.baseLatency)}, output latency ${ms(ac?.outputLatency)}`), 1000);
    groupsDeclared.clear();   // the runtime is new: its groups are told again as they are used
    session = sp.createLiveSession(runtime, engine, {
      output, log, error: showError, status: showJobs, state: () => {}, record: onRecord, midi: midiSend,
      host: (address, number, name) => {
        if (address === "/sonic-pi/audio-in") return audioIn.want(number);   // live_audio: the sound card's input, opened on first use
        logs.add("Runtime", { "/sonic-pi/synthdef": `load synthdef ${name}`, "/sonic-pi/sample": `load sample ${name} into buffer ${number}`, "/sonic-pi/sample_free": `free buffer ${number} (${name})` }[address] ?? `${address} ${number} ${name}`);
      },
      fail: (msg) => logs.add("SuperSonic", `/fail ${msg.slice(1).map(describe).join(" ")}`),     // a message the engine refused, as it said it
      synthMeta: registerSynths,   // a program's own synths (load_synthdef), as the runtime now knows them
      // Which names are Sonic Pi's own. A program's `synth :whoosh` has the preloader ask for `sonic-pi-whoosh`
      // before load_synthdef has said where that synth comes from, and that 404 is expected — only a synth this
      // build ships is worth telling the player about when it will not load.
      builtIn: (name) => (name.startsWith("sonic-pi-fx_")
        ? fx.pages.some((p) => p.key === name.slice("sonic-pi-fx_".length))
        // a synth of the player's own (load_synthdef) is not one of ours, however the docs list it
        : synths.pages.some((p) => p.key === name.slice("sonic-pi-".length) && !p.user)),
    });
    // what the Link strip was set to before the engine booted
    if (linkState.bpm !== 60) session.setLinkBpm(linkState.bpm);
    if (linkState.warp) session.setTimeWarp(linkState.warp);
    engineRef = engine;
    if (debugOn) showDebug(true);   // the Debug pane, open before the engine was: its metrics now
    if (store.get("sp-midi", false)) enableMidi();   // the MIDI the player had on last time, now there is a front to hang it on
    startGamepads();
    scope.attach(engine);
    attachNavScope(engine);
    setEngineStatus("running");
    return session;
  })();
  try {
    return await booting;
  } catch (e) {
    booting = null;
    loadingStop?.cancel();
    setEngineStatus("Error - SuperSonic failed to boot", true);
    showError({ class: "BootError", message: String(e.message ?? e) });
    return null;
  } finally {
    scopeBox.classList.remove("booting");
  }
}

/** Runs code as its own job; the job id, or null. */
let startingBuffer = null; // the buffer of a run whose head is running now
let lastRun = null;        // { buffer, code, prefix } of the last run started
// Groups (Scheduler#stop_group): a run belongs to one, and a group stops as one, with every group under it. A
// buffer's runs are its own group under the buffers'; a card's runs are the card's (ui/deck.js asks for a fresh
// one) under the cards'.
const BUFFERS_GROUP = 999, BUFFER_GROUP = 1000;   // buffer n's group is 1000 + n, under 999
const CARDS_GROUP = 2000;                          // the cards count up from 2001, under 2000
let cardGroups = CARDS_GROUP;
const groupsDeclared = new Set();   // told to this session (a new session starts over)
const declareGroup = (s, group, parent) => { if (!groupsDeclared.has(group)) { groupsDeclared.add(group); s.groupUnder(group, parent); } };
const nextCardGroup = () => ++cardGroups;
// native's "Enable external synths and FX" (Studio preferences): every run starts use_external_synths true, on the
// program's first line, so its line numbers stay its own
const externalSynths = { on: store.get("sp-external-synths", false) };
// native's Safe mode (Synths and FX, on by default): use_arg_checks before every run, so an opt's value out of its range
// is an error; on the web it also makes a sample there is none of an error
const safeMode = { on: store.get("sp-safe-mode", true) };
// the web's own, on by default and apart from Safe mode (native says nothing of it): an opt no synth, FX or sample
// takes is warned of, as the misspelling it most likely is
const unknownOpts = { warn: store.get("sp-warn-unknown-opts", true) };
// On by default: an instrument should be as quick as the browser will allow. Turning it off asks for a larger
// buffer, which is the cure for a machine that crackles under a heavy patch — and nothing at all in Safari, which
// gives its smallest buffer either way.
const lowLatency = { on: store.get("sp-low-latency", true) };   // read at the engine's boot
async function play(code, { buffer = null, scopeSlot = null, group = buffer != null ? BUFFER_GROUP + buffer : 0 } = {}) {
  const asked = stops;
  loadingStop?.expect();   // this run waits for its first sound: what loads under it holds the comet till then
  const s = await ensureSession();
  if (!s || stops !== asked) return null;   // Stop, pressed while it was starting, calls the run off
  try {
    startingBuffer = buffer;
    if (buffer != null) declareGroup(s, group, BUFFERS_GROUP); else if (group > CARDS_GROUP) declareGroup(s, group, CARDS_GROUP);
    // native's cards: the program inside a scope_out of its own, so the card's rings draw its own sound. A program
    // that opens with use_real_time (a live synth's key) keeps it ahead of the wrap: the scope_out made on the
    // sched-ahead clock (0.5s) would hold the note's sound back that long. The lines only swap places, so a line of the
    // program still stands one below its own number (ui/deck.js WRAP_LINES).
    const head = scopeSlot != null ? (/^(?:(?:use_real_time|use_debug false)\n)+/.exec(code)?.[0] ?? "") : "";
    const run = scopeSlot != null ? `${head}with_fx :scope_out, scope_num: ${scopeSlot} do\n${code.slice(head.length)}\nend\n` : code;
    // on line 1, so every line keeps its number (native puts its use_arg_checks on a line of its own); the error card
    // takes the prefix's length off a column on line 1
    const prefix = `${safeMode.on ? "use_arg_checks true; " : ""}${unknownOpts.warn ? "__warn_unknown_opts true; " : ""}${externalSynths.on ? "use_external_synths true; " : ""}`;
    lastRun = { buffer, code, prefix: prefix.length };   // a syntax error names no job: the error card reads the code from here
    const job = await s.run(prefix + run, { group });
    if (buffer != null) { jobBuffer.set(job, buffer); paintLoopScopes(); }
    programs.set(job, { job, buffer, code, prefix: prefix.length, started: new Date().toISOString() });
    if (programs.size > 20) programs.delete(programs.keys().next().value);
    refreshJobs();
    return job;
  } catch (e) {
    showError({ class: "RunError", message: String(e.message ?? e) });
    return null;
  } finally {
    startingBuffer = null;
  }
}

// The session reports status only when it changes; a job that ends inside
// its own run (a single play) changes nothing it has reported. Ask directly.
function refreshJobs() {
  if (session) showJobs(session.status());
}
setInterval(refreshJobs, 500);

const realTime = "use_real_time\nuse_debug false\n";
const caretSynth = () => synthAt(editor.getCode(), editor.view.state.selection.main.head);

// ── Live loop scopes ──────────────────────────────────────────────────────
// As native's: every live_loop plays through a scope_out of its own, whose
// stream the page reads out of the engine's shared memory (SuperSonic's
// getScope, in SAB mode) and the editor draws after the loop's header line.
// The fx record says which loop got which slot; its fx_free, or the job
// ending, takes the scope down again.
const loopScopes = new Map();        // fx node → { name, slot, job }
const loopScopePrefs = { show: store.get("sp-loop-scopes", true), scroll: store.get("sp-loop-scope-scroll", false) };
const scopeFrame = (slot, frames) => engineRef?.getScope?.(slot, frames) ?? null;
function paintLoopScopes() {
  editor.setLoopScopes(loopScopePrefs.show
    ? [...loopScopes.values()].filter((s) => (s.buffer ?? jobBuffer.get(s.job)) === editor.active).map(({ name, slot }) => ({ name, slot }))
    : []);
}

// Threads held on a sync, marked at the line they wait on (editor.js waitField):
// the status says which threads wait where; the active buffer's lines get a mark
let lastThreads = [], lastWaits = "";
function paintWaits(threads = lastThreads) {
  lastThreads = threads;
  const byLine = new Map();
  for (const t of threads) {
    if (t.state !== "waiting" || !t.line || (jobBuffer.get(t.job) ?? -1) !== editor.active) continue;
    const w = byLine.get(t.line) ?? { line: t.line, key: String(t.on ?? ""), who: [] };
    w.who.push(threadLabel(t.id, t.name));
    byLine.set(t.line, w);
  }
  const list = [...byLine.values()].map((w) => ({ ...w, who: w.who.join(", ") }));
  const key = JSON.stringify(list);
  if (key === lastWaits) return;
  lastWaits = key;
  editor.setWaits(list);
}

const hooks = {
  run: () => run(),
  webSupport: (name) => support?.why?.[name] ?? null,   // what the web build says of a function it does not have (the completion pane)
  cueValue: (path) => api.cueValue(path),   // what a cue last carried, for the completion pane (a cue has no docs)
  scopeFrame,
  loopScopeScroll: () => loopScopePrefs.scroll,
  stop: () => stop(),
  showDocs: (word, hint) => {
    openDrawer("docs");
    withDocs((d) => { if (!d.showFor(word, hint)) toast(`no docs for ${word}`); });
  },
  onBuffer: () => { paintLoopScopes(); paintWaits(); if (shownError) paintFix(); },   // the tabs draw themselves (buffer-tabs.js)
  onCaret: (line, position) => { $("caret-pos").textContent = `Line: ${line},  Position: ${position}`; },
  playNote: (n, { synth, fx } = {}) => {
    if (fx) return play(`${realTime}with_fx :${fx} do\n  synth :prophet, note: ${n}, release: 0.6, cutoff: 90\nend`);   // an FX's keys: a note through it
    const s = synth ?? caretSynth();
    return play(`${realTime}${s ? `use_synth :${s}\n` : ""}play ${n}, release: 0.4`);
  },
  playChord: (notes) => play(`${realTime}play [${notes.join(", ")}], release: 1`),
  playScale: (notes) => play(`${realTime}[${notes.join(", ")}].each do |n|\n  play n, release: 0.2\n  sleep 0.15\nend`),
  playSample: (name) => play(`${realTime}sample :${name}`),
};

// ── Editor and buffers ────────────────────────────────────────────────────

// ── The workspace (workspace.js): every set and its buffers, which shows, what arriving code does ──
// The one place they live: the editor shows a buffer of it, the tabs and the sets draw from it, and it keeps itself in
// the browser's storage. New sets have eight buffers; sets from native, or from before sets had sizes, their ten.
const workspace = createWorkspace({
  storage: { get: (k) => { try { return localStorage.getItem(k); } catch { return null; } }, set: (k, v) => localStorage.setItem(k, v), remove: (k) => { try { localStorage.removeItem(k); } catch {} } },
  starter: STARTER, formerStarters: FORMER_STARTERS, defaultSize: 8,
  warn: (text) => toast(text, true, 6000),
});
addEventListener("pagehide", () => workspace.flush());   // what was typed in the last moment, kept as the page goes
const editor = createEditor($("editor-mount"), { api, hooks, workspace });

// ── The buffer tabs (buffer-tabs.js), and the sets' chip in them (sets-view.js) ──
// A tap on a pad, or its shortcut: a switch, and with jam on what the mode says (run it, as Run does, layering; exec:
// stop what the buffers are playing, then run; show: only switch). A tap on the pad already showing runs it again.
const JAM_FADE = 0.05;   // seconds an exec's stop takes: a drum pad's cut, without the click
async function stopAndRun() {
  session?.stopGroup(BUFFERS_GROUP, JAM_FADE);   // every buffer's runs (Scheduler#stop_group): the cards play on
  await run();
}
function pickBuffer(i) {
  if (!workspace.showBuffer(i)) return;   // past the set's last buffer
  if (!tabs.jam.on) return;
  if (tabs.jam.mode === "run") run();
  else if (tabs.jam.mode === "exec") stopAndRun();
}
const tabs = createBufferTabs({ root: $("buffer-tabs"), workspace, title: (i) => keys.title(`Buffer ${i}`, `Tab${i}`), onPick: pickBuffer, store });

// native's auto-indent on run (on by default): the buffer is re-indented before it is read
const editorPrefs = { autoIndent: store.get("sp-auto-indent-on-run", true) };
// native's Accessibility menu: "Speak Run and Stop" (announce.js) and "Reduce Animations"
const a11yPrefs = { speakTransport: store.get("sp-speak-transport", true) };
setSpeakTransport(a11yPrefs.speakTransport);
const reduceMotionMedia = window.matchMedia("(prefers-reduced-motion: reduce)");
const motionPrefs = { reduce: store.get("sp-reduce-motion", null) ?? reduceMotionMedia.matches };
// stills the scope (native's reduce_motion leaves it paused) and the run and line flashes; style.css stops the rest
function setReduceMotion(on, chosen = false) {
  motionPrefs.reduce = on;
  if (chosen) store.set("sp-reduce-motion", on);
  document.body.classList.toggle("reduce-motion", on);
  scope.paused = on;
}
setReduceMotion(motionPrefs.reduce);
reduceMotionMedia.addEventListener("change", (e) => { if (store.get("sp-reduce-motion", null) == null) setReduceMotion(e.matches); });

// the Run button's glyph lights in the accent as a run goes, and fades back: two names for the one flash, so a Run
// straight after another starts it again (as the editor's own kick does)
let runFlashB = false;
function flashRunButton() {
  const b = $("btn-run");
  b.classList.remove("run-flash-a", "run-flash-b");
  b.classList.add((runFlashB = !runFlashB) ? "run-flash-b" : "run-flash-a");
}

async function run() {
  clearError();
  if (editorPrefs.autoIndent) editor.command("Align");
  if (!motionPrefs.reduce) { editor.flash(); flashRunButton(); }
  announce("Run started", false, Announcement.Transport);
  const job = await play(editor.getCode(), { buffer: editor.active });
  if (job != null && audioRestarted) { audioRestarted = false; setEngineStatus("running"); }
  if (job != null) logInfo(`Starting run ${job}`);
  logs.add("Runtime", job != null ? `run ${job} started from buffer ${editor.active}` : `a run from buffer ${editor.active} did not start`);
}

function stop() {
  stops++;   // a run still waiting for the engine to start does not start (play)
  loadingStop?.cancel();
  insight.stopped(session?.clockNow() ?? null);
  session?.stop();
  loopScopes.clear();
  paintLoopScopes();
  paintWaits([]);
  lastJobs = "";
  docs?.jobs([]);
  for (const d of cardDecks()) d.groups([]);
  logInfo("Stopping all runs");
  logs.add("Runtime", "stopped all runs");
  announce("Stopped", false, Announcement.Transport);
}

$("btn-run").addEventListener("click", run);
$("btn-stop").addEventListener("click", stop);
document.querySelector("#site-nav .sn-stop").addEventListener("click", stop);   // the bar's quiet stop (siteNav is named further down)
// a toggled button tells a screen reader so, as native's checkable actions do: its "on" class, as aria-pressed
for (const b of [$("btn-scope"), $("btn-info"), $("btn-help"), $("btn-prefs"), $("btn-zen"), ...$("drawer-rail").querySelectorAll("[data-drawer]")]) {
  const paint = () => b.setAttribute("aria-pressed", String(b.classList.contains("on")));
  new MutationObserver(paint).observe(b, { attributes: true, attributeFilter: ["class"] });
  paint();
}
$("btn-size-up").addEventListener("click", () => editor.setFontSize(editor.fontSize() + 1));
$("btn-size-down").addEventListener("click", () => editor.setFontSize(editor.fontSize() - 1));
// Run, Stop and the rest are native's shortcuts, wherever the focus is (the keys, at the end)
// Escape, and Emacs's Ctrl-G, put away what is up: the preferences and the error card. Not when the editor's own
// popup has taken the key (its completion, its search): that closes first, and the next press clears the error.
document.addEventListener("keydown", (e) => {
  if (shortcutEditor.isOpen || e.defaultPrevented) return;
  const quit = e.key === "Escape" || (e.ctrlKey && !e.metaKey && !e.altKey && (e.key === "g" || e.key === "G"));
  if (!quit) return;
  if (e.key === "Escape") showPrefs(false);
  if (shownError) clearError();
});

// ── Log, cues, errors ─────────────────────────────────────────────────────

const logBox = $("log"), cueBox = $("cues");
let lastHead = "", lastLine = null;

// A pane's lines live in a buffer; the document holds the window you can see.
//
// These panes take hundreds of lines a second from a controller, and a browser lays out a scrolling box every
// time something is added to it, so the cost of one line grows with how many the box holds. The pane keeps its
// lines here and the document holds WINDOW of them: what a line costs does not depend on how many came before
// it, and the buffer can be large.
//
// The buffer is deliberately the whole state of the pane, in order, owned here. That is the shape a shared
// memory segment would have (the runtime writing, the page rendering a window of it), so this can become that
// without the rendering changing.
const MAX_LOG = 4000;        // lines a pane remembers: nearly free, as only WINDOW of them are in the document
const WINDOW = 150;          // lines in the document at once: a tall pane shows ~40

// An entry is a line's data and how to build it; its node exists only while the line is in the window.
// The window is drawn in the box's shadow root (shadow.js: lines arriving as a program plays are out of sight of a
// page-wide watcher's), and the box itself scrolls. It says when it has nothing to show (data-empty), and the box
// shows its placeholder (style.css).
const panes = new Map();     // box → { rows, first, pending, following, built, inner }
const paneOf = (box) => {
  let pane = panes.get(box);
  if (!pane) panes.set(box, (pane = { rows: [], first: 0, pending: [], following: true, built: new Set(), inner: shadowPane(box) }));
  return pane;
};
let painting = false;

// The window is the last WINDOW lines while the pane follows the end, else the ones around where it was left.
function drawPane(box, pane) {
  const rows = pane.rows;
  const first = pane.following ? Math.max(0, rows.length - WINDOW) : Math.min(pane.first, Math.max(0, rows.length - WINDOW));
  pane.first = first;
  const want = rows.slice(first, first + WINDOW);
  box.toggleAttribute("data-empty", !rows.length);   // only as it changes: the same state is no change to the page
  const have = pane.inner.children;
  let same = have.length === want.length;
  for (let i = 0; same && i < want.length; i++) same = have[i] === want[i].node;
  if (same) return;
  const batch = document.createDocumentFragment();
  for (const e of want) batch.appendChild(e.node ??= e.make());
  pane.inner.replaceChildren(batch);
  // a line that has left the window gives its node back; the buffer keeps only what the line says
  const keep = new Set(want);
  for (const e of pane.built) if (!keep.has(e)) e.node = null;
  pane.built = keep;
  if (pane.following) box.scrollTop = box.scrollHeight;
}

function paintPanes() {
  painting = false;
  for (const [box, pane] of panes) {
    if (!pane.pending.length) continue;
    pane.rows.push(...pane.pending);
    pane.pending.length = 0;
    if (pane.rows.length > MAX_LOG) {
      const drop = pane.rows.length - MAX_LOG;
      for (const e of pane.rows.splice(0, drop)) e.node = null;
      pane.first = Math.max(0, pane.first - drop);
    }
    drawPane(box, pane);
  }
}

function append(box, entry) {
  const pane = paneOf(box);
  pane.pending.push(entry);
  if (pane.pending.length > MAX_LOG) pane.pending.splice(0, pane.pending.length - MAX_LOG);
  if (!painting) { painting = true; requestAnimationFrame(paintPanes); }
}

// Scrolled off the end: the pane stops following and shows where it was left, and older lines are brought in as
// it reaches the top of the window. Scrolled back to the end: it follows again.
// Only the player's own scrolling lets go of the end — a wheel, a touch, a key, the scrollbar dragged. The browser
// scrolls a box by itself too (Chromium's scroll anchoring as the window is redrawn, a box squeezed by the panes
// around it or shown again), and taking that for the player reading back would freeze the pane at its first lines
// while the lines go on arriving underneath.
const USER_SCROLL_MS = 1000;
for (const box of [logBox, cueBox]) {
  paneOf(box);   // its shadow root, from the start
  let userAt = -Infinity;
  const byUser = () => { userAt = performance.now(); };
  for (const ev of ["wheel", "touchmove", "pointerdown"]) box.addEventListener(ev, byUser, { passive: true });
  box.addEventListener("keydown", (e) => { if (/^(Arrow(Up|Down)|Page(Up|Down)|Home|End| )$/.test(e.key)) byUser(); });
  box.addEventListener("scroll", () => {
    const pane = paneOf(box);
    const atBottom = box.scrollHeight - box.scrollTop - box.clientHeight < 40;
    const user = performance.now() - userAt < USER_SCROLL_MS;
    if (!atBottom && pane.following && !user) { box.scrollTop = box.scrollHeight; return; }   // not the player: back to the end
    if (atBottom !== pane.following) { pane.following = atBottom; if (atBottom) drawPane(box, pane); return; }
    if (!pane.following && box.scrollTop < 60 && pane.first > 0) {
      const was = box.scrollHeight;
      pane.first = Math.max(0, pane.first - Math.floor(WINDOW / 2));
      drawPane(box, pane);
      box.scrollTop += box.scrollHeight - was;    // the older lines go above: stay where the eye is
    }
  }, { passive: true });
}

// Clearing a pane drops its buffer too, or what it was keeping would come back on the next line.
const clearPane = (box) => { const p = paneOf(box); p.rows.length = 0; p.pending.length = 0; p.first = 0; p.following = true; p.built.clear(); drawPane(box, p); };

// Native's times: to four places, trimmed, a whole second as 270.0
const fmtTime = (t) => { const x = Math.round(t * 10000) / 10000; return Number.isInteger(x) ? x.toFixed(1) : String(x); };
const breakLog = () => { lastHead = ""; lastLine = null; };
function logHead(r) {
  const thread = r.name ? `, thread: :${r.name}` : "";
  const head = `{run: ${r.job}, time: ${fmtTime(r.t ?? 0)}${thread}}`;
  if (head === lastHead) return;
  lastHead = head;
  lastLine = null;
  append(logBox, { make: () => el("div", "log-head", head) });
}

// A line under its header: the one before it becomes ├─, as native draws the tree
function logLine(text, cls) {
  // the line before this one becomes ├─ — said on the entry, and on its node when it is one you can see
  if (lastLine) { lastLine.mid = true; lastLine.node?.classList.add("mid"); }
  const body = String(text).replace(/\n[ \t]+/g, "\n");   // the runtime's continuation indent, set for its own column: the line wraps to ours
  const line = { mid: false, make: () => el("div", line.mid ? `${cls} mid` : cls, body) };   // itself, not whatever lastLine is by then
  lastLine = line;
  append(logBox, line);
}

// The log line for a phone (style.css shows one or the other): the time, the thread in brackets,
// then the call on one line — a sample by its name, options without their braces, numbers in the accent
function logRow(r, out = false) {
  let text = String(r.text).replace(/\s+/g, " ").trim();
  text = text.replace(/^sample "<samples>", "([^"]+)\.\w+"(?:, \{(.*)\})?$/, (_, name, opts) => `sample :${name}${opts ? `, ${opts}` : ""}`);
  text = text.replace(/^(synth :\S+|control \S+|kill \S+), \{(.*)\}$/, "$1, $2");
  const t = fmtTime(r.t ?? 0), who = r.name ? `[${r.name.replace(/^live_loop_/, "")}]` : "";
  append(logBox, { make: () => {
    const row = el("div", `log-row${out ? " log-row-out" : ""}`);
    row.appendChild(el("span", "log-t", t));
    row.appendChild(el("span", "log-th", who));
    const msg = el("span", "log-msg");
    for (const part of text.split(/(-?\b\d+(?:\.\d+)?\b)/)) msg.appendChild(/^-?\d/.test(part) ? el("span", "log-num", part) : document.createTextNode(part));
    row.appendChild(msg);
    return row;
  } });
}

function log(r) {
  logHead(r);
  logLine(r.text, "log-line");
  logRow(r);
}


function output(r) {
  logHead(r);
  logLine(r.text, "log-line log-out");
  logRow(r, true);
}

function logInfo(text) {
  breakLog();
  append(logBox, { make: () => {
    const line = el("div", "log-info");
    line.appendChild(el("span", "", `=> ${text}`));
    return line;
  } });
}

// A cue as native lists it: its path, then its data ([] for none)
function addCue(path, data, t = null) {
  // whatever sent it — a program's cue, a live loop going round, a knob on a MIDI controller — the completion
  // learns the path here, since this is the one place they all pass through (sync takes a cue path: api.js)
  api.addCuePath(path, data);
  const when = t == null ? new Date().toLocaleTimeString() : `time: ${fmtTime(t)}`;
  append(cueBox, { make: () => {
    const row = el("div", "cue-row");
    row.appendChild(el("span", "cue-t", t == null ? "" : fmtTime(t)));   // a phone's column (style.css)
    // a break allowed after each slash (a <wbr>: copied, the address is as it was), so a long one wraps between its parts
    const cell = el("span", "cue-path");
    String(path).split(/(?<=\/)/).forEach((part, i) => { if (i) cell.appendChild(document.createElement("wbr")); cell.appendChild(document.createTextNode(part)); });
    cell.title = when;
    row.append(cell, el("span", "cue-data", data || "[]"));
    return row;
  } });
}

$("log-clear").addEventListener("click", () => { clearPane(logBox); breakLog(); });
$("cue-clear").addEventListener("click", () => clearPane(cueBox));

let errorLine = null;
let shownError = null;   // what the card says (friendly.js), for Jump, Fix it and Copy
// backticked code in an explanation, set as code; the rest as text
function codeSpans(node, text) {
  node.replaceChildren();
  String(text).split("`").forEach((part, i) => { if (part) node.append(i % 2 ? el("code", "", part) : part); });
}
// the line of code with the place marked, as the editor marks it: the rest recedes, a word gets the zigzag, a single
// character or a point a ^ beneath
function paintErrorCode(box, number, text, from, to) {
  box.replaceChildren(el("span", "err-code-num", String(number)));
  const code = el("span", "err-code-text");
  code.append(text.slice(0, from));
  if (to - from > 1) code.append(el("span", "err-code-mark", text.slice(from, to)));
  else if (to > from) code.append(el("span", "err-code-point", text.slice(from, to)));
  else code.append(el("span", "err-code-caret"));
  code.append(text.slice(Math.max(from, to)));
  box.append(code);
}
const plainText = (t) => String(t).replaceAll("`", "");

// A program's own synths (load_synthdef and the .json beside its .scsyndef): each a synth as a built-in is — in the docs
// under Your synths, offered by completion, played on its page's live synth — or, with no metadata, a page made from its
// synthdef's controls. A mistake in the metadata is said as an error is, pointing at the file.
function registerSynths(metas) {
  for (const x of metas) {
    if (x.error) { showError({ class: "SynthMetadataError", message: x.error }); continue; }
    const page = x.meta ? pageFromMeta(x.meta, x.url) : x.derived ? pageFromSynthdef(x.name, x.derived, x.url) : null;
    if (!page) continue;
    const at = synths.pages.findIndex((p) => p.key === page.key);
    if (at >= 0) synths.pages[at] = page; else synths.pages.push(page);
    api.addSynth(page);
    logs.add("Runtime", page.derived ? `:${page.key} has no ${page.key}.json beside it: its controls from its synthdef` : `:${page.key}: its metadata installed, a standard synth`);
  }
  docs?.refreshList();
}

function showError(r, { warning = false } = {}) {
  // a card's own run (the docs' demos, the cards, the site's): its error is the card's to say, on the card (ui/deck.js),
  // not the editor's pane over the code with a line of a buffer marked that has nothing to do with it. A run whose
  // job is not known yet, while a card is starting and no buffer is, is the card's too (the deck keeps it for it).
  if (r.job == null || jobBuffer.get(r.job) == null) {
    const deck = cardDecks().find((d) => d.owns(r.job)) ?? (startingBuffer == null ? cardDecks().find((d) => d.starting) : null);
    if (deck) { if (!warning) deck.error(r); return; }
  }
  const buffer = (r.job != null ? jobBuffer.get(r.job) : undefined) ?? startingBuffer ?? undefined;
  // a heads-up (the code playing on) takes the card only from a buffer's run, and never from an error on show
  if (warning && (buffer == null || (shownError && !shownError.warning))) return;
  // native's two kinds: a syntax error is "Syntax Error" with the parser's words; its record has no line of its own
  // (-1), the message says it ("file run-1 line 8:12: …", the column the runtime adds), so it is read from there
  const syntax = r.class === "SyntaxError";
  let atLine = r.line > 0 ? r.line : null, atCol = null, message = String(r.message ?? "");
  if (syntax) {
    const m = /^(?:file \S+ )?line (\d+)(?::(\d+))?:\s*/.exec(message);
    if (m) { atLine ??= Number(m[1]); atCol = m[2] ? Number(m[2]) : null; message = message.slice(m[0].length); }
  }
  // the code as it ran: the buffer's, less what play() put before it on line 1
  const ran = (r.job != null ? programs.get(r.job) : null) ?? (lastRun?.buffer === buffer ? lastRun : null);
  const code = buffer != null ? ran?.code ?? null : null;
  if (atCol && atLine === 1 && ran?.prefix) atCol = Math.max(1, atCol - ran.prefix);
  const e = explainError({ syntax, cls: r.class, message, line: atLine ?? 0, col: atCol, code: code ?? "", thread: r.name, fault: r.fault }, known);
  const line = code != null && e.line ? e.line : atLine;
  const where = [buffer != null ? `buffer ${buffer}` : null, line ? `line ${line}` : null].filter(Boolean).join(", ");
  const title = r.title ?? (warning ? "Heads up" : syntax ? "Syntax Error" : "Runtime Error");   // a link's own: Link Error (loadFromHash)
  shownError = { ...e, buffer, line, where, title, code, thread: r.name, warning };

  const card = $("error-pane");
  card.classList.toggle("syntax", syntax);
  card.classList.toggle("warning", warning);
  // the line in the editor and the strip under it in the card's colour
  $("editor-column").style.setProperty("--err-mark", warning ? "var(--KeywordForeground)" : syntax ? "var(--MarkerBackgroundSyntax)" : "var(--MarkerBackground)");
  $("err-title").textContent = title;
  $("err-where").textContent = where;
  codeSpans($("err-message"), e.headline);
  $("err-hint").hidden = !e.hint;
  if (e.hint) codeSpans($("err-hint"), e.hint);
  const text = code != null && line ? code.split("\n")[line - 1] : null;
  $("err-code").hidden = text == null;
  if (text != null) paintErrorCode($("err-code"), line, text, e.from ?? 0, e.to ?? 0);
  paintFix();
  $("err-example").hidden = !e.example;
  if (e.example) $("err-example").querySelector("code").textContent = e.example.code;
  $("err-details").textContent = `${e.reason}${r.name ? `\nthread: :${r.name}` : ""}`;
  $("err-details").hidden = true;
  $("err-more").textContent = "Details";
  card.hidden = false;

  const said = plainText(`${e.headline}${e.hint ? ` ${e.hint}` : ""}`);
  announce(`${title}${where ? `, ${where}` : ""}. ${said}`, true, Announcement.Error);
  // the strip under the editor says the same, for a phone whose card is out of view
  $("error-strip").querySelector(".es-title").textContent = `${title}${where ? `: [${where}]` : ""}`;
  $("error-strip").querySelector(".es-msg").textContent = plainText(e.headline);
  $("error-strip").hidden = false;
  logs.add("Runtime", `${warning ? "warning" : syntax ? "syntax error" : "error"}${where ? ` [${where}]` : ""}: ${e.reason}${r.name ? ` (thread :${r.name})` : ""}`);
  errorLine = line && (buffer == null || buffer === editor.active) ? line : null;
  if (!warning) for (const d of cardDecks()) d.error(r);
  $("err-jump").hidden = !errorLine;
  if (errorLine) editor.markError(errorLine, text != null ? e.from : undefined, text != null ? e.to : undefined);
  if (warning) return;   // the runtime's own log line says it (… - ignored)
  breakLog();
  const errLine = `=> Error${where ? ` [${where}]` : ""}: ${plainText(e.headline)}`;
  append(logBox, { make: () => {
    const row = el("div", "log-info");
    row.appendChild(el("span", "", errLine));
    return row;
  } });
}
// the fix as one button named for what it does ("Use `4`"), the line as it would read in its tooltip; offered only
// while the line still reads as it ran (an edit since leaves it to the words)
function paintFix() {
  const e = shownError, f = e?.fix;
  const now = f && e.buffer === editor.active ? editor.lineText(f.line) : null;
  const ok = f && now != null && now === e.code?.split("\n")[f.line - 1];
  const stripFix = $("error-strip").querySelector(".es-fix");
  $("err-fix").hidden = stripFix.hidden = !ok;
  if (!ok) return;
  codeSpans($("err-fix"), f.label);
  codeSpans(stripFix, f.label);
  $("err-fix").title = `Line ${f.line} will read:\n${now.slice(0, f.from)}${f.insert}${now.slice(f.to)}`;
}
function clearError() {
  $("error-pane").hidden = true;
  $("error-strip").hidden = true;
  shownError = null;
  editor.clearError();
}
const jumpToError = () => { if (!errorLine) return; editor.markError(errorLine, shownError?.from, shownError?.to); editor.goTo(errorLine, shownError?.from ?? 0); };
$("error-strip").querySelector(".es-go").addEventListener("click", jumpToError);
$("error-strip").querySelector(".es-fix").addEventListener("click", () => $("err-fix").click());
$("err-jump").addEventListener("click", jumpToError);
$("err-close").addEventListener("click", clearError);
$("error-pane").addEventListener("keydown", (ev) => { if (ev.key === "Escape") { ev.stopPropagation(); clearError(); editor.focus(); } });
$("err-fix").addEventListener("click", () => {
  const f = shownError?.fix;
  if (!f || shownError.buffer !== editor.active || editor.lineText(f.line) !== shownError.code?.split("\n")[f.line - 1]) return paintFix();
  editor.applyFix(f);
  clearError();
  toast(`Fixed line ${f.line}: Run to hear it`);
});
$("err-more").addEventListener("click", () => {
  const d = $("err-details");
  d.hidden = !d.hidden;
  $("err-more").textContent = d.hidden ? "Details" : "Hide details";
});
$("err-copy").addEventListener("click", () => {
  const e = shownError;
  if (!e) return;
  const lineText = e.code != null && e.line ? e.code.split("\n")[e.line - 1] : null;
  const report = [`${e.title}${e.where ? ` [${e.where}]` : ""}`, plainText(e.headline), e.hint && plainText(e.hint), lineText != null && `line ${e.line}: ${lineText}`, e.reason, e.thread && `thread: :${e.thread}`].filter(Boolean).join("\n");
  navigator.clipboard?.writeText(report).then(() => { $("err-copy").textContent = "Copied ✓"; status("Copied the error to the clipboard"); setTimeout(() => { $("err-copy").textContent = "Copy"; }, 1500); }, () => {});
});

let lastJobs = "";
let soundAt = -Infinity;   // the last sound sent (onRecord): a run's first note sounds before the status says it has a job
let liveJobs = 0;   // for the leave prompt: a reload mid-performance asks first (Safari keeps Cmd+R for itself: a page cannot take it)
let liveGroups = [];   // the runtime's live groups, as the last status said (a reload plays the buffers' again)
function showJobs(s) {
  liveJobs = s.jobs.length;
  liveGroups = s.groups;
  // sounding: a thread of any run still going, or a group still live — which counts a card's one-shot while its note
  // rings out and a reverb's tail, where no thread is left (the runtime's word, in the status)
  const sounding = liveJobs > 0 || s.groups.length > 0 || performance.now() - soundAt < 1000;
  document.body.classList.toggle("sounding", sounding);   // the bar's quiet stop shows, its rings on the mix
  const rings = sounding && !loadingStop?.showing;          // once the load's comet is off it (the first sound: onRecord)
  if (rings !== stopRings.live) stopRings.set(rings);
  let pruned = false;
  for (const [node, sc] of loopScopes) if (!s.jobs.includes(sc.job)) { loopScopes.delete(node); pruned = true; }
  if (pruned) paintLoopScopes();
  paintWaits(s.threads || []);
  docs?.jobs(s.jobs);
  for (const d of cardDecks()) d.groups(s.groups);   // a card's group gone quiet: the runtime says
  insight.status(s);
  const key = JSON.stringify([s.jobs, s.named]);
  if (key === lastJobs) return;
  lastJobs = key;
  // the runs going, as their ids ([1, 3]); the live loops they have going, on hover
  const loops = s.named.filter((n) => n.startsWith("live_loop_")).map((n) => `:${n.slice(10)}`);
  const el = $("status-jobs");
  el.textContent = s.jobs.length ? `[${s.jobs.join(", ")}]` : "";
  el.title = s.jobs.length ? `Runs going: ${s.jobs.join(", ")}${loops.length ? ` · live loops: ${loops.join(" ")}` : ""}` : "";
}

// ── Flight recorder: the data behind a crackle, a pop or a jitter ─────────

const flight = createFlightRecorder({
  session: () => session,
  programs: () => [...programs.values()],
  versions: () => ({ language: "Sonic Pi v5.0.0", runtime: versions.runtime, supersonic: SUPERSONIC_VERSION }),
});
// off unless Preferences says otherwise: while it records, the page samples its clocks ten times a second
if (store.get("sp-flight", false)) flight.start();
// a mark or a report asked for with the recorder off: said so, and where it is turned on
const flightOff = () => { if (flight.recording) return false; toast("the flight recorder is off: turn it on in Preferences"); return true; };
// the status bar stays quiet about it: the report keeps the counts, and Preferences (Flight recorder) and the web's
// own keys, FlightMark and FlightSave (at the end), mark a moment and save a report

// ── Live records: flashes, the threads view, MIDI out ────────────────────

// The process table, read out of the runtime's memory each frame; each
// thread's id and name come from the records, once per thread.
function readProcesses() {
  if (!session) return null;
  const table = session.processTable();
  const width = PROCESS_FIELDS.length;
  const rows = [];
  for (let i = 0; i + width <= table.length; i += width) {
    const row = {};
    for (let f = 0; f < width; f++) row[PROCESS_FIELDS[f]] = table[i + f];
    if (row.kind === 9) {          // a group: one of the page's own, named for what it holds
      row.id = ""; row.name = "";
      row.label = row.group === BUFFERS_GROUP ? "buffers" : row.group === CARDS_GROUP ? "cards" : row.group > CARDS_GROUP ? `card ${row.group - CARDS_GROUP}` : row.group >= BUFFER_GROUP ? `buffer ${row.group - BUFFER_GROUP}` : `group ${row.group}`;
    } else if (row.kind >= 6) {           // an fx or a sound, named by the record that started its synth
      const n = session.nodeName(row.node);
      row.id = "";
      row.name = n?.synth ?? "";
      row.sample = n?.buf ?? "";
    } else {
      const t = session.threadName(row.uid);
      row.id = t?.id ?? "";
      row.name = t?.name ?? "";
    }
    rows.push(row);
  }
  return rows;
}

// the flight recorder's marks (glitches, late bundles, stalls) are the host's news
flight.on((e) => { if (e.type === "mark") logs.add("Host", `${e.mark.kind}: ${e.mark.detail}`); });

const insight = createInsight(shadowPane($("insight-pane")), {   // in its shadow root (shadow.js)
  processes: readProcesses,
  stop: (uid) => session?.stopSubtree(uid, 0.25),   // the threads view: a node and everything under it, faded (Scheduler#stop_subtree)
  synthDefaults: (synth) => synthOpts.get(synth.replace(/^sonic-pi-/, "")) ?? null,
  now: () => session?.clockNow() ?? null,
  jump: (line, job) => {
    const buffer = jobBuffer.get(job);
    if (buffer != null && buffer !== editor.active) editor.switchBuffer(buffer);
    editor.goToLine(line);
  },
});

// the records whose line flashes, as native's code flash: a sound, and what acts on one
const FLASHES = new Set(["synth", "control", "kill", "midi", "output", "cue"]);
// every deck of cards (ui/deck.js) the session tells of jobs, errors and sounds: the panes' and the site's
const cardDecks = () => [docs, quickstart, infoApi].filter(Boolean);

function onRecord(r, at, stale = false) {
  if (r.kind === "synth") {
    navScope.wake?.();   // the bar's scope, resting in silence, draws the sound
    soundAt = performance.now();
    if (!document.body.classList.contains("sounding")) refreshJobs();   // the first sound lights the stop now, not at the next status
    loadingStop?.sounding();   // and its comet gives way to its rings
  }
  flight.record(r);
  insight.record(r);
  if (stale) return;   // the past, after the page was held: kept above, painted nowhere (sonic_pi.js deliver)
  if (r.kind === "warning") return showError(r, { warning: true });   // an unknown opt's heads-up: the code plays on
  if (r.kind === "studio" && r.op === "link_bpm") setLinkBpm(r.value, true);      // a program's set_link_bpm!
  if (r.kind === "load_example") return loadExample(r.name);
  if (r.kind === "cue") addCue(r.address, r.val, r.t);   // every cue as the runtime sends it (gui-stream.js): a live loop's each time round, cue's, set's
  if (r.kind === "synth" && r.synth === "sonic-pi-fx_scope_out" && r.node != null && r.args?.scope_num != null && /^live_loop_/.test(r.name)) {
    // the fx record can land before run() has said which buffer the job is: the run starting says
    loopScopes.set(r.node, { name: r.name.slice(10), slot: r.args.scope_num, job: r.job, buffer: jobBuffer.get(r.job) ?? startingBuffer });
    paintLoopScopes();
  }
  if (r.kind === "fx_free" && loopScopes.delete(r.node)) paintLoopScopes();
  if (!r.line || r.job == null) return;
  const deck = cardDecks().find((d) => d.owns(r.job) || d.starting);   // a run's first sound is recorded before play returns its job: the flash, due sched-ahead later, finds it known
  if (deck) {   // a card's run: its strip draws the sound, and its card flashes the line, as the editor would
    deck.record(r);
    if (!FLASHES.has(r.kind) || (r.kind === "synth" && r.synth.startsWith("sonic-pi-fx_")) || (r.kind === "cue" && r.address?.startsWith("/live_loop/")) || motionPrefs.reduce) return;   // a loop's own cue each time round is not a flash
    const delay = Math.max(0, (r.time - session.clockNow()) * 1000);
    setTimeout(() => deck.flash(r.job, r.line), delay);
    return;
  }
  const buffer = jobBuffer.get(r.job) ?? startingBuffer;
  if (buffer == null || buffer !== editor.active) return;
  if (r.kind === "synth" && r.synth.startsWith("sonic-pi-fx_")) return;
  if (!FLASHES.has(r.kind)) return;
  // at the moment it sounds, not when it was scheduled
  const delay = Math.max(0, (r.time - session.clockNow()) * 1000);
  if (motionPrefs.reduce) return;
  setTimeout(() => { if (editor.active === buffer) editor.flashLine(r.line); }, delay);
}

async function loadExample(name) {
  await needReference();   // the examples are the reference's (above)
  for (const g of examples.groups) {
    const e = g.examples.find((x) => x.key === name);
    if (e) {
      editor.setCode(`# ${e.title}\n${e.code}`);
      toast(`loaded example :${name}`);
      return;
    }
  }
  showError({ class: "IOError", message: `Error - no example found with name: :${name}` });
}

// MIDI out: a record's message as MIDI bytes, to the ports it names, at its time.
// What a program's midi_* verb is called on the engine's side. Ours name a couple of things differently
// (aftertouch is MIDI's poly pressure), and a channel of -1 — every channel — is 0 there.
const MIDI_OUT = {
  "/note_on": "note_on", "/note_off": "note_off", "/control_change": "control_change",
  "/aftertouch": "poly_pressure", "/program_change": "program_change",
  "/channel_pressure": "channel_pressure", "/pitch_bend": "pitch_bend",
  "/clock": "clock", "/start": "start", "/stop": "stop", "/continue": "continue", "/raw": "raw",
};

/**
 * A program's MIDI, out through the engine: "/clockwork/midi/out/<kind> <port> <channel> <values…>" in a
 * bundle for the moment it should sound. The engine forwards it to the host front, which puts it on the port
 * with that time — and the browser schedules the send itself, which is tighter than racing to deliver it.
 *
 * A beat of clock is 24 ticks across the beat; the engine's own clock/beat needs an audio thread to spread it
 * from, which the worklet has not got, so they are sent here as 24 bundles at their times.
 */
function midiSend(r) {
  if (!midi) { enableMidi(); return; }
  if (!engineRef || !session) return;
  const verb = MIDI_OUT[r.path];
  const [port, ...rest] = r.args;
  const at = r.time;
  const at_ = (time, kind, args) => { try { engineRef.sendOSC(sp.oscSchedule(time, `/clockwork/midi/out/${kind}`, [String(port), ...args])); } catch { /* the engine is being rebuilt */ } };
  const send = (time, args) => at_(time, verb, args);
  if (r.path === "/clock_beat") {
    const beat = rest[0] / 24;
    for (let i = 0; i < 24; i++) at_(at + beat * i, "clock", []);
    return;
  }
  if (!verb) return;
  if (verb === "raw") return send(at, rest.map((v) => v | 0));
  if (["clock", "start", "stop", "continue"].includes(verb)) return send(at, []);
  const channel = rest[0] === -1 ? 0 : rest[0];          // -1 here, 0 there: every channel
  send(at, [channel, ...rest.slice(1)]);
}

// ── Drawer: docs, quickstart and threads ──────────────────────────────────

let docs = null, quickstart = null;
// as native's: "Copied to clipboard", "Inserted … at the cursor" (a screen reader hears the action was done)
const clipboard = async (text) => { try { await navigator.clipboard.writeText(text); status("Copied to clipboard"); } catch { toast("copy failed", true); } };
const paneHooks = {
  run: (code) => play(code),
  stop: (job) => session?.stopJob(job),   // the docs pane's sample buttons: one job
  stopGroup: (group, fade) => session?.stopGroup(group, fade),
  group: nextCardGroup,
  insert: (code) => { editor.insertAtCursor(code); status("Inserted at the cursor"); },
  copy: clipboard,
  playSample: hooks.playSample,
  play: (code, opts) => play(code, opts),
  scope: () => (scope.analysers ? scope : null),
  scopeFrame,
  now: () => session?.clockNow() ?? null,
  synthDefaults: (synth) => synthOpts.get(synth.replace(/^sonic-pi-/, "")) ?? null,
  // a synth's definition, which the engine fetches the first time it plays: loaded yet, and load it (the engine
  // started for it, if it has not been)
  loopScopes: () => loopScopePrefs,   // a card's loop scopes (ui/card.js): the preferences' Show and Scroll
  synthReady: (key) => !!session?.bridge?.synthDefReady(`sonic-pi-${key}`),
  loadSynth: async (key) => (await ensureSession()).bridge?.synthDef(`sonic-pi-${key}`),
};

const DRAWER_TITLES = { docs: "Help - Documentation", quickstart: "Help - Quickstart cards", logs: "System log", insight: "Threads", debug: "Debug - SuperSonic", output: "Scope, log and cues" };
const phoneMedia = window.matchMedia("(max-width: 760px)");
// On a phone the scope, log and cues are a pane of the drawer's ("output"), so the rail stays with them:
// the sidebar moves into the drawer's panes for it, and back to its place for anything else.
const sidebarHome = document.createComment("sidebar");
$("sidebar").before(sidebarHome);
function placeSidebar(which) {
  const bar = $("sidebar"), inDrawer = bar.parentElement === $("drawer-panes");
  if (which === "output" && !inDrawer) $("drawer-panes").appendChild(bar);
  else if (which !== "output" && inDrawer) sidebarHome.after(bar);
  else return;
  window.dispatchEvent(new Event("resize"));   // the scope's canvas takes its new size
}
// ── The bottom panel, as one state ────────────────────────────────────────
// What it shows is one value: nothing, a pane of the rail's (the cards, the docs, the threads, the system log, and
// on a phone the scope, log and cues), or the preferences. Everything shown follows from it — which icon is lit,
// which way the chevron points, whether the code has the screen — so there is no state where a lit icon stands over
// a panel that is not there, or a panel holds something invisible.
//
//   a rail icon      → that pane, or nothing when it was already showing
//   the help button  → nothing while a pane shows, else the pane last shown
//   the π button     → the preferences, or nothing when they already show
//   the rail chevron → nothing
//   the divider's    → nothing while something shows, else what was shown last (it brings the panel back)
//   Escape           → nothing, while the preferences show
//   a phone's width  → the scope, log and cues are the sidebar's on a wide screen: that pane closes
const PANES = ["quickstart", "docs", "output", "insight", "debug", "logs"];
const isPane = (p) => PANES.includes(p);
let lastPanel = store.get("sp-panel-last", "quickstart");        // what the divider's chevron brings back
let lastPane = store.get("sp-last-pane", "quickstart");          // what the help button opens

function setPanel(next) {
  if (next === "output" && !phoneMedia.matches) next = "";   // a wide screen has the sidebar in view already
  if (next === "docs" && !docs) docs = null;
  const now = panelNow();
  if (next) { lastPanel = next; store.set("sp-panel-last", next); }
  if (isPane(next)) { lastPane = next; store.set("sp-last-pane", next); }
  if (isPane(next)) helpHint?.done();   // help found: the note that pointed at it goes, for good
  placeSidebar(next);
  // a panel asked for is a panel to read: the code keyboard makes way for it (a tap on the code brings it back)
  if (next && document.body.classList.contains("kbd-open")) codeKeyboard.close();
  document.body.dataset.drawer = isPane(next) ? next : "";
  document.body.classList.toggle("prefs-open", next === "prefs");
  document.body.classList.toggle("panel-open", !!next);
  if (next !== "prefs") store.set("sp-drawer", isPane(next) ? next : "");   // the preferences pass; the pane behind them is what a reload comes back to
  // the icons: one lit, and only while what it opens is on show
  $("btn-help").classList.toggle("on", isPane(next));
  $("btn-prefs").classList.toggle("on", next === "prefs");
  for (const b of $("drawer-rail").querySelectorAll("[data-drawer]")) b.classList.toggle("on", b.dataset.drawer === next);
  $("drawer-title").textContent = DRAWER_TITLES[next] ?? "";
  const say = next ? "Hide the panel" : "Show the panel";
  { const el = $("divider-grip"); el.title = say; el.setAttribute("aria-label", say); }
  if (!isPane(next)) setPanelFull(false);   // full size is a help pane's: gone with it
  if (now !== next) status(next ? `Showing ${DRAWER_TITLES[next] ?? "the panel"}...` : "Hiding the panel...");
  if (next === "prefs") buildPrefs();
  if (next === "docs") withDocs();
  if (next === "quickstart") needReference().then(() => { quickstart ??= createQuickstart($("quickstart-pane"), quickstartData, paneHooks); requestAnimationFrame(() => quickstart.render()); });
  if (next === "logs") requestAnimationFrame(() => logs.shown());
  showDebug(next === "debug");
}
// Debug, as native's: the engine's live metrics, SuperSonic's own <clockwork-metrics> reading its shared memory, a
// few times a second and only while the pane shows. The element comes with the engine (supersonic/), and wants the
// engine to read: until it runs, the pane says how to start it. Under them native's two OSC logs: what was sent in,
// every sender's (SuperSonic's out:osc, from its watcher on the ingress ring: the runtime's worker's sends too),
// written out here with the page's own decoder, since out:text leaves bundles out and everything the runtime sends
// is one; and what came back to this page (in:text). Both cost something only while listened to, so the logs start
// when the pane is first opened, and keep on after. The node tree native has there is the Threads pane's.
const oscLogs = createLogs(shadowPane($("debug-pane").querySelector(".debug-logs")), ["To SuperSonic", "From SuperSonic"]);
let oscLogged = null;   // the engine the logs listen to
async function logOsc(engine) {
  if (!engine || oscLogged === engine) return;
  oscLogged = engine;
  const { decode } = await import(new URL("osc.js", location.href).href);
  engine.on?.("out:osc", (d) => {
    if (oscLogged !== engine) return;
    try { oscLogs.add("To SuperSonic", oscText(decode(d.oscData), d.timestamp)); }
    catch (e) { oscLogs.add("To SuperSonic", `<${d.oscData?.length ?? "?"} bytes: ${describe(e)}>`); }
  });
  engine.on?.("in:text", (m) => { if (oscLogged === engine) oscLogs.add("From SuperSonic", m?.text ?? ""); });
}
// a message as SuperSonic writes one (/s_new "sonic-pi-beep", 1001, …; a blob by its size); a bundle its messages,
// one a line, the first marked with how far ahead of its sending it is to sound (none: immediately)
function oscText(p, sentAt) {
  const arg = (a) => (typeof a === "string" ? JSON.stringify(a) : a instanceof Uint8Array ? `<${a.length} bytes>` : Array.isArray(a) ? `[${a.map(arg).join(", ")}]` : typeof a === "number" && !Number.isInteger(a) ? String(Math.round(a * 1e4) / 1e4) : String(a));
  if (Array.isArray(p)) return `${p[0]}${p.length > 1 ? " " + p.slice(1).map(arg).join(", ") : ""}`;
  const ahead = p.timeTag > 1 && sentAt ? `+${(p.timeTag - sentAt).toFixed(3)}s ` : "";
  return p.packets.map((q, i) => (i === 0 ? ahead : " ".repeat(ahead.length)) + oscText(q, sentAt)).join("\n");
}
// the metrics' counts of trouble marked while they are 0, so the CSS lights only the ones that are not (the element
// writes its numbers in place, a few times a second)
let zerosMarked = false;
function markZeros(el) {
  if (zerosMarked) return;
  zerosMarked = true;
  const paint = () => { for (const v of el.querySelectorAll('.ssm-value[data-kind="error"]')) v.toggleAttribute("data-zero", /^[\s0.–-]*$/.test(v.textContent)); };
  new MutationObserver(paint).observe(el, { subtree: true, childList: true, characterData: true });
  paint();
}
let debugOn = false, debugLoaded = null;
function showDebug(on) {
  debugOn = on;
  const el = $("debug-pane").querySelector("clockwork-metrics");
  if (!on) return el.disconnect?.();
  logOsc(engineRef);
  requestAnimationFrame(() => oscLogs.shown());   // the tails that follow the end go there
  debugLoaded ??= import(new URL("supersonic/metrics_component.js", location.href).href).catch((e) => { logs.add("Host", `the metrics did not load: ${describe(e)}`); });
  debugLoaded.then(() => {
    if (!debugOn || !engineRef) return;
    el.hidden = false;
    $("debug-pane").querySelector(".debug-idle").hidden = true;
    el.connect(engineRef, { refreshRate: 10 });
    markZeros(el);
  });
}
/** What the panel shows now: "" (nothing), a pane's name, or "prefs". */
function panelNow() { return document.body.classList.contains("prefs-open") ? "prefs" : document.body.dataset.drawer || ""; }
/** The panel shows this pane (or nothing, for ""); the help pane's own API. */
const openDrawer = (which) => setPanel(which);
/** The docs pane at a section (synths, fx, …) and a page of it. */
function showDocs(section, key = null) { openDrawer("docs"); withDocs((d) => d.show(section, key)); }
/** A pane's own button: it opens its pane, or puts it away when that is what shows. */
const toggleDrawer = (which) => setPanel(panelNow() === which ? "" : which);
/** The preferences, as a panel like the panes. */
function showPrefs(on) { if (on !== (panelNow() === "prefs")) setPanel(on ? "prefs" : ""); }
// Help, as native's: the help pane open or closed, on the pane it last showed; its rail picks the pane
$("btn-help").addEventListener("click", () => setPanel(isPane(panelNow()) ? "" : lastPane));
// A first visit's help pane starts closed (the editor has the room), so its button says where help is: the Δ itself
// swelling out from it, again and again, and a note beneath it pointing up at it, while the editor shows, until the
// pane has been opened once or the note put away, and never again after. The echo is the button's own (style.css,
// its --echo the Δ's own mask), growing out of the Δ's ink, not round its box. The note is placed on the glyph's centre, measured.
let helpHint = null;
function showHelpHint() {
  const btn = $("btn-help"), glyph = btn.querySelector(".tb-glyph");
  const el = document.createElement("div");
  el.id = "help-hint";
  el.innerHTML = `<div class="hh-note" role="note"><p><strong>Welcome to Sonic Pi!</strong><br>Toggle the documentation by clicking this glyph.</p><button type="button" class="sp-mini-btn primary">Got it</button></div>`;
  btn.style.setProperty("--echo", glyph.style.getPropertyValue("--icon"));
  btn.classList.add("hinting");
  document.body.appendChild(el);
  // followed each frame, not placed once: the toolbar's other parts change width (the engine's state, the time), so
  // the button moves along it with nothing resized to say so. Only while it can be seen (ui/shown.js), and resting
  // while the site's pages cover the editor: closing them wakes it (closeInfo)
  let at = "";
  const place = () => {
    if (infoOpen()) { el.hidden = true; at = ""; loop.rest(); return; }   // the editor's, not the site's pages'
    const r = glyph.getBoundingClientRect();
    const now = `${r.left + r.width / 2} ${r.top + r.height / 2} ${r.width}`;
    if (now !== at) {
      at = now;
      el.hidden = false;
      el.style.setProperty("--x", `${r.left + r.width / 2}px`);
      el.style.setProperty("--y", `${r.top + r.height / 2}px`);
    }
  };
  const loop = animateWhileShown(btn, place, { onStop: () => { el.hidden = true; at = ""; } });
  el.querySelector("button").addEventListener("click", () => helpHint.done());
  helpHint = { wake: () => loop.wake(), done() { loop.stop(); el.remove(); btn.classList.remove("hinting"); helpHint = null; store.set("sp-help-seen", true); } };
}

$("btn-prefs").addEventListener("click", () => setPanel(panelNow() === "prefs" ? "" : "prefs"));
for (const b of $("drawer-rail").querySelectorAll("[data-drawer]")) b.addEventListener("click", () => toggleDrawer(b.dataset.drawer));
// The panel full size: the help pane over the whole of the code's room (the editor and the scope, log and cues
// beside it), for reading the docs or the cards at length. Up makes it so; down steps back, to beside the code, and
// from there (as before) away. Not remembered: a reload comes back to the code.
let panelFull = false;
function setPanelFull(on) {
  on = on && isPane(panelNow());
  if (on === panelFull) return;
  panelFull = on;
  document.body.classList.toggle("panel-full", on);
  const hide = $("divider-grip"), say = on ? "Back to the code, the panel beside it" : "Hide the panel";
  hide.title = say; hide.setAttribute("aria-label", say);
  status(on ? "Help panel full size..." : "Help panel beside the code...");
  requestAnimationFrame(() => { quickstart?.render(); window.dispatchEvent(new Event("resize")); });   // the cards and the threads' canvases size themselves to the room
}
// the divider's chevrons: down puts the panel away (from full size, back beside the code first) and brings it back;
// up makes it full size
const toggleBottom = () => (panelFull ? setPanelFull(false) : setPanel(panelNow() ? "" : lastPanel || lastPane));
const growBottom = () => setPanelFull(true);
for (const [id, act] of [["divider-grip", toggleBottom], ["divider-full", growBottom]]) $(id).addEventListener("keydown", (e) => { if (e.key === "Enter" || e.key === " ") { e.preventDefault(); act(); } });
setPanel(store.get("sp-drawer", ""));   // a pane, not the preferences (those are for the session they are opened in); a first visit none, the editor the room

// ── Dividers: drag to resize ──────────────────────────────────────────────

// onTap(target): the pointer went down and up without moving (a finger wobbles a few px)
function dragDivider(handle, onMove, onDone, onTap) {
  handle.addEventListener("pointerdown", (e) => {
    if (e.button > 0) return;
    e.preventDefault();   // a resize, never the page's own drag: no text selected, no link or card picked up
    handle.setPointerCapture(e.pointerId);
    handle.classList.add("dragging");
    document.body.classList.add("resizing");
    const x0 = e.clientX, y0 = e.clientY, target = e.target;
    let moved = false;
    const move = (m) => {
      if (!moved && Math.hypot(m.clientX - x0, m.clientY - y0) < 4) return;
      moved = true;
      onMove(m);
    };
    const up = () => {
      handle.classList.remove("dragging");
      document.body.classList.remove("resizing");
      handle.removeEventListener("pointermove", move);
      handle.removeEventListener("pointerup", up);
      handle.removeEventListener("pointercancel", up);
      if (moved) onDone(); else onTap?.(target);
    };
    handle.addEventListener("pointermove", move);
    handle.addEventListener("pointerup", up);
    handle.addEventListener("pointercancel", up);
  });
}

const mainGrid = $("main");
const sizes = store.get("sp-sizes", {});
if (sizes.editor) mainGrid.style.setProperty("--editor-size", `${sizes.editor}px`);
if (sizes.drawer) document.documentElement.style.setProperty("--drawer-size", `${sizes.drawer}px`);
dragDivider($("panel-divider"), (m) => {
  const box = mainGrid.getBoundingClientRect();
  sizes.editor = Math.max(200, Math.min(box.width - 246, m.clientX - box.left));
  mainGrid.style.setProperty("--editor-size", `${sizes.editor}px`);
}, () => store.set("sp-sizes", sizes));
// dragged down to within 70px of the bottom, the panel hides; dragged back up, it shows
let belowDivider = Infinity;
dragDivider($("drawer-divider"), (m) => {
  const box = mainGrid.getBoundingClientRect();
  belowDivider = box.bottom - m.clientY;
  if (belowDivider > 70 && !panelNow()) setPanel(lastPanel || lastPane);   // dragged back up: what it had returns
  sizes.drawer = Math.max(120, Math.min(box.height - 90, belowDivider));
  document.documentElement.style.setProperty("--drawer-size", `${sizes.drawer}px`);
}, () => {
  if (belowDivider <= 70) setPanel("");   // dragged to the foot: away, as the chevron puts it
  store.set("sp-sizes", sizes);
  quickstart?.render();
}, (target) => { if ($("divider-grip").contains(target)) toggleBottom(); else if ($("divider-full").contains(target)) growBottom(); });
// The help pane's − and +, as native's ZoomBar: the open pane's text a step
// smaller or larger, 1.1× a step from −4 to +8 (dpi.h FontZoomFactor), each
// panel one zoom for all its tabs, remembered.
const ZOOM_MIN = -4, ZOOM_MAX = 8;
const zooms = { help: -2, site: 1, ...store.get("sp-zooms", {}) };   // help starts two steps out: its cards and docs beside the code at a glance; the site's pages a step up, to read on a wide screen (a phone has them at their own size) (index.html's head applies these before the first paint: keep the two in step)
zooms.help ??= zooms.docs ?? -1;
const zoomFactor = (step) => Math.min(3, Math.max(0.5, 1.1 ** step));
const applyZoom = (pane) => document.documentElement.style.setProperty(`--${pane}-zoom`, String(zoomFactor(zooms[pane] ?? 0)));
function zoomPane(delta, pane = document.body.dataset.drawer) {
  if (["docs", "quickstart", "logs", "insight", "debug"].includes(pane)) pane = "help";   // the help panel's tabs zoom as one
  if (!["help", "site"].includes(pane)) return;
  zooms[pane] = Math.max(ZOOM_MIN, Math.min(ZOOM_MAX, (zooms[pane] ?? 0) + delta));
  store.set("sp-zooms", zooms);
  applyZoom(pane);
  if (pane === "help") requestAnimationFrame(() => { quickstart?.render(); window.dispatchEvent(new Event("resize")); });   // the cards' pages, and the threads' canvases, size themselves to the pane
}
applyZoom("help");
applyZoom("site");
$("site-smaller").addEventListener("click", () => zoomPane(-1, "site"));
$("site-bigger").addEventListener("click", () => zoomPane(1, "site"));
// the help panel's − and +: every one of its tabs a step together, each keeping its own size against the others (the
// cards start smaller than the docs), so what the panel shows grows or shrinks as one whatever tab is up
const zoomHelp = (delta) => zoomPane(delta, "help");
$("rail-smaller").addEventListener("click", () => zoomHelp(-1));
$("rail-bigger").addEventListener("click", () => zoomHelp(1));

// ── Scope ─────────────────────────────────────────────────────────────────

// each view: its id, its name, native's glyph for it, and what it shows (native's descriptions where it has one)
const SCOPE_MODES = [["bars", "Bars", "scope-levels", "A level meter per channel, then each channel's peaks as bars"], ["wave", "Wave", "scope-mono", "The left and right channels combined into one wave"],
  ["stereo", "Stereo", "scope-stereo", "Two independent scopes for the left and right channels"], ["lissajous", "Lissajous", "scope-lissajous", "Illustrates the phase relationship between the left and right channels"],
  ["line", "Line", "scope-line", "Everything playing as one thin line across the top bar, the log taking the scope's room"]];   // the bar's scope (navScope below): a kind as the others are, off when the scopes are
// the Link metronome and time warp strip, shown unless turned off, and the caret's line and position, hidden until asked for (preferences)
document.body.classList.toggle("no-metro", !store.get("sp-metro-shown", true));
document.body.classList.toggle("no-caret-pos", !store.get("sp-caret-pos-shown", false));
const paintScopeShown = () => $("btn-scope").classList.toggle("on", !document.body.classList.contains("no-scope"));
document.body.classList.toggle("no-scope", !store.get("sp-scope-shown", true));
paintScopeShown();
$("btn-scope").addEventListener("click", () => {
  document.body.classList.toggle("no-scope");
  store.set("sp-scope-shown", !document.body.classList.contains("no-scope"));
  paintScopeShown();
  status(document.body.classList.contains("no-scope") ? "Hiding audio oscilloscopes..." : "Showing audio oscilloscopes...");
});
const saveScope = () => store.set("sp-scope", { mode: scope.mode, lineWidth: scope.lineWidth, glow: scope.glow, trail: scope.trail });
const paintScopeMode = () => {
  $("btn-scope-mode").textContent = SCOPE_MODES.find(([m]) => m === scope.mode)?.[1] ?? "Bars";
  document.body.classList.toggle("scope-line", scope.mode === "line");   // the pane down to its head, the bar's line up (style.css)
};
$("btn-scope-mode").addEventListener("click", () => {
  const i = SCOPE_MODES.findIndex(([m]) => m === scope.mode);
  scope.mode = SCOPE_MODES[(i + 1) % SCOPE_MODES.length][0];
  paintScopeMode();
  saveScope();
  buildPrefs();
});
paintScopeMode();
// the scope's size, a tap or Enter or Space: full, or half height
const toggleScopeSize = () => scopeBox.setAttribute("aria-pressed", String(scopeBox.classList.toggle("half")));
scopeBox.addEventListener("click", toggleScopeSize);
scopeBox.addEventListener("keydown", (e) => { if (e.key === "Enter" || e.key === " ") { e.preventDefault(); toggleScopeSize(); } });
theme.onChange(() => { if (!scope.running) scope.drawSilent(); });

// ── The Link strip: the tempo, tap tempo and the global time warp ─────────
// As native's SonicPiMetro, without the network: joining Ableton Link is
// native-only, but Link's timeline is the runtime's and SuperSonic's clock's,
// so setting the tempo, tapping it and warping time all work. Before the
// engine boots the values wait here, and the session takes them when it is made.
const linkState = { bpm: 60, warp: 0 };
const bpmField = $("lk-bpm"), warpField = $("lk-warp"), warpSlider = $("lk-warp-slider");
const cue = (field) => { field.classList.add("lk-cue"); setTimeout(() => field.classList.remove("lk-cue"), 250); };
// a field shows its value unless something typed there is not yet set: a tap,
// the wheel or a program changes it while it only has the focus
const paint = (field, text) => {
  if (field.dataset.edited) return;
  field.value = text;
  if (document.activeElement === field) field.select();
};
const paintBpm = () => paint(bpmField, `${+linkState.bpm.toFixed(2)} bpm`);
const paintWarp = () => {
  paint(warpField, `${linkState.warp > 0 ? "+" : ""}${linkState.warp} ms`);
  warpSlider.value = String(Math.max(-250, Math.min(999, linkState.warp)));   // native's slider's range (sonicpimetro.cpp); the field takes any
};
// fromProgram: the tempo a program set (set_link_bpm!), which the runtime already has
function setLinkBpm(bpm, fromProgram = false) {
  if (!Number.isFinite(bpm)) return paintBpm();
  const next = Math.min(999, Math.max(20, bpm));                     // native's BPMScrubWidget range
  if (next === linkState.bpm) return paintBpm();
  linkState.bpm = next;
  paintBpm();
  cue(bpmField);
  if (fromProgram) session?.followLinkBpm(next); else session?.setLinkBpm(next);
}
function setTimeWarp(ms) {
  if (!Number.isFinite(ms)) return paintWarp();
  linkState.warp = Math.round(ms);
  paintWarp();
  session?.setTimeWarp(linkState.warp);
}

// A field that scrubs as native's do: drag up or down a step every 2px, the
// wheel and the arrow keys a step, Return sets what was typed, Escape puts it
// back, a double-click resets it. A tap or click without a drag edits the text.
function scrubField(field, { get, set, reset, parse, repaint }) {
  field.addEventListener("pointerdown", (e) => {
    if (document.activeElement === field) return;
    const y0 = e.clientY, v0 = get();
    let dragged = false;
    const move = (m) => {
      if (!dragged && Math.abs(m.clientY - y0) < 3) return;
      if (!dragged) { dragged = true; field.setPointerCapture(e.pointerId); }
      set(v0 + Math.trunc((y0 - m.clientY) / 2));
    };
    const up = () => {
      field.removeEventListener("pointermove", move);
      field.removeEventListener("pointerup", up);
      field.removeEventListener("pointercancel", up);
    };
    field.addEventListener("pointermove", move);
    field.addEventListener("pointerup", up);
    field.addEventListener("pointercancel", up);
    e.preventDefault();                               // no focus yet: a drag is not an edit
    field.addEventListener("pointerup", () => { if (!dragged) { field.focus(); field.select(); } }, { once: true });
  });
  field.addEventListener("wheel", (e) => { e.preventDefault(); set(get() + (e.deltaY < 0 ? 1 : -1)); }, { passive: false });
  field.addEventListener("keydown", (e) => {
    if (e.key === "ArrowUp" || e.key === "ArrowDown") { e.preventDefault(); set(get() + (e.key === "ArrowUp" ? 1 : -1)); }
    else if (e.key === "Enter") { e.preventDefault(); set(parse(field.value)); field.blur(); }
    else if (e.key === "Escape") { delete field.dataset.edited; repaint(); field.blur(); }
  });
  field.addEventListener("input", () => { field.dataset.edited = "1"; });
  field.addEventListener("blur", () => { delete field.dataset.edited; repaint(); });
  field.addEventListener("change", () => set(parse(field.value)));
  field.addEventListener("dblclick", (e) => { e.preventDefault(); set(reset); field.blur(); });
}
const number = (text) => parseFloat(String(text).replace(/[^0-9.+-]/g, ""));
scrubField(bpmField, { get: () => Math.round(linkState.bpm), set: (v) => setLinkBpm(v), reset: 60, parse: number, repaint: paintBpm });
scrubField(warpField, { get: () => linkState.warp, set: (v) => setTimeWarp(v), reset: 0, parse: number, repaint: paintWarp });
warpSlider.addEventListener("input", () => setTimeWarp(Number(warpSlider.value)));

// Tap tempo, as native's SonicPiMetro::tapTempo: once three taps are evenly
// spaced (within 30 ms, then 50 ms) the tempo is their average; a tap out of
// step starts again. On the press, not the release, so the timing is the tap's.
let taps = 0, firstTap = 0, lastTap = 0;
function tapTempo() {
  const now = performance.now();
  const tap = $("lk-tap");
  tap.classList.add("lk-flash");
  setTimeout(() => tap.classList.remove("lk-flash"), 120);
  taps++;
  if (taps === 1) firstTap = now;
  else {
    const since = now - lastTap, avg = (now - firstTap) / (taps - 1);
    if ((taps < 3 && (since > avg + 30 || since < avg - 30)) || since > avg + 50 || since < avg - 50) {
      taps = 1;
      firstTap = now;
    } else if (taps > 2) {
      const bpm = Math.round(60 / (avg / 1000));
      if (bpm !== linkState.bpm) { setLinkBpm(bpm); toast(`tap tempo: ${bpm} bpm`); }
    }
  }
  lastTap = now;
}
$("lk-tap").addEventListener("pointerdown", (e) => { e.preventDefault(); tapTempo(); });
$("lk-tap").addEventListener("keydown", (e) => { if (e.key === " " || e.key === "Enter") { e.preventDefault(); tapTempo(); } });
// native's TapTempo shortcut (Shift+Return in every keymap) taps wherever the focus is: the keys, at the end

// ── Full screen ───────────────────────────────────────────────────────────
// The Fullscreen API where a page has one. An iPad's Safari does not take a
// page full screen, but an app added to the Home Screen opens full screen, so
// there the button says how; installed, or on an iPhone, the button goes.
const isIPad = /iPad/.test(navigator.userAgent) || (navigator.platform === "MacIntel" && navigator.maxTouchPoints > 1);
const isIPhone = /iPhone/.test(navigator.userAgent);
const standalone = navigator.standalone === true || window.matchMedia("(display-mode: standalone)").matches;
document.body.classList.toggle("standalone", standalone);
if (standalone || isIPhone) $("btn-zen").hidden = true;
const fullscreenElement = () => document.fullscreenElement || document.webkitFullscreenElement;
$("btn-zen").addEventListener("click", () => {
  if (isIPad && !standalone) { editor.aside(); showDialog("install-overlay", true); return; }
  if (fullscreenElement()) (document.exitFullscreen || document.webkitExitFullscreen).call(document);
  else {
    const root = document.documentElement;
    Promise.resolve((root.requestFullscreen || root.webkitRequestFullscreen)?.call(root)).catch((e) => console.warn("fullscreen:", e));
  }
});
let quietFullScreenChange = false;   // focus mode says its own piece (native's quietFullScreenChange)
const paintFullscreen = () => {
  const on = !!fullscreenElement();
  $("btn-zen").classList.toggle("on", on);
  if (!quietFullScreenChange) status(on ? "Full screen mode on." : "Full screen mode off.");
};
document.addEventListener("fullscreenchange", paintFullscreen);
document.addEventListener("webkitfullscreenchange", paintFullscreen);
$("install-close").addEventListener("click", () => showDialog("install-overlay", false));


// Load, as native's, and its Load Set too: a buffer's code into a free buffer, a set (.sonicpi) as a set of its own
// on top (arrive, below). Nothing that is there is written over.
$("btn-load").addEventListener("click", () => $("load-file").click());
$("load-file").addEventListener("change", async () => {
  const file = $("load-file").files[0];
  if (!file) return;
  $("load-file").value = "";
  const base = file.name.replace(/\.(rb|txt|sonicpi)$/i, "");
  arrive(await file.text(), base, file.name, base);
});
$("install-overlay").addEventListener("click", (e) => { if (e.target === $("install-overlay")) showDialog("install-overlay", false); });

// ── The on-screen keyboard ────────────────────────────────────────────────
// On an iPad or a phone the keyboard covers the page without resizing it:
// Safari's layout viewport stays put and only the visual viewport shrinks.
// The app takes the visible height, and while the keyboard is up for the
// editor the panel below it hides, so the code being typed is what shows. For
// a card's code in the panel it is the other way about: the buffer gives way,
// the panel has the room, and the card is scrolled into it (editing-card).
// It holds while the keyboard is up: a tap to place the caret can move focus
// for a moment (to the card, a button of it), and the panel must not go then.
// Another editor's focus ends it, and so does the keyboard going away.
let editedCard = null;   // the panel's card whose code has focus
const setEditedCard = (card) => { editedCard = card; document.body.classList.toggle("editing-card", !!card); };
document.addEventListener("focusin", (e) => {
  const el = e.composedPath()[0];
  if (!(el instanceof Element) || !el.closest(".cm-editor")) return;
  setEditedCard(el.closest("#drawer .qs-card"));
});
const keyboardUp = () => document.body.classList.contains("keyboard-open") || document.body.classList.contains("kbd-open");
new MutationObserver(() => { if (editedCard && !keyboardUp() && !editedCard.contains(document.activeElement)) setEditedCard(null); })
  .observe(document.body, { attributes: true, attributeFilter: ["class"] });
const showEditedCard = () => requestAnimationFrame(() => editedCard?.scrollIntoView({ block: "nearest" }));
const vv = window.visualViewport;
if (vv) {
  const fitKeyboard = () => {
    const covered = window.innerHeight - vv.height;
    const typing = !!deepActive()?.closest?.(".cm-editor") || (!!editedCard && covered > 120);   // a caret tap's moment away from the card's code is still typing
    document.documentElement.style.setProperty("--app-height", `${Math.round(vv.height)}px`);
    document.body.classList.toggle("keyboard-open", covered > 120 && typing);
    if (vv.offsetTop > 0) window.scrollTo(0, 0);
    if (covered > 120 && editedCard) showEditedCard();
  };
  vv.addEventListener("resize", fitKeyboard);
  vv.addEventListener("scroll", fitKeyboard);
  document.addEventListener("focusin", fitKeyboard);
  document.addEventListener("focusout", () => setTimeout(fitKeyboard, 60));
  fitKeyboard();
}

// The code keyboard (keyboard.js): on a touch screen, in place of the
// system keyboard above, unless the player prefers the system one
const codeKeyboard = createCodeKeyboard({ editor, dock: $("input-dock"), mount: $("editor-mount"), store });

// ── Share: the buffer in the link (share.js) ──────────────────────────────

let toastTimer = null;
// said as it is shown (politely; assertive for a failure), or, with null, only shown: something else has said it
function toast(text, assertive = false, ms = 2000) {
  if (assertive !== null) status(text, assertive);
  ms = Math.max(ms, 1200 + String(text).length * 55);   // time to read it: about 18 characters a second
  // in the status bar, as native's, for two seconds or its reading time; where there is none (a phone, focus mode) it floats
  if ($("statusbar").offsetParent !== null) {
    $("status-engine").textContent = text;
    $("status-engine").classList.remove("err");
    clearTimeout(messageTimer);
    messageTimer = setTimeout(() => { messageTimer = null; paintEngine(); }, ms);
    return;
  }
  const t = $("toast");
  t.textContent = text;
  t.style.setProperty("--toast-top", `${Math.round($("toolbar").getBoundingClientRect().bottom) + 8}px`);
  t.hidden = false;
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => (t.hidden = true), ms);
}

// ── Code arriving (workspace.js) ────────────────────────────────────────
// From a link, a file or a card, it never goes over code that is there: a buffer's code takes a free buffer (or a set
// of its own when every buffer has code), a set goes on top as a set of its own. Each says where it went, and what
// it left alone.
function arrive(text, name, from, bufferName = "") {
  if (isSet(text)) {
    const r = workspace.openSet(text, { fallbackName: name || "Shared Set" });
    if (!r.ok) { toast(`${from} could not be opened: ${r.error}`, true, 8000); logs.add("Host", `${from} could not be opened: ${r.error}`); return false; }
    toast(`opened the set "${r.name}"${r.description ? `: ${r.description.length > 90 ? r.description.slice(0, 89) + "…" : r.description}` : ""} · your other sets are under Sets`, false, r.description ? 6000 : 4000);
    return true;
  }
  const was = workspace.active, r = workspace.openProgram(text, { name: name || "Shared Code", bufferName });
  if (r.set) toast(`opened in a new set, "${r.set}": every buffer had code · your other sets are under Sets`, false, 5000);
  else toast(r.buffer === was ? `opened in buffer ${r.buffer}` : `opened in buffer ${r.buffer} · buffer ${was} is as you left it`, false, 4000);
  return true;
}

// Share or save (share-menu.js): the buffer showing or the whole set, as a link, a QR code or a file
const linkTo = (code) => new URL(`${infoApi?.code.file ?? "code.html"}#code=${code}`, location.href).href;
createShareMenu({
  button: $("btn-share"), menu: $("link-menu"), clipboard, ready: loadShareCodec,
  scopes: {
    buffer: {
      icon: "file-code", fileKind: ".txt",
      kind: "This buffer", get title() { return `buffer ${workspace.active}`; },
      get summary() { const n = editor.getCode().replace(/\n$/, "").split("\n").length; return `${n} ${n === 1 ? "line" : "lines"}`; },   // its name is in the panel
      get preview() { const lines = editor.getCode().replace(/\s+$/, "").split("\n"), at = lines.findIndex((l) => l.trim() && !l.trim().startsWith("#")); return (at < 0 ? lines : lines.slice(at)).join("\n"); },   // from its first line of code
      link: () => { history.replaceState(null, "", `${infoApi?.code.file ?? "code.html"}#code=${encodeCode(editor.getCode())}`); return location.href; },
      qrLink: () => linkTo(encodeDigits(editor.getCode())),
      about: "Save the current buffer as a single plain text file.",
      save: () => saveBuffer({ quiet: true }),
      // its name: the file's, saved as it is typed
      fields: {
        get name() { return workspace.bufferName(workspace.active); }, nameMax: NAME_MAX,
        set: (name) => workspace.setBufferName(workspace.active, name),
      },
    },
    set: {
      icon: "stack-2", fileKind: ".sonicpi",
      kind: "Whole set", get title() { return `the set "${workspace.set().name}"`; },
      get summary() { return `${workspace.size} buffers`; },
      link: () => linkTo(encodeCode(workspace.fileText())),
      qrLink: () => linkTo(encodeDigits(workspace.fileText())),
      about: "Save all buffers together as one set with an optional name and description.",
      save: () => saveSet({ quiet: true }),
      // its name and description, given here where it is shared, saved as they are typed (its link, file and QR code say them)
      fields: {
        get name() { return workspace.set().name; }, get description() { return workspace.set().description; },
        nameMax: NAME_MAX, descriptionMax: DESCRIPTION_MAX,
        set: (name, description) => workspace.editSet(workspace.set().id, { name, description }),
      },
    },
  },
});

// the sets' chip, in the tabs' slot, and the list it opens (sets-view.js)
createSetsView({ slot: tabs.slot, menu: $("sets-menu"), workspace, toast });

// ── Info: sonic-pi.net as the app's λ card (info.js) ─────────────────────
// Every page of the site is this document with its page in the card
// (scripts/build-site.mjs): index.html is Home, learn.html Learn and so on,
// so a visitor lands on sonic-pi.net with the editor a click away and the
// engine shared. λ opens the card from anywhere, × or the Code tab closes it onto
// the editor, a card's Open closes it with the card's code (its run carrying
// on, now the buffer's). A section is the page's #anchor; code.html is the
// editor and code.html#code=… the editor with that code. Closing puts code.html
// in the history, so Back is the card again. Another page of the site is another document.
// The website's bar (#site-nav) keeps the site's nav across the top of the
// app — a way back to the web parts until you are happy in the app and close
// it, when it folds to a slim strip with its wordmark at the top left; that,
// or λ, opens the bar and the pages again. The pages fill the window under it.
const infoCard = $("info-card"), siteNav = $("site-nav"), brand = siteNav.querySelector(".sn-brand");
// The bar's words, between its icons (the mark at its start, the tablet and the palette at its end): spread evenly
// across the room between them, but no further apart than SPREAD_MAX; past that the words keep that spacing and sit
// centred between the icons, the rest of the room either side. Where the row takes only its words' room (the editor's,
// its scope after them) there is nothing to share, and the stylesheet's own gap spaces them.
const SPREAD_MAX = 48;
{
  const row = siteNav.querySelector(".ic-tabs");
  const phoneWidth = matchMedia("(max-width: 760px)");
  phoneWidth.addEventListener("change", () => spread());
  const spread = () => {
    const words = [...row.querySelectorAll(".ic-tab:not(.ic-tab-home)")];
    for (const w of words) w.style.marginLeft = w.style.marginRight = "";
    row.classList.remove("spread");
    if (!words.length || !phoneWidth.matches || getComputedStyle(row).flexGrow === "0" || !row.clientWidth) return;   // a wide screen: the words after the wordmark, left-aligned
    row.classList.add("spread");   // its own gap off: the margins below are the spacing
    const used = [...row.children].reduce((n, c) => n + c.getBoundingClientRect().width, 0);
    const free = Math.max(0, row.clientWidth - used);
    const slots = words.length + 1;   // before the first word, between each two, after the last
    // the room past the row before the tablet (its own margin) counts on the right, so the two ends look alike
    const past = parseFloat(getComputedStyle(siteNav.querySelector(".sn-code") ?? row).marginLeft) || 0;
    const gap = Math.min((free + past) / slots, SPREAD_MAX), left = (free + past - gap * (words.length - 1)) / 2;
    words.forEach((w, i) => { w.style.marginLeft = `${i === 0 ? left : gap}px`; });
    words.at(-1).style.marginRight = `${Math.max(0, left - past)}px`;
  };
  if (row) { new ResizeObserver(() => requestAnimationFrame(spread)).observe(row); document.fonts?.ready.then(spread); }
}
let info = null, infoApi = null;   // the promise of the page made live, and what it resolved to
function showSiteNav(on) {
  siteNav.classList.toggle("folded", !on);
  if (on) ensureInfo();
  store.set("sp-site-nav", on ? null : "closed");
  if (infoOpen()) placeInfo();
}
siteNav.querySelector(".sn-close").addEventListener("click", () => { showSiteNav(false); closeInfo(); });
brand.addEventListener("click", (e) => { showSiteNav(true); if (infoApi?.page === "about") { e.preventDefault(); pickTab("about"); } });   // Home: this page's top, or the page itself
// A phone's page bar (ui/pagebar.js), under the tabs: where you are in the page showing — its group and section —
// ‹ › to the section either side, a tap in the middle for the page's whole list, and the line through the page.
// The page says what is lit and whether its list is open (info.js tellFold); the examples have their own (ui/flip.js).
const siteStrip = $("site-strip");
// the page's own bar when its build wrote one (scripts/build-site.mjs), there from the first paint, taken over
const stripBar = createPageBar({ adopt: siteStrip.querySelector(".pb"), onPrev: () => infoApi?.stepContents(-1), onNext: () => infoApi?.stepContents(1), onJump: () => (phoneMedia.matches ? infoApi?.toggleContents() : infoApi?.revealContents()), onGroup: (i) => infoApi?.contentsGroup(i) });
stripBar.el.classList.add("pb-eased");
if (!stripBar.el.parentNode) siteStrip.append(stripBar.el);
function paintContentsChip(fold) {
  document.body.classList.toggle("contents-open", !!fold?.open);   // the page holds still and a tap on it only closes the list (below)
  siteStrip.classList.toggle("on", !!fold && !fold.bare);   // bare: the page names where it is itself (a phone's examples, the flip's bar)
  if (!fold) return;
  document.documentElement.style.setProperty("--strip-left", `${phoneMedia.matches ? 0 : fold.sideRight ?? 0}px`);
  stripBar.groups(fold.groups ?? [], (k) => infoApi?.contentsItem(k));
  stripBar.set({ eyebrow: fold.group ?? "", main: fold.label, prev: fold.at > 0, next: fold.at < (fold.count ?? 1) - 1, prevTitle: fold.prevTitle, nextTitle: fold.nextTitle, said: fold.label, open: fold.open });
  stripBar.fill(fold.at ?? 0);
}
// The list open: a tap on the page beneath closes it and goes no further — nothing under the finger is pressed
let swallowClickUntil = 0;   // the click that ends the tap which closed the list: swallowed too
for (const type of ["pointerdown", "click"]) infoCard.addEventListener(type, (e) => {
  if (type === "click" && performance.now() < swallowClickUntil) { swallowClickUntil = 0; e.preventDefault(); e.stopPropagation(); return; }
  if (!document.body.classList.contains("contents-open")) return;
  if (e.composedPath().some((n) => n.classList?.contains("ic-side"))) return;   // a tap in the list is the list's
  e.preventDefault(); e.stopPropagation();
  if (type === "pointerdown") { swallowClickUntil = performance.now() + 800; infoApi?.toggleContents(); }
}, true);
// A tab, or the page holding an id: the card at that page, there, the address its own
function pickTab(tab, id = null) { showInfo(tab, id); }
function ensureInfo() {
  return info ??= Promise.resolve().then(() => createInfo(infoCard, {
    tabs: siteNav.querySelector(".ic-tabs"),
    pick: pickTab,
    log: (text) => logs.add("Host", text),   // a page's first visit, timed (info.js)
    play: (code, opts) => play(code, opts),
    stopGroup: (group, fade) => session?.stopGroup(group, fade),
  group: nextCardGroup,
    scopeFrame,
    loopScopes: () => loopScopePrefs,   // the site's cards' loop scopes, as the preferences say
    now: () => session?.clockNow() ?? null,
    synthDefaults: (synth) => synthOpts.get(synth.replace(/^sonic-pi-/, "")) ?? null,
    open: openFromInfo,
    docs: (section, key) => { if (section === "examples") return showInfo(section); if (section === "tutorial") return showTutorial(key); closeInfo(); showDocs(section, key); },   // the examples and the tutorial are pages of the site
    leave: () => { warmEngine(); closeInfo(); },   // Launch Sonic Pi: into the editor with the engine starting, in the tap
    // a live synth on a page (the home page's): the docs pane's instrument, its Play a card of a deck of its own, its
    // QWERTY keys the instrument's while focus is in it
    instrument: (host, key, scroller) => {
      // the deck at once (the page keeps it); the instrument once the synth's page has arrived (the editor's data)
      const deck = createDeck({ play: (code, opts) => play(code, opts), stopGroup: (group, fade) => session?.stopGroup(group, fade), group: nextCardGroup, scopeFrame }, scroller);
      // the synth's page of the reference: the page's own copy (scripts/build-site.mjs synth-page), else the reference's
      const own = host.querySelector("script.synth-page");
      const page = own ? Promise.resolve(JSON.parse(own.textContent)) : needReference().then(() => synths.pages.find((x) => x.key === key));
      page.then((p) => {
        if (!p) return;
        if (!synthOpts.has(p.key)) synthOpts.set(p.key, Object.fromEntries((p.opts || []).map((o) => [o.name, o.default])));
        const inst = createInstrument(p, false, paneHooks, deck, { fit: true, place: "home", heading: 2, open: openFromInfo });   // an h2, under the home page's section
        host.replaceChildren(inst.face);
        host.classList.add("live");
        host.addEventListener("keydown", inst.keyHandler);
      });
      return deck;
    },
    onTheme: (fn) => theme.onChange(fn),
    contents: paintContentsChip,   // a phone: the page's contents fold from the bar's chip
    // the section being read, as the address's anchor: a link copied from the bar is a link to it
    shown: (tab, id) => {
      if (!infoOpen()) return;
      const want = (infoApi?.fileOf(tab) ?? "") + (id ? `#${id}` : "");
      const u = new URL(want, location.href);
      if (u.pathname !== location.pathname || u.hash !== location.hash) history.replaceState(null, "", want);
    },
  })).then((api) => {
    infoApi = api;
    if (!infoOpen()) api.blur();
    else {   // once the page has loaded, while it is idle: the other tabs ready for a first tap
      const soon = () => (window.requestIdleCallback ?? ((f) => setTimeout(f, 200)))(() => api.prefetch(), { timeout: 1000 });
      if (document.readyState === "complete") soon(); else addEventListener("load", soon, { once: true });
    }
    return api;
  }).catch((e) => { logs.add("Host", `the site did not load: ${describe(e)}`); return null; });
}
const infoOpen = () => !infoCard.hidden;
// the card sits under the website's bar (or the top of the window), the whole app under it — toolbar, icons and all — blurred behind
// under the bar; the contents strip (a phone's) floats over the page's top as frosted glass, so the page starts under
// it, padded by its height, and the list it opens drops from its foot (style.css, site.css: --strip-h, --list-top)
const placeInfo = () => {
  const navBottom = Math.round(siteNav.getBoundingClientRect().bottom);
  const stripH = siteStrip && getComputedStyle(siteStrip).display !== "none" ? siteStrip.getBoundingClientRect().height : 0;   // exact: the CSS's own reckoning before this (style.css) is not rounded either
  infoCard.style.setProperty("--ic-top", `${navBottom}px`);
  const root = document.documentElement.style;
  root.setProperty("--nav-bottom", `${navBottom}px`);
  root.setProperty("--strip-h", `${stripH}px`);
  root.setProperty("--list-top", `${navBottom + stripH}px`);
  if (stripH !== lastStripH) { lastStripH = stripH; requestAnimationFrame(() => $("info-body").dispatchEvent(new Event("scroll"))); }   // the page's place in its contents, looked at again from under the strip
};
let lastStripH = 0;
window.addEventListener("resize", () => { if (infoOpen()) placeInfo(); });
{ const ro = new ResizeObserver(() => { if (infoOpen()) placeInfo(); }); ro.observe(siteNav); ro.observe($("site-strip")); }   // the bar grows a row on a phone once its tabs are in
setInterval(() => { if (infoOpen()) placeInfo(); }, 500);                        // and, belt and braces, the pages check where the bar ends
/** The card at a page (the one on show, if none) or at the page holding an id; history: how the address follows
 *  ("push" a step Back undoes, "replace" in place of an old link, "none" when the address is where Back went). */
// The tutorial, a page a chapter on the site (scripts/build-site.mjs): a part of it by its key (02.1-Your-First-Beeps,
// the form old links use), on its chapter's page, or its first page
const tutorialId = (key) => `tut-${key.toLowerCase().replace(/[^a-z0-9]+/g, "-")}`;   // build-site.mjs tutId
function showTutorial(key = null, opts = {}) { return showInfo("tutorial", key ? tutorialId(key) : null, opts); }
async function showInfo(tab = null, id = null, { history: how = "push" } = {}) {
  await ensureInfo();
  const at = (id && infoApi?.pageOf(id)) ?? tab ?? infoApi?.page;
  if (siteNav.classList.contains("folded")) showSiteNav(true);
  placeInfo();
  infoCard.hidden = false;
  document.body.classList.add("info-open");
  editorReachable(false);
  editor.aside();   // its completion would float over the page
  try { await infoApi?.go(at, id, { history: how }); }
  catch (e) { logs.add("Host", `the page did not load: ${describe(e)}`); const file = infoApi?.fileOf(at); if (file) location.assign(file + (id ? `#${id}` : "")); return; }   // the page itself, as a link would
  infoApi?.focus();
}
function closeInfo({ push = true } = {}) {
  if (!infoOpen()) return;
  infoCard.hidden = true;
  document.body.classList.remove("info-open");
  editorReachable(true);
  helpHint?.wake?.();   // the first visit's pointer at Help, resting while the pages covered it
  wakeEditor();
  paintContentsChip(null);   // the page's fold chip goes with the page
  infoApi?.blur();
  editor.focus();
  // the editor's own address, code.html: a step Back is the page again
  const code = infoApi?.code;
  if (code) { document.title = code.title; if (push && !onCode()) history.pushState(null, "", code.file); }
}
// from a card: its code (its run carrying on, now the buffer's), and nothing beside the editor
function openFromInfo(code, job) {
  closeInfo();
  arrive(code, "Examples", "the card");
  openDrawer("");
  if (job != null) { jobBuffer.set(job, editor.active); for (const d of cardDecks()) d.release(job); paintLoopScopes(); }
  else editorFillsPhone();
}
// On a phone, code arriving with nothing running would sit over an empty strip of log and cues:
// the editor takes the screen instead, and the divider's chevron brings the strip back.
function editorFillsPhone() { if (phoneMedia.matches) setPanel(""); }
// The strip on a phone: the log or the cues, one at a time behind the chips; clear clears the one showing
for (const b of $("strip-head").querySelectorAll("[data-strip]")) b.addEventListener("click", () => {
  document.body.classList.toggle("strip-cues", b.dataset.strip === "cues");
  for (const o of $("strip-head").querySelectorAll("[data-strip]")) { o.classList.toggle("active", o === b); o.setAttribute("aria-selected", String(o === b)); }
});
$("strip-clear").addEventListener("click", () => $(document.body.classList.contains("strip-cues") ? "cue-clear" : "log-clear").click());
// The bar's scope: everything playing, as one of the editor's live loop scopes (loopscope.js), across
// the room the wordmark leaves. A tap on the engine's output feeds it; it draws only while it shows.
const navScope = { canvas: siteNav.querySelector(".sn-scope"), state: new LoopScopeState(), analyser: null, buf: null, cursor: 0 };
// The bar's stop wears the card's rings (ui/card.js cardRings), the mix's left channel outside and right inside,
// read off a stereo pair of analysers on the engine's output while anything sounds.
const stopRings = (() => {
  const box = document.querySelector("#site-nav .sn-stop"), canvas = box.querySelector(".sn-stop-rings");
  const rings = cardRings([{ box, canvas, channels: [0, 1] }], { gain: 3, swing: 0.1 });   // small, so it bends further: alive at a glance
  let taps = null, live = false;
  const read = () => {
    if (!taps) return null;
    taps.l.getFloatTimeDomainData(taps.bufL); taps.r.getFloatTimeDomainData(taps.bufR);
    const frames = taps.bufL.length;
    for (let i = 0; i < frames; i++) { taps.out[i * 2] = taps.bufL[i]; taps.out[i * 2 + 1] = taps.bufR[i]; }
    return { frames, channels: 2, interleaved: taps.out };
  };
  return {
    get live() { return live; },
    attach(ac, node) {
      const split = ac.createChannelSplitter(2);
      node.connect(split);
      const tap = (ch) => { const a = Object.assign(ac.createAnalyser(), { fftSize: 1024, smoothingTimeConstant: 0 }); split.connect(a, ch); return a; };
      taps = { l: tap(0), r: tap(1), bufL: new Float32Array(1024), bufR: new Float32Array(1024), out: new Float32Array(2048) };
    },
    set(on) { live = on; if (on) rings.play(read); else rings.stop(); },
    repaint: () => rings.repaint(),
  };
})();
loadingStop = createLoadingStop({
  box: document.querySelector("#site-nav .sn-stop"), canvas: document.querySelector("#site-nav .sn-stop-rings"),
  playing: () => stopRings.live,
  // the load over: the rings take the canvas if anything sounds, else the stop is drawn at rest again
  after: () => { const on = document.body.classList.contains("sounding"); if (on !== stopRings.live) stopRings.set(on); else if (!on) stopRings.repaint(); },
});
function attachNavScope(engine) {
  const ac = engine.audioContext ?? engine.node?.context;
  if (!ac || !engine.node || navScope.analyser?.context === ac) return;   // tapped already, unless a reload made the context new
  const first = !navScope.analyser;
  navScope.analyser = Object.assign(ac.createAnalyser(), { fftSize: 4096, smoothingTimeConstant: 0 });
  navScope.buf = new Float32Array(navScope.analyser.fftSize);
  engine.node.connect(navScope.analyser);
  stopRings.attach(ac, engine.node);
  if (!first) return;   // the draw loop reads navScope.analyser as it stands: one loop, whatever context
  // drawn while the bar's scope is on screen (ui/shown.js), and resting once it has drawn half a second of silence: a
  // sound starting wakes it (onRecord)
  let quiet = 0;
  navScope.loop = animateWhileShown(navScope.canvas, () => {
    const c = navScope.canvas, buf = navScope.buf;
    navScope.analyser.getFloatTimeDomainData(buf);
    const frame = { frames: buf.length, channels: 1, interleaved: buf, writePosition: ++navScope.cursor };
    if (navScope.state.feed(frame, false) || !c.dataset.painted) { drawLoopScope(c, navScope.state); c.dataset.painted ||= "1"; }   // said once: a write is a change to the page, even of the same value
    let loud = false;
    for (let i = 0; i < buf.length; i += 8) if (Math.abs(buf[i]) > 1e-4) { loud = true; break; }
    quiet = loud ? 0 : quiet + 1;
    if (quiet > 30) navScope.loop.rest();
  });
  navScope.wake = () => { quiet = 0; navScope.loop.wake(); };   // a sound: half a second more before it rests again
}
phoneMedia.addEventListener("change", () => { if (!phoneMedia.matches && document.body.dataset.drawer === "output") openDrawer(""); });
// Info, as native's λ: a dialog about this Sonic Pi. The site is the bar's (sonic-pi.net, or the dialog's link).
let aboutFrom = null;   // what had focus when the dialog opened: it gets it back on closing
// A dialog up or down (about, install): <body> says whether one is (dialog-open), and so the editor's popups, in its
// shadow root, stay under it (style.css)
function showDialog(id, on) {
  $(id).hidden = !on;
  document.body.classList.toggle("dialog-open", !$("about-overlay").hidden || !$("install-overlay").hidden);
}
function showAbout(on) {
  const was = !$("about-overlay").hidden;
  showDialog("about-overlay", on);
  $("btn-info").classList.toggle("on", on);
  if (!on) {
    if (was) editorReachable(true);
    // back where it was; Safari does not focus a clicked button, so a click leaves nothing but the page: the editor then
    if (was) { if (aboutFrom?.isConnected && aboutFrom !== document.body) aboutFrom.focus(); else editor.focus(); }
    aboutFrom = null;
    return;
  }
  if (!was) aboutFrom = deepActive();   // the editor's content, in its shadow root, not its mount
  editorReachable(false);   // a modal dialog: Tab and a screen reader's cursor stay in it (aria-modal, index.html)
  editor.aside();   // its completion would float over the dialog
  $("about-v-sound").textContent = `SuperSonic ${SUPERSONIC_VERSION}`;
  showInfoPage("about");
  $("about-close").focus();
}
// the dialog's pages: About is the page's own; the rest native's texts, fetched the first time they are asked for
const infoPages = { about: $("info-about") };
async function showInfoPage(key) {
  for (const b of $("about-overlay").querySelectorAll("[data-info]")) { const on = b.dataset.info === key; b.classList.toggle("active", on); b.setAttribute("aria-selected", String(on)); }
  const page = $("info-page");
  if (!infoPages[key]) {
    const box = el("section");
    try { box.innerHTML = await (await fetch(`info/${key}.html`)).text(); } catch (e) { box.textContent = `This page did not load: ${describe(e)}`; }
    infoPages[key] = box;
  }
  page.replaceChildren(infoPages[key]);
  page.scrollTop = 0;
}
for (const b of $("about-overlay").querySelectorAll("[data-info]")) b.addEventListener("click", () => showInfoPage(b.dataset.info));
// a link within the dialog: the site's pages by their hash (#windows the downloads, #support the ask), the history's own anchors in place
$("info-page").addEventListener("click", (e) => {
  const a = e.target.closest("a[href]"); if (!a) return;
  const href = a.getAttribute("href");
  if (!href.startsWith("#")) return;
  e.preventDefault();
  const here = $("info-page").querySelector(`[id="${CSS.escape(href.slice(1))}"]`);
  if (here) return here.scrollIntoView({ block: "start" });
  showAbout(false); showSiteNav(true);
  const to = href.slice(1);
  ensureInfo().then(() => (infoApi?.tabs.includes(to) ? showInfo(to) : showInfo(null, to)));   // a page by its name, or the page holding an anchor
});
$("btn-info").addEventListener("click", () => showAbout($("about-overlay").hidden));
$("about-close").addEventListener("click", () => showAbout(false));
$("about-overlay").addEventListener("click", (e) => { if (e.target === $("about-overlay")) showAbout(false); });
document.addEventListener("keydown", (e) => { if (e.key === "Escape" && !$("about-overlay").hidden) { e.stopPropagation(); showAbout(false); } }, true);
// what the card covers, out of reach of a keyboard and a screen reader while it does: the pages or the editor,
// never both (the build starts a page with the editor inert)
const EDITOR_PARTS = ["toolbar", "main", "statusbar"].map((id) => $(id));
const editorReachable = (on) => { for (const e of EDITOR_PARTS) e.inert = !on; };
document.addEventListener("keydown", (e) => { if (e.key === "Escape" && infoOpen() && !infoApi?.playing) { e.stopPropagation(); closeInfo(); } }, true);
// the URL: the editor, the editor with code, or the card at this page or an anchor on it. An anchor another page
// holds, or a page's name, is that page: the links the site had when its pages were one (#learn, #mac, #schools)
const fileNow = () => location.pathname.split("/").pop() || "index.html";
const onCode = () => fileNow() === (infoApi?.code.file ?? "code.html");
async function route() {
  const h = location.hash;
  await ensureInfo();
  // the docs at a page (code.html#docs/tutorial/02-Synths: the site's links into the tutorial), and #tutorial, the old
  // one-page site's: the editor with them beside it
  // (and an opt on a synth's or an FX's page, #docs/synths/pluck/opt-release: the home page's live synth's knobs link
  // there, where the opt's detail is)
  const d = /^#docs\/(\w+)(?:\/([\w.-]+))?(?:\/(opt-\w+))?$/.exec(h) ?? (h === "#tutorial" ? [h, "tutorial", null] : null);
  if (d?.[1] === "tutorial") return showTutorial(d[2] ?? null, { history: "replace" });   // the tutorial is pages of the site: the part asked for, on its chapter's
  if (d) {
    history.replaceState(null, "", infoApi?.code.file ?? "code.html");
    closeInfo({ push: false });
    showDocs(d[1], d[2] ?? null);
    if (d[3]) requestAnimationFrame(() => {
      const opt = document.getElementById(d[3]);
      if (!opt) return;
      opt.scrollIntoView({ block: "start" });
      if (!opt.hasAttribute("tabindex")) opt.tabIndex = -1;
      opt.focus({ preventScroll: true });
      opt.classList.add("tip-landed");   // where the link went, lit a moment (as the docs page's own tip links light it)
    });
    return;
  }
  // the editor: code.html, with a program (#code=…) or without; #app and a program on a page's address were its old links
  if (/^#code=/.test(h) || h === "#app") {
    if (!onCode()) history.replaceState(null, "", infoApi?.code.file ?? "code.html");   // the program itself is loadFromHash's to take out of the address
    closeInfo({ push: false });
    if (h !== "#app") openDrawer("");
    return;
  }
  const page = infoApi?.keyOf(fileNow()) ?? null;   // the page the address names
  const m = /^#([\w.-]+)$/.exec(h);
  // the editor's document stays the editor, unless the anchor is a page's or on one (a skip link's #main is its own)
  if (onCode() && !(m && (m[1] === "schools" || m[1] === "patreon" || infoApi?.tabs.includes(m[1]) || infoApi?.pageOf(m[1])))) return closeInfo({ push: false });
  if (!m) return showInfo(page, null, { history: "none" });
  const forward = { history: "replace" };   // an old link gives way to the page's own address: Back does not return to it
  if (m[1] === "schools") return showInfo("learn", "teachers", forward);   // Schools is a part of Learn now
  if (m[1] === "patreon") return showInfo("support", null, forward);    // and Patreon is Support
  if (infoApi?.tabs.includes(m[1])) return showInfo(m[1], null, forward);
  const holder = infoApi?.pageOf(m[1]);
  showInfo(holder ?? page, holder ? m[1] : null, holder && holder !== page ? forward : { history: "none" });
}
window.addEventListener("popstate", route);
showSiteNav(store.get("sp-site-nav") !== "closed");
route();
if (!store.get("sp-help-seen", false)) showHelpHint();   // once the page is laid out and knows whether it shows the editor

function loadFromHash() {
  const m = /^#code=([\w-]+)/.exec(location.hash);
  if (!m) return;
  // a link's code is the point: no pane beside it (the cards are for a fresh, empty opening)
  openDrawer("");
  editorFillsPhone();
  history.replaceState(null, "", location.pathname + location.search);
  // one that can't be read says so on the error card, as a program's error does, until it's put away: what happened
  // to it and what to do (friendly.js explainLink), not a line gone before it could be read
  loadShareCodec().then(() => arrive(decodeCode(m[1]), "", "the link")).catch((e) => {
    showError({ class: "LinkError", title: "Link Error", message: e?.message ?? String(e) });
    logs.add("Host", `the link's code could not be read: ${describe(e)}`);
  });
}
loadFromHash();
window.addEventListener("hashchange", loadFromHash);

// ── MIDI (Web MIDI): devices listed, input shown as cues ──────────────────

// MIDI goes through the engine, both ways.
//
// Web MIDI belongs to SuperSonic's host front (clockwork/js/lib/midi_manager.js), which drives the same Rust
// core the native app does. A message a controller sends is parsed there, put on the engine's ingress as
// "/clockwork/midi/in/<kind> <port> <channel> <values…>", and comes back off the egress to whoever subscribed
// — this page. A message a program sends goes round the same way: "/clockwork/midi/out/<kind>" into the
// engine, forwarded back to the front, out of the port at the time the bundle asked for. One path, one shape,
// the same on web and native, and the tooling for it is clockwork's rather than ours.
//
// NOTHING HERE SETS A CALLBACK ON THE MANAGER. onMessage, onPorts and onTempo are how the front feeds the
// ingress (host_front.js init), so setting one takes it away: onMessage is preferred over the OSC path, which
// means setting it stops MIDI reaching the engine at all.
let midi = null;                       // the manager, for what only it can answer
let midiIns = [], midiOuts = [];       // the ports, as the engine last pushed them
const MIDI_IN = "/clockwork/midi/in/", MIDI_PORTS = "/clockwork/midi/ports";
const midiPorts = () => ({ ins: midiIns, outs: midiOuts });

// "<nIn> [name enabled]* <nOut> [name enabled]*", the native subsystem's own wire form
function readPorts(args) {
  let i = 0;
  const take = () => { const n = args[i++] ?? 0; const out = []; for (let k = 0; k < n; k++) { out.push([args[i], !!args[i + 1]]); i += 2; } return out; };
  const ins = take(), outs = take();
  return { ins, outs };
}

// An event off the egress: a controller's message, a tempo from its clock, or the ports changing.
function midiInbound(m) {
  const address = m[0];
  if (typeof address !== "string") return;
  if (address === MIDI_PORTS || address === `${MIDI_PORTS}.reply`) {
    ({ ins: midiIns, outs: midiOuts } = readPorts(m.slice(1)));
    api.updateMidiOuts(midiOuts.map(([name]) => name));
    // a port that has appeared is opened, so a controller plugged in mid-session is heard
    buildPrefs();
    return;
  }
  // A controller's message never reaches this thread: the runtime's worker drains the egress and turns it
  // into a cue where it can act on it (live-worker.js startEgress), and the Cues pane fills from the record
  // that comes back like any other. What arrives here is what the worker did not keep.
  if (!address.startsWith(MIDI_IN)) return;
  if (address.slice(MIDI_IN.length) === "clock_bpm") logs.add("Host", `midi clock ${m[1]}: ${Number(m[2]).toFixed(1)} bpm`);
}

// MIDI on (the switch, or the last session's choice): the manager the first time, which is where the browser asks
// for permission; after that, the ports opened again. Off closes every port, so nothing comes in or goes out.
const midiPorts_ = (on) => { engineRef?.send(`${MIDI_IN}enable`, "*", on ? 1 : 0); engineRef?.send("/clockwork/midi/out/enable", "*", on ? 1 : 0); };
function disableMidi() {
  store.set("sp-midi", false);
  if (midi) midiPorts_(false);
  buildPrefs();
}
async function enableMidi() {
  if (midi) { store.set("sp-midi", true); midiPorts_(true); buildPrefs(); return midi; }
  if (!navigator.requestMIDIAccess) { toast("this browser has no Web MIDI"); return null; }
  if (!engineRef) await ensureSession().catch(() => {});      // MIDI hangs off the engine's front
  if (!engineRef) { toast("MIDI needs the engine"); return null; }
  try {
    // sysex is not asked for: it is what raises the browser's permission prompt, and nothing here reads or sends it
    midi = await engineRef.enableMidi({ requestAccess: () => navigator.requestMIDIAccess() });
    if (!midi) throw engineRef.midiError ?? new Error("no MIDI on this host");
    engineRef.on("in", midiInbound);
    // Subscribed, or the front drops every event before the page sees it (host_front.js _takeEvent). The
    // subscribe answers with the ports as they stand, which is where midiIns and midiOuts come from.
    engineRef.send("/clockwork/midi/notify/subscribe");
    // Ports are closed until opened, as they are natively. Sonic Pi listens to every input, and sends to every
    // output because a program may name any of them (midi_note_on port: "*").
    midiPorts_(true);
    store.set("sp-midi", true);
    buildPrefs();
  } catch (e) {
    midi = null;
    store.set("sp-midi", false);
    toast(`MIDI: ${e.message ?? e}`);
    buildPrefs();
  }
  return midi;
}

// ── Game controllers, as native's (gamepad_api.rb, and its Preferences: "Game Controllers") ───────────────
// The engine's front reads the Gamepad API (the engine is made with gamepad: true, sonic_pi.js); a pad's buttons
// and sticks become cues in the runtime's worker, which reads the engine's egress (live-worker.js padIn). This
// side keeps what native's preferences keep: incoming cues on or off, and the pads the player has muted, each
// asked of the engine (/clockwork/gamepad/enable) and asked again whenever a pad comes back, since the engine
// forgets a pad's setting when it leaves. Subscribing is what brings the pads' comings and goings here.
const gamepadPrefs = { cues: store.get("sp-gamepad-cues", true), off: new Set(store.get("sp-gamepads-off", [])) };
let gamepads = [];   // [name, enabled], as the engine last said
const PAD_DEVICES = "/clockwork/gamepad/devices";
function startGamepads() {
  if (!engineRef?.gamepad) return;   // no Gamepad API here, or its front did not come up (engineRef.gamepadError)
  engineRef.on("in", gamepadInbound);
  engineRef.send("/clockwork/gamepad/notify/subscribe");   // answered with the pads there now
  applyGamepads();
}
// every pad as the player wants it: all off with the cues, else each one but the muted
function applyGamepads() {
  if (!engineRef?.gamepad) return;
  engineRef.send("/clockwork/gamepad/enable", "*", gamepadPrefs.cues ? 1 : 0);
  if (gamepadPrefs.cues) for (const name of gamepadPrefs.off) engineRef.send("/clockwork/gamepad/enable", name, 0);
}
// "/clockwork/gamepad/devices[.reply] <n> [name enabled]*"
function gamepadInbound(m) {
  if (m?.[0] !== PAD_DEVICES && m?.[0] !== `${PAD_DEVICES}.reply`) return;
  const pairs = m.slice(2);
  gamepads = [];
  for (let i = 0; i + 1 < pairs.length; i += 2) gamepads.push([String(pairs[i]), !!pairs[i + 1]]);
  // a pad that is back comes back enabled: muted again, as the player left it (the engine rebroadcasts only on a change)
  for (const [name, on] of gamepads) if (on && (!gamepadPrefs.cues || gamepadPrefs.off.has(name))) engineRef.send("/clockwork/gamepad/enable", name, 0);
  buildPrefs();
}
function setGamepadCues(on) {
  gamepadPrefs.cues = on;
  store.set("sp-gamepad-cues", on);
  applyGamepads();
}
function setGamepadOn(name, on) {
  if (on) gamepadPrefs.off.delete(name); else gamepadPrefs.off.add(name);
  store.set("sp-gamepads-off", [...gamepadPrefs.off]);
  if (gamepadPrefs.cues) engineRef?.send("/clockwork/gamepad/enable", name, on ? 1 : 0);
}

// ── Recording, as native's: the button starts a recording of everything playing and, pressed again, stops it
// and saves the WAV. SuperSonic captures the mix out of its shared memory (startCapture, SAB mode), so the
// page needs the cross-origin isolation the server sends; without it the button says so. ─────────────────
// SuperSonic's capture is a ring a second long, read from the page: the recording drains it every quarter
// second into 16-bit chunks, so a take can run as long as memory allows (an hour is ~700 MB as floats, ~350 MB here)
let recording = null;   // { chunks: Int16Array[], frames, sampleRate, channels, tick }
const recBtn = $("btn-record");
const REC_MAX_SECS = 60 * 60;
function recordingUnavailable() {
  if (globalThis.crossOriginIsolated) return null;
  return "Recording needs the site's cross-origin isolation (the headers scripts/serve.mjs sends): not on this host";
}
async function toggleRecording() {
  if (recording) return stopRecording();
  const why = recordingUnavailable();
  if (why) return toast(why);
  const s = await ensureSession();
  if (!s || !engineRef) return;
  try { engineRef.startCapture(); } catch (e) { return toast(`Recording: ${describe(e)}`, true); }   // assertive, as native's
  recording = { chunks: [], frames: 0, sampleRate: 0, channels: 2, tick: 0 };
  recBtn.classList.add("on");
  recBtn.title = "Stop recording and save the WAV";
  toast("Recording");   // said once (toast speaks it)
  recording.tick = setInterval(() => { drainCapture(); if (recording && recording.frames / (recording.sampleRate || 48000) >= REC_MAX_SECS) { toast("Recording stopped after an hour"); stopRecording(); } }, 250);
}
// the ring's contents since the last drain, as 16-bit PCM; the capture restarted at once so the next drain follows on
function drainCapture(final = false) {
  const cap = engineRef.stopCapture();
  if (!final) engineRef.startCapture();
  if (!cap || !cap.frames) return;
  const r = recording, ch = cap.right ? 2 : 1, n = cap.frames;
  const pcm = new Int16Array(n * ch);
  const q = (v) => { const c = Math.max(-1, Math.min(1, v)); return c < 0 ? c * 0x8000 : c * 0x7fff; };
  for (let i = 0, o = 0; i < n; i++) { pcm[o++] = q(cap.left[i]); if (ch === 2) pcm[o++] = q(cap.right[i]); }
  r.chunks.push(pcm); r.frames += n; r.sampleRate = cap.sampleRate; r.channels = ch;
}
function stopRecording() {
  if (!recording) return;
  clearInterval(recording.tick);
  const r = recording;
  recording = null;
  recBtn.classList.remove("on");
  recBtn.title = "Start recording to a WAV audio file (Shift+Cmd/Ctrl+R)";
  try { recording = r; drainCapture(true); } catch (e) { toast(`Recording: ${describe(e)}`); } finally { recording = null; }
  status("Stop recording...");
  if (!r.frames) return toast("Nothing was recorded");
  const blob = wavBlob(r);
  const stamp = new Date().toISOString().replace(/[:T]/g, "-").slice(0, 19);
  const name = `sonic-pi-${stamp}.wav`;
  const a = el("a"); a.href = URL.createObjectURL(blob); a.download = name; document.body.appendChild(a); a.click(); a.remove();
  setTimeout(() => URL.revokeObjectURL(a.href), 60_000);
  toast(`Saved ${name}: ${(r.frames / r.sampleRate).toFixed(1)} s`);
  logs.add("Host", `recording saved: ${name}, ${r.frames} frames at ${r.sampleRate} Hz`);
}
// the chunks as one 16-bit PCM WAV, stereo as native records
function wavBlob({ chunks, frames, sampleRate, channels }) {
  const head = new DataView(new ArrayBuffer(44));
  const str = (o, t) => { for (let i = 0; i < t.length; i++) head.setUint8(o + i, t.charCodeAt(i)); };
  const bytes = frames * channels * 2;
  str(0, "RIFF"); head.setUint32(4, 36 + bytes, true); str(8, "WAVE");
  str(12, "fmt "); head.setUint32(16, 16, true); head.setUint16(20, 1, true); head.setUint16(22, channels, true);
  head.setUint32(24, sampleRate, true); head.setUint32(28, sampleRate * channels * 2, true); head.setUint16(32, channels * 2, true); head.setUint16(34, 16, true);
  str(36, "data"); head.setUint32(40, bytes, true);
  return new Blob([head, ...chunks], { type: "audio/wav" });   // little-endian Int16Arrays, as the platform lays them
}
recBtn.addEventListener("click", toggleRecording);
if (recordingUnavailable()) { recBtn.disabled = true; recBtn.title = recordingUnavailable(); }

// ── Preferences ───────────────────────────────────────────────────────────


function slider(label, { min, max, step = 1, value, format = (v) => v, onInput, tip = null }) {
  const row = el("label", "pref-row");
  if (tip) row.title = tip;   // what it does, on hover, as native's preferences say it
  const input = el("input");
  Object.assign(input, { type: "range", min, max, step, value });
  // named by its words alone (the label around it holds the value too), and its value as shown: "25%", "18px"
  input.setAttribute("aria-label", label);
  input.setAttribute("aria-valuetext", String(format(value)));
  const out = el("span", "pref-val", format(value));
  input.addEventListener("input", () => { out.textContent = format(Number(input.value)); input.setAttribute("aria-valuetext", String(format(Number(input.value)))); onInput(Number(input.value)); });
  row.append(el("span", "", label), input, out);
  return row;
}
// a switch: its track, the preference's glyph (native v5's, icons.js pref-…, lit while on as native tints it), its words
function toggle(label, on, onChange, tip = null, glyph = null) {
  const b = el("button", `switch${on ? " on" : ""}`);
  b.type = "button";
  if (tip) b.title = tip;   // the catch a label cannot carry, where there is one
  b.setAttribute("role", "switch");
  b.setAttribute("aria-checked", String(!!on));
  b.append(el("span", "track", null));
  if (glyph) { const g = el("span", "pref-icon"); g.innerHTML = icon(glyph, ""); g.setAttribute("aria-hidden", "true"); b.append(g); }
  b.append(el("span", "", label));
  b.firstChild.setAttribute("aria-hidden", "true");
  b.addEventListener("click", () => { b.classList.toggle("on"); b.setAttribute("aria-checked", String(b.classList.contains("on"))); onChange(b.classList.contains("on")); });
  const row = el("div", "pref-row");
  row.appendChild(b);
  return row;
}
// options with a glyph are drawn as their glyphs, the chosen one's name beside its own (each button named for a
// screen reader all the same)
function segmented(options, current, onPick) {
  const seg = el("div", "seg");
  for (const [id, label, glyph, tip] of options) {
    const b = el("button", id === current ? "active" : "", glyph ? null : label);
    if (glyph) { seg.classList.add("seg-glyphs"); b.innerHTML = icon(glyph); b.append(el("span", "seg-label", label)); b.setAttribute("aria-label", label); }
    if (tip) b.title = `${label}: ${tip}`;
    b.type = "button";
    b.setAttribute("aria-pressed", String(id === current));
    b.addEventListener("click", () => { [...seg.children].forEach((c) => { c.classList.toggle("active", c === b); c.setAttribute("aria-pressed", String(c === b)); }); onPick(id); });
    seg.appendChild(b);
  }
  return seg;
}

// The colour theme, the bar's own widget (its palette button, at the right of the site's bar): the schemes, each
// shown in its own colours, then the hue, its spread, monochrome and invert, as native's theme preferences have them
const themeButton = siteNav.querySelector(".sn-theme"), themeMenu = $("theme-menu");
function buildThemeMenu() {
  const st = theme.settings();
  themeMenu.textContent = "";
  themeMenu.append(el("h2", "tm-title", "Colour theme"));
  const grid = el("div", "scheme-grid");
  for (const s of theme.schemes()) {
    const b = el("button", `scheme-btn${s.id === st.scheme ? " active" : ""}`);
    b.type = "button";
    b.dataset.scheme = s.id;
    b.setAttribute("aria-pressed", String(s.id === st.scheme));
    // native's ThemeCard: the scheme's background, the toolbar's λ Δ π above its name in its text colour — each glyph
    // in one of the scheme's main colours (its accent, its numbers, its keywords), all as the adjustments now make them
    const glyphs = el("span", "scheme-glyphs");
    glyphs.setAttribute("aria-hidden", "true");
    for (const g of ["info", "help", "prefs"]) { const i = el("i", "scheme-glyph"); i.style.setProperty("--icon", `url(data/toolbar/${g}.png)`); glyphs.appendChild(i); }
    b.append(glyphs, el("span", "scheme-name", s.name));
    paintSchemeCard(b);
    b.addEventListener("click", () => {
      theme.set({ scheme: s.id });
      for (const o of grid.children) { const on = o === b; o.classList.toggle("active", on); o.setAttribute("aria-pressed", String(on)); }
    });
    grid.appendChild(b);
  }
  themeMenu.appendChild(grid);
  themeMenu.appendChild(slider("Hue", { min: 0, max: 359, value: st.hue, format: (v) => `${v}°`, tip: "Rotate the colour wheel", onInput: (v) => theme.set({ hue: v }) }));
  themeMenu.appendChild(slider("Hue spread", { min: 0, max: 100, value: st.spread, format: (v) => `${v}%`, tip: "Spread the colours apart on the spectrum", onInput: (v) => theme.set({ spread: v }) }));
  const foot = el("div", "tm-foot"), switches = el("div", "tm-switches");
  switches.append(toggle("Monochrome", st.monochrome, (on) => theme.set({ monochrome: on }), "Switch to black and white / greyscale mode", "pref-monochrome"),
    toggle("Invert", st.invert, (on) => theme.set({ invert: on }), "Switch to colour inversion mode", "pref-invert"));
  const reset = el("button", "sp-mini-btn tm-reset", "Reset theme");
  reset.type = "button";
  reset.title = "Reset theme to defaults";
  reset.addEventListener("click", () => { theme.resetMods(); buildThemeMenu(); themeMenu.querySelector(".tm-reset")?.focus(); status("Theme reset"); });
  foot.append(switches, reset);
  themeMenu.appendChild(foot);
}
const CARD_GLYPH_COLOURS = ["HighlightedBackground", "NumberForeground", "KeywordForeground"];
function paintSchemeCard(b) {
  const id = b.dataset.scheme, c = (k) => theme.preview(id, k);
  b.style.background = c("Background");
  b.style.color = c("Foreground");
  b.style.setProperty("--card-border", c("WindowBorder") ?? c("Foreground"));
  [...b.querySelectorAll(".scheme-glyph")].forEach((g, i) => { g.style.background = c(CARD_GLYPH_COLOURS[i]) ?? c("HighlightedBackground"); });
}
// the cards follow the adjustments as they are made (a hue dragged, monochrome on)
theme.onChange(() => { if (!themeMenu.hidden) for (const b of themeMenu.querySelectorAll(".scheme-btn")) paintSchemeCard(b); });
function showThemeMenu(on) {
  if (on === !themeMenu.hidden) return;
  if (on) {
    buildThemeMenu();
    const r = themeButton.getBoundingClientRect();
    themeMenu.style.setProperty("--picker-top", `${Math.round(r.bottom) + 6}px`);
    themeMenu.style.setProperty("--menu-right", `${Math.round(Math.max(12, window.innerWidth - r.right))}px`);
  }
  themeMenu.hidden = !on;
  themeButton.setAttribute("aria-expanded", String(on));
  themeButton.classList.toggle("on", on);
  if (on) (themeMenu.querySelector(".scheme-btn.active") ?? themeMenu.querySelector("button"))?.focus();
  else if (themeMenu.contains(document.activeElement)) themeButton.focus();
}
themeButton.addEventListener("click", () => showThemeMenu(themeMenu.hidden));
document.addEventListener("pointerdown", (e) => { if (!themeMenu.hidden && !themeMenu.contains(e.target) && !themeButton.contains(e.target)) showThemeMenu(false); }, true);
window.addEventListener("keydown", (e) => { if (e.key === "Escape" && !themeMenu.hidden) { e.stopPropagation(); e.preventDefault(); showThemeMenu(false); themeButton.focus(); } }, true);   // the keyboard's way out, before the page's own Escape (window's capture is first); back where it came in (Safari does not focus a clicked button)
window.addEventListener("resize", () => showThemeMenu(false));

function buildPrefs() {
  const pane = $("prefs-pane");
  if (!document.body.classList.contains("prefs-open")) return;
  // Rebuilt whole (a switch that changes what is offered, a device plugged in, a keymap changed): the control that
  // had focus is found again in the new one by what it is and says, so a keyboard or screen reader user stays put,
  // as native's preferences, which change in place, keep them
  const had = pane.contains(document.activeElement) ? document.activeElement : null;
  const twin = had && { tag: had.tagName, text: (had.closest("label") ?? had).textContent.trim(), type: had.type };
  const scroll = pane.scrollTop;
  pane.textContent = "";
  // each section is a block of its own, so a wide pane lays them out in columns (style.css)
  const body = el("div", "pref-body");
  pane.appendChild(body);
  let box = body;
  const h = (t) => {
    box = el("section", "pref-group");
    box.appendChild(el("h3", "", t));
    body.appendChild(box);
  };

  h("Scope");
  const modeRow = el("div", "pref-row");
  modeRow.title = "Select scope type";
  modeRow.append(el("span", "", "View"), segmented(SCOPE_MODES, scope.mode, (m) => { scope.mode = m; paintScopeMode(); saveScope(); }));
  box.appendChild(modeRow);
  box.appendChild(slider("Line width", { min: 1, max: 6, step: 0.5, value: scope.lineWidth, tip: "Scope thickness (the pen width of the scope line)", onInput: (v) => { scope.lineWidth = v; saveScope(); } }));
  box.appendChild(slider("Glow", { min: 0, max: 24, value: scope.glow, tip: "How much the scope's lines glow", onInput: (v) => { scope.glow = v; saveScope(); } }));
  box.appendChild(slider("Trail", { min: 0, max: 95, value: Math.round(scope.trail * 100), format: (v) => `${v}%`, tip: "How long the scope's lines linger as they fade", onInput: (v) => { scope.trail = v / 100; saveScope(); } }));

  h("Editor");
  box.appendChild(slider("Code size", { min: 8, max: 40, value: editor.fontSize(), format: (v) => `${v}px`, tip: sizeTitle("Change the size of the code", "TextZoomOut", "TextZoomIn"), onInput: (v) => editor.setFontSize(v) }));
  // the log's text, and the cues': the same zoom as the log's ZoomBar and native's Log Zoom In and Out
  box.appendChild(slider("Log text size", { min: ZOOM_MIN, max: ZOOM_MAX, value: zooms.sidebar ?? 0, format: (v) => `${Math.round(zoomFactor(v) * 100)}%`, tip: sizeTitle("Change the size of the log's and the cues' text", "LogZoomOut", "LogZoomIn"), onInput: (v) => zoomLogs(v - (zooms.sidebar ?? 0), false) }));
  box.appendChild(toggle("Virtual keyboard", codeKeyboard.virtual, (on) => { codeKeyboard.setVirtual(on); buildPrefs(); }, "Use Sonic Pi's live coding keyboard in place of the system keyboard", "pref-keyboard"));
  box.appendChild(slider("Keyboard delay", { min: 0, max: 500, step: 50, value: codeKeyboard.delay, format: (v) => `${v} ms`, tip: "Length of time needed to hold before a touch summons the keyboard", onInput: (v) => codeKeyboard.setDelay(v) }));
  box.appendChild(toggle("Auto-indent on run", editorPrefs.autoIndent, (on) => { editorPrefs.autoIndent = on; store.set("sp-auto-indent-on-run", on); }, keys.title("Align the code every time you run it. When off, use Align Code", "Align"), "pref-auto-indent"));
  box.appendChild(toggle("Show the line and position", !document.body.classList.contains("no-caret-pos"), (on) => { document.body.classList.toggle("no-caret-pos", !on); store.set("sp-caret-pos-shown", on); }, "Show the cursor's line and position", "pref-context"));
  box.appendChild(toggle("Show the metronome", !document.body.classList.contains("no-metro"), (on) => { document.body.classList.toggle("no-metro", !on); store.set("sp-metro-shown", on); }, "Show the metronome controls", "pref-metro"));
  box.appendChild(toggle("Show Loop Scopes", loopScopePrefs.show, (on) => { loopScopePrefs.show = on; store.set("sp-loop-scopes", on); paintLoopScopes(); }, "Show a small scope of its own audio beside each running live loop", "pref-loop-scopes"));
  box.appendChild(toggle("Scroll Loop Scopes", loopScopePrefs.scroll, (on) => { loopScopePrefs.scroll = on; store.set("sp-loop-scope-scroll", on); }, "Scroll the loop scopes sideways, the newest sound on the right. When off, they hold a steady waveform like the main scope", "pref-loop-scroll"));

  // native's Studio > Synths and FX
  h("Synths and FX");
  box.appendChild(toggle("Safe mode", safeMode.on, (on) => { safeMode.on = on; store.set("sp-safe-mode", on); }, "Check synth and FX opts before playing. When off, some values can make unexpectedly loud or harsh sounds", "pref-safe"));
  box.appendChild(toggle("Warn about unknown opts", unknownOpts.warn, (on) => { unknownOpts.warn = on; store.set("sp-warn-unknown-opts", on); },
    "Warn about an opt a synth, FX or sample doesn't have, which is usually a typo. When off, a misspelt opt silently does nothing", "pref-warn"));
  box.appendChild(toggle("Enable external synths", externalSynths.on, (on) => { externalSynths.on = on; store.set("sp-external-synths", on); },
    "Play synths and FX loaded with load_synthdef. When off, Sonic Pi complains about any synth or FX it doesn't recognise", "pref-external"));

  h("Audio");
  box.appendChild(toggle("Low latency", lowLatency.on, (on) => { lowLatency.on = on; store.set("sp-low-latency", on); },
    "Use the smallest audio buffer, so sound follows Run as fast as possible. Turn off if the sound crackles or drops out (from the next load)", "pref-latency"));

  // native's View > Accessibility
  h("Accessibility");
  box.appendChild(toggle("Speak Run and Stop", a11yPrefs.speakTransport, (on) => { a11yPrefs.speakTransport = on; store.set("sp-speak-transport", on); setSpeakTransport(on); }, "Have your screen reader say \"Run started\" and \"Stopped\". Turn off to hear the very start of your music without the announcement over it", "pref-speak"));
  box.appendChild(toggle("Reduce animations", motionPrefs.reduce, (on) => setReduceMotion(on, true), "Keep the interface still: panes and popups appear in place instead of sliding. Always on while your system's reduce motion setting is", "pref-motion"));

  h("Shortcuts");
  shortcutPrefs(box);

  // native's MIDI: on or off (the first time on, the browser asks), then the ports it found
  h("MIDI");
  const midiOn = !!store.get("sp-midi", false);
  const midiSwitch = toggle("Enable MIDI", midiOn && !!navigator.requestMIDIAccess, (on) => (on ? enableMidi() : disableMidi()),
    "Enable communication with connected MIDI devices. Incoming MIDI events (note on, cc, etc) are received as cues. Outgoing MIDI is via the midi functions (see the reference)", "pref-midi");
  // no Web MIDI: Safari on a Mac (another browser has it, or the app), or any browser on an iPhone or iPad (none has)
  const iosDevice = /iPhone|iPad|iPod/.test(navigator.userAgent) || (/Macintosh/.test(navigator.userAgent) && navigator.maxTouchPoints > 1);
  const noMidi = iosDevice ? "Apologies, MIDI is not supported in the browser on your device."
    : "Apologies, MIDI isn't something that Apple's Safari supports. Either switch to a different browser or download the native app.";
  if (!navigator.requestMIDIAccess) {
    const b = midiSwitch.querySelector("button");
    b.disabled = true;
    b.title = noMidi;
  }
  box.appendChild(midiSwitch);
  if (!navigator.requestMIDIAccess) box.appendChild(el("div", "device-list", noMidi));
  else if (midiOn && !midi) box.appendChild(el("div", "device-list", "The ports are listed once the engine has started: press Run"));
  else if (midiOn) {
    const { ins, outs } = midiPorts();
    box.appendChild(el("div", "device-list", `In: ${ins.length ? ins.map(([n]) => n).join(", ") : "none"}`));
    box.appendChild(el("div", "device-list", `Out: ${outs.length ? outs.map(([n]) => n).join(", ") : "none"}`));
  }

  // native's Game Controllers: incoming cues on or off, and each pad there now, on or off
  // the switch is there from the start (it is kept, and asked of the engine when it boots); the pads once it has
  h("Game controllers");
  const padSwitch = toggle("Enable incoming gamepad cues", gamepadPrefs.cues && !!navigator.getGamepads, (on) => { setGamepadCues(on); buildPrefs(); },
    "Automatically route incoming game controller events to cues.", "pref-gamepad");
  if (!navigator.getGamepads) padSwitch.querySelector("button").disabled = true;
  box.appendChild(padSwitch);
  if (!navigator.getGamepads) box.appendChild(el("div", "device-list", "This browser has no gamepad support"));
  else if (!gamepadPrefs.cues) {}
  else if (!engineRef) box.appendChild(el("div", "device-list", "Controllers are listed once the engine has started: press Run"));
  else if (!engineRef.gamepad) box.appendChild(el("div", "device-list", `Game controllers are not available: ${describe(engineRef.gamepadError ?? "the engine has no gamepad support")}`));
  else {
    if (!gamepads.length) box.appendChild(el("div", "device-list", "No controllers yet: plug one in and press a button on it (the browser shows a controller only once it has been pressed)"));
    for (const [name] of gamepads) box.appendChild(toggle(name, !gamepadPrefs.off.has(name), (on) => setGamepadOn(name, on), "Cues from this controller: off mutes it alone.", "pref-gamepad"));
  }

  h("Flight recorder");
  box.appendChild(toggle("Record performance", flight.recording, (on) => { store.set("sp-flight", on); if (on) flight.start(); else flight.stop(); buildPrefs(); },
    "For chasing a glitch or a stutter: while on, the page samples every clock ten times a second, for a report to share. Off, it costs nothing"));
  const flightRow = el("div", "pref-row");
  const markBtn = el("button", "sp-mini-btn", "Mark this moment");
  markBtn.title = keys.title("Mark a significant moment of time in the flight recorder", "FlightMark");
  markBtn.addEventListener("click", () => { if (flightOff()) return; flight.mark("heard", "the player heard something"); toast("marked"); });
  const saveBtn = el("button", "sp-mini-btn", "Save report");
  saveBtn.title = keys.title("Save a flight report to share when something goes wrong", "FlightSave");
  saveBtn.addEventListener("click", () => { if (!flightOff()) flight.save(); });
  markBtn.disabled = saveBtn.disabled = !flight.recording;
  flightRow.append(markBtn, saveBtn);
  box.appendChild(flightRow);

  pane.scrollTop = scroll;
  if (twin) [...pane.querySelectorAll(twin.tag)].find((e) => e.type === twin.type && (e.closest("label") ?? e).textContent.trim() === twin.text)?.focus({ preventScroll: true });

}

// ── Keyboard shortcuts ────────────────────────────────────────────────────
// Native Sonic Pi's, in the keymap the player picked (shortcuts.js). A key
// press is looked up once, here, wherever the focus is, as native's QActions
// are the window's. The editing commands are the editor's and want it
// focused; a text field or a button keeps the keys it types or presses with;
// a key on two commands runs neither, as in Qt, and says so.

// native's commands the web build cannot do, so their keys say why
const UNAVAILABLE = {
  Link: "Joining a Link network is native-only; the tempo and time warp work",
};

// the log, the cues and the toolbar's buttons, shown or hidden as native's View menu has them
const shown = { log: true, cues: true, buttons: true, ...store.get("sp-shown", {}) };
const paintShown = () => { for (const k of Object.keys(shown)) document.body.classList.toggle(`no-${k}`, !shown[k]); };
const SHOWN_NAMES = { log: "log", cues: "cue log", buttons: "buttons" };
const toggleShown = (k, on = !shown[k]) => {
  if (shown[k] !== on) status(`${on ? "Showing" : "Hiding"} ${SHOWN_NAMES[k] ?? k}...`);
  shown[k] = on; store.set("sp-shown", shown); paintShown();
};
paintShown();

// native's log zoom: the log and the cues together, a step as the help pane's panes step
applyZoom("sidebar");
function zoomLogs(delta, repaintPrefs = true) {
  zooms.sidebar = Math.max(ZOOM_MIN, Math.min(ZOOM_MAX, (zooms.sidebar ?? 0) + delta));
  store.set("sp-zooms", zooms);
  applyZoom("sidebar");
  if (repaintPrefs && document.body.classList.contains("prefs-open")) buildPrefs();   // its slider in the preferences follows the buttons and keys
}
// a size's slider in the preferences, titled with both its keys, smaller and larger, as a menu item would show them
const sizeTitle = (text, smaller, larger) => { const a = keys.label(smaller), b = keys.label(larger); return a && b ? `${text} (${a} / ${b})` : text; };

const setPrefsOpen = (on) => { if (document.body.classList.contains("prefs-open") !== on) $("btn-prefs").click(); };

function focusPane(node) {
  if (!node || node.hidden || !node.offsetParent) return false;
  if (!node.matches("input, button, select, textarea, [tabindex]")) node.tabIndex = -1;
  node.focus();
  return true;
}
// a help pane, and in it what native focuses (the docs' topics, a log's tail)
function focusDrawer(which, selector = null) {
  openDrawer(which);
  requestAnimationFrame(() => {
    const pane = $(`${which}-pane`);
    focusPane((selector && pane.querySelector(selector)) || pane);
  });
}
// native's F6 and Shift+F6: the editor, then each pane that shows, and round
function cycleFocus(step) {
  const drawer = document.body.dataset.drawer;
  const panes = [
    editor.view.contentDOM, $("error-pane"), $("log"), $("cues"), $("lk-bpm"),
    panelNow() === "prefs" ? $("prefs-pane") : null,
    drawer ? $(`${drawer}-pane`) : null,
  ].filter((n) => n && !n.hidden && n.offsetParent);
  const at = panes.findIndex((n) => n.contains(document.activeElement));
  const next = panes[at < 0 ? (step > 0 ? 0 : panes.length - 1) : (at + step + panes.length) % panes.length];
  if (next === editor.view.contentDOM) editor.focus();
  else focusPane(next);
}

// native's focus mode: the editor alone and full screen; again, and everything is back as it was
let beforeFocusMode = null;
function toggleFocusMode() {
  const canFullscreen = !(isIPad && !standalone);
  quietFullScreenChange = true;
  setTimeout(() => { quietFullScreenChange = false; }, 500);   // the browser's fullscreenchange comes later
  if (!beforeFocusMode) {
    beforeFocusMode = { drawer: document.body.dataset.drawer, prefs: document.body.classList.contains("prefs-open"), fullscreen: !!fullscreenElement() };
    openDrawer("");
    setPrefsOpen(false);
    document.body.classList.add("focus-mode");
    if (canFullscreen && !beforeFocusMode.fullscreen) $("btn-zen").click();
    editor.focus();
    status(`Focus mode on. Press ${keys.label("FocusMode") || "F10"} to exit.`);
  } else {
    status("Focus mode off.");
    document.body.classList.remove("focus-mode");
    if (beforeFocusMode.drawer) openDrawer(beforeFocusMode.drawer);
    if (beforeFocusMode.prefs) setPrefsOpen(true);
    if (canFullscreen && !beforeFocusMode.fullscreen && fullscreenElement()) $("btn-zen").click();
    beforeFocusMode = null;
  }
}

// native's cycleThemes, in its order
const THEME_CYCLE = ["light", "dark", "mild_dark", "phosphor", "high_contrast", "signal"];
function cycleTheme() {
  const next = THEME_CYCLE[(THEME_CYCLE.indexOf(theme.settings().scheme) + 1) % THEME_CYCLE.length];
  theme.set({ scheme: next });
  toast(`theme: ${theme.schemes().find((s) => s.id === next)?.name ?? next}`);
  buildPrefs();
}

// native's Save As: the buffer to a file, picked where the browser lets a page pick one
async function saveBuffer({ quiet = false } = {}) {
  const name = `${fileName(workspace.bufferName(workspace.active))}.txt`, text = editor.getCode();   // .txt, as native's Save As adds: opens on anything
  if (window.showSaveFilePicker) {
    try {
      const handle = await window.showSaveFilePicker({ suggestedName: name, types: [{ description: "Sonic Pi code", accept: { "text/plain": [".txt", ".rb"] } }] });
      const out = await handle.createWritable();
      await out.write(text);
      await out.close();
      if (!quiet) toast(`saved ${handle.name}`);
      return handle.name;
    } catch (e) {
      if (e.name === "AbortError") return null;
    }
  }
  const a = el("a");
  a.href = URL.createObjectURL(new Blob([text], { type: "text/plain" }));
  a.download = name;
  a.click();
  setTimeout(() => URL.revokeObjectURL(a.href), 1000);
  if (!quiet) toast(`saved ${name}`);
  return name;
}

// a name as a file's: what no system allows in one taken out
const fileName = (name) => name.replace(/[\/\\:*?"<>|\u0000-\u001f]+/g, "-").replace(/^[.\s]+|[.\s]+$/g, "") || "sonic-pi";
// native's Save Set As: the set showing as a .sonicpi file, which native Sonic Pi opens too
async function saveSet({ quiet = false } = {}) {
  const name = `${fileName(workspace.set().name)}.sonicpi`, text = workspace.fileText();
  if (window.showSaveFilePicker) {
    try {
      const handle = await window.showSaveFilePicker({ suggestedName: name, types: [{ description: "Sonic Pi set", accept: { "application/octet-stream": [".sonicpi"] } }] });
      const out = await handle.createWritable();
      await out.write(text);
      await out.close();
      if (!quiet) toast(`saved ${handle.name}`);
      return handle.name;
    } catch (e) {
      if (e.name === "AbortError") return null;
    }
  }
  const a = el("a");
  a.href = URL.createObjectURL(new Blob([text], { type: "application/octet-stream" }));
  a.download = name;
  a.click();
  setTimeout(() => URL.revokeObjectURL(a.href), 1000);
  if (!quiet) toast(`saved ${name}`);
  return name;
}

// what each of native's commands does here; the editing ones (group Code) are editor.command's
const COMMANDS = {
  Run: () => run(),
  Stop: () => stop(),
  Save: () => saveBuffer(),
  Load: () => $("btn-load").click(),
  TextZoomIn: () => editor.setFontSize(editor.fontSize() + 1),
  TextZoomOut: () => editor.setFontSize(editor.fontSize() - 1),
  Scope: () => $("btn-scope").click(),
  CycleThemes: () => cycleTheme(),
  Info: () => $("btn-info").click(),
  Help: () => $("btn-help").click(),
  Prefs: () => $("btn-prefs").click(),
  Record: () => $("btn-record").click(),
  TabPrev: () => workspace.showBuffer((workspace.active + workspace.size - 1) % workspace.size),
  TabNext: () => workspace.showBuffer((workspace.active + 1) % workspace.size),
  ...Object.fromEntries(Array.from({ length: NUM_BUFFERS }, (_, i) => [`Tab${i}`, () => pickBuffer(i)])),   // the pads' own behaviour, jam and all
  TapTempo: () => tapTempo(),
  CycleFocusForward: () => cycleFocus(1),
  CycleFocusBack: () => cycleFocus(-1),
  FocusEditor: () => editor.focus(),
  FocusLogs: () => { toggleShown("log", true); focusPane($("log")); },
  FocusContext: () => focusPane($("caret-pos")),
  FocusCues: () => { toggleShown("cues", true); focusPane($("cues")); },
  FocusPrefs: () => { setPrefsOpen(true); focusPane($("prefs-pane")); },
  FocusHelpListing: () => focusDrawer("docs", ".docs-list"),
  FocusHelpDetails: () => focusDrawer("docs", ".docs-content"),
  FocusErrors: () => focusPane($("error-pane")) || toast("no errors"),
  FocusHelpCards: () => focusDrawer("quickstart"),
  FocusHelpLogs: () => focusDrawer("logs", ".logs-body"),
  FocusHelpDebug: () => focusDrawer("debug"),   // native's Debug pane: the engine's metrics
  FocusBPMScrubber: () => focusPane($("lk-bpm")),
  FocusTimeWarpScrubber: () => focusPane($("lk-warp")),
  ShowButtons: () => toggleShown("buttons"),
  ShowCueLog: () => toggleShown("cues"),
  ShowLog: () => toggleShown("log"),
  LogZoomIn: () => zoomLogs(1),
  LogZoomOut: () => zoomLogs(-1),
  FullScreen: () => $("btn-zen").click(),
  FocusMode: () => toggleFocusMode(),
  ScopePaused: () => { scope.paused = !scope.paused; toast(scope.paused ? "scope paused" : "scope running"); },
  ContextualDocs: () => editor.command("ContextualDocs"),
  ReadCompletionDetails: () => editor.command("ReadCompletionDetails"),
  FlightMark: () => { if (flightOff()) return; flight.mark("heard", "the player heard something"); toast("marked"); },
  FlightSave: () => { if (!flightOff()) flight.save(); },
};

// the platform's own clipboard, undo and select-all keys stay the browser's and the editor's, so a paste needs no permission
const PLATFORM_KEYS = { Copy: "Native+c", Cut: "Native+x", Paste: "Native+v", Undo: "Native+z", Redo: "Native+Shift+z", SelectAll: "Native+a" };
// held down, these repeat; a toggle does not
const REPEATS = new Set(["TextZoomIn", "TextZoomOut", "LogZoomIn", "LogZoomOut", "TabPrev", "TabNext", "CycleFocusForward", "CycleFocusBack"]);

const TEXT_INPUTS = /^(text|search|url|email|password|number|tel)$/;
function entryKind(t) {
  if (!t || t.closest(".cm-content")) return null;
  if (t.isContentEditable || t.tagName === "TEXTAREA") return "area";
  if (t.tagName === "INPUT" && TEXT_INPUTS.test(t.type)) return "field";
  if (t.tagName === "SELECT") return "select";
  if (t.tagName === "BUTTON" || t.tagName === "INPUT" || t.getAttribute("role") === "button") return "control";
  return null;
}
const MOVE_KEYS = new Set(["Backspace", "Delete", "ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown", "Home", "End", "PageUp", "PageDown", "Tab"]);
// what a focused text field, list or button does with a key itself, and a shortcut leaves to it, as a QLineEdit claims its keys
function takesKey(kind, chord, inSearch = false) {
  const c = parseChord(chord);
  const plain = !c.ctrl && !c.meta && !c.alt;
  if (kind === "control") return plain && !c.shift && (c.key === " " || c.key === "Enter" || MOVE_KEYS.has(c.key));
  // Shift+Enter is native's tap tempo everywhere but the find bar, where it is the previous match
  if (plain) return c.key.length === 1 || MOVE_KEYS.has(c.key) || (c.key === "Enter" && (kind !== "field" || !c.shift || inSearch));
  const mac = keys.platform === "mac";
  if ((mac ? c.meta && !c.ctrl : c.ctrl && !c.meta) && !c.alt && (/^[acvxzy]$/.test(c.key) || MOVE_KEYS.has(c.key))) return true;
  if (mac && c.ctrl && !c.meta && !c.alt && /^[abefnpdhktv]$/.test(c.key)) return true;     // a Mac's Cocoa keys, in every text field
  return c.alt && !c.ctrl && !c.meta && ["ArrowLeft", "ArrowRight", "Backspace", "Delete"].includes(c.key);
}

// A held main thread (a dialog up, a tab in the background) is logged with how long it was held and what the
// audio did after, so a stutter on return can be read in the Logs pane
{ let last = performance.now(); setInterval(() => { const now = performance.now(), gap = now - last; last = now; if (gap > 1000) { logs.add("Host", `main thread held for ${(gap / 1000).toFixed(1)} s; audio context ${engineRef?.audioContext?.state ?? "not booted"}, ${liveJobs} job(s) live`); } }, 250); }
// Leaving or reloading with sound playing, or a recording running, asks first. Chrome and Firefox let the page keep
// Cmd/Ctrl+R for Run (below); Safari reserves it for reload and lets no page stop it, so there this prompt is what stands
// between a reflex and a lost performance. Cmd+Return runs everywhere.
window.addEventListener("beforeunload", (e) => { if (liveJobs || recording) { e.preventDefault(); e.returnValue = ""; } });
// Cmd/Ctrl+R is Run here, never the browser's reload, wherever the focus is and whatever is showing: an app that
// loses its buffers to a reflex is worse than one that asks for the menu to reload
document.addEventListener("keydown", (e) => { if ((e.metaKey || e.ctrlKey) && !e.shiftKey && !e.altKey && e.key.toLowerCase() === "r") e.preventDefault(); }, true);
document.addEventListener("keydown", (e) => {
  if (e.isComposing || e.keyCode === 229) return;
  if (e.defaultPrevented && !((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === "r")) return;
  if (infoOpen()) return;     // the Info card is up: its cards have their own keys
  const hit = keys.match(e);
  if (!hit) return;
  const began = origin(e);   // inside the editor's shadow root, not its mount (shadow.js)
  const target = began instanceof Element ? began : null;
  const kind = entryKind(target);
  const inSearch = !!target?.closest(".cm-search");
  if (kind && takesKey(kind, hit.chord, inSearch)) return;
  if (hit.ids.length > 1) {
    e.preventDefault();
    if (!e.repeat) toast(`${keys.format(hit.chord)} is on ${hit.ids.length} commands, so none runs: see Preferences, Shortcuts`);
    return;
  }
  const [id] = hit.ids;
  const editing = DEF.get(id).group === "Code";
  if (editing && !target?.closest(".cm-content") && !(inSearch && id.startsWith("Find"))) return;
  if (PLATFORM_KEYS[id] && keys.resolve(PLATFORM_KEYS[id]) === hit.chord) return;
  e.preventDefault();
  e.stopPropagation();
  if (e.repeat && !editing && !REPEATS.has(id)) return;
  if (UNAVAILABLE[id]) toast(UNAVAILABLE[id]);
  else if (editing) editor.command(id, { inSearch });
  else COMMANDS[id]?.();
}, true);

const shortcutEditor = createShortcutEditor({ keys, unavailable: UNAVAILABLE, standalone, toast });

// native's tooltips name each action's key, in the keymap in force
function paintKeyTitles() {
  for (const [id, text, command] of [
    ["btn-run", "Run", "Run"], ["btn-stop", "Stop everything", "Stop"], ["btn-load", "Load a file: a buffer's code into a free buffer, or a set (.sonicpi) as a set of its own", "Load"],
    ["btn-size-down", "Code size down", "TextZoomOut"], ["btn-size-up", "Code size up", "TextZoomIn"],
    ["btn-scope", "Show or hide the scope", "Scope"], ["btn-info", "Info about Sonic Pi", "Info"],
    ["btn-help", "Help: cards, docs and threads", "Help"], ["btn-prefs", "Preferences", "Prefs"], ["btn-zen", "Full screen", "FullScreen"],
    ["lk-tap", "Tap tempo", "TapTempo"],
  ]) $(id).title = keys.title(text, command);
  tabs.retitle();
  editor.titleKeys((text, command) => keys.title(text, command));
}
paintKeyTitles();
keys.onChange(() => { paintKeyTitles(); buildPrefs(); });

// Preferences, Shortcuts: the keymap, and the editor for the rest
function shortcutPrefs(pane) {
  const row = el("div", "pref-row");
  row.title = "Choose your shortcut mode";
  row.append(el("span", "", "Keymap"), segmented(MODES, keys.mode, (m) => keys.setMode(m)));
  pane.appendChild(row);
  const edit = el("button", "sp-mini-btn", "Edit shortcuts…");
  edit.type = "button";
  edit.title = "Create your own custom shortcuts";
  edit.addEventListener("click", () => shortcutEditor.open());
  const row2 = el("div", "pref-row");
  row2.append(el("span", "", ""), edit);
  pane.appendChild(row2);
}

// Everything is up: the editor gets the keys. Showing, it is woken (its runtime and completion on their way); a page of
// the site wakes the runtime once a card is reached for, so a Play is not kept waiting on it
editor.focus();
if (!infoOpen()) wakeEditor();
else for (const type of ["pointerover", "pointerdown", "focusin"]) infoCard.addEventListener(type, (e) => { if (e.target.closest?.(".qs-card, .home-synth")) startRuntime().catch(() => {}); }, { passive: true });
window.sonicPi = { editor, workspace, keyboard: codeKeyboard, api, theme, get session() { return session; }, docs: () => docs, quickstart: () => quickstart, scope, processTree: () => insight.tree.snapshot(), pianoRoll: () => insight.roll.snapshot(), flight, keys, shortcuts: shortcutEditor, get engine() { return engineRef; } };
