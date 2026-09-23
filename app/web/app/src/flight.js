// SPDX-License-Identifier: AGPL-3.0-or-later
// The flight recorder: when a performance cracks, pops or jitters, the data
// to find out why. Ten times a second it samples every clock that can go
// wrong — SuperSonic's (glitches heard, bundles played late, messages
// dropped, queue depths), the runtime's (the headroom each sound left with,
// ticks' cost and how late they woke), the page's (stalls, frame times, what
// each view cost to draw, memory) — into a buffer of the last five minutes.
// A glitch, a late bundle, a drop or a stall marks the moment; so does the
// player ("I heard that"). A report is the whole buffer, the marks, the
// programs that were running, and the runtime's records around each mark,
// all on the engine's clock so a crackle lines up with what played.
import { perfTake } from "./perf.js";

const SAMPLE_MS = 100;
const KEEP_SAMPLES = 3000;      // five minutes
const KEEP_RECORDS = 20000;
const AROUND_MARK = 3;          // seconds of records kept either side of a mark

const ENGINE_KEYS = [
  "glitchCount", "glitchDurationMs", "averageLatencyUs", "maxLatencyUs", "hasPlaybackStats",
  "engineProcessCount", "engineMessagesProcessed", "engineMessagesDropped", "engineSequenceGaps", "engineWasmErrors",
  "engineSchedulerDepth", "engineSchedulerPeakDepth", "engineSchedulerDropped", "engineSchedulerLates",
  "engineSchedulerMaxLateMs", "engineSchedulerLastLateMs", "ringBufferDirectWriteFails",
  "inBufferUsedBytes", "inBufferPeakBytes", "outBufferUsedBytes", "outBufferPeakBytes", "audioBlockSize", "audioSampleRate",
];

/**
 * @param hooks { session() → the LiveSession or null, programs() → [{job, buffer, code}], versions() → {} }
 */
export function createFlightRecorder(hooks) {
  const samples = [];
  const marks = [];
  const records = [];
  let prevEngine = null;
  let longTasks = [];
  let frameMax = 0;
  let frames = 0;
  let lastFrame = null;
  const listeners = new Set();

  // stalls on the page's one thread, where the browser reports them
  try {
    new PerformanceObserver((list) => {
      for (const e of list.getEntries()) longTasks.push({ at: e.startTime, ms: e.duration });
    }).observe({ type: "longtask", buffered: true });
  } catch { /* not in this browser */ }

  // frame gaps: the page's paint cadence, as a player sees jank
  const onFrame = (t) => {
    if (lastFrame != null) frameMax = Math.max(frameMax, t - lastFrame);
    lastFrame = t;
    frames++;
    requestAnimationFrame(onFrame);
  };
  requestAnimationFrame(onFrame);

  function mark(kind, detail = "", clock = null) {
    const session = hooks.session();
    const m = { at: performance.now(), clock: clock ?? session?.clockNow() ?? null, kind, detail };
    marks.push(m);
    if (marks.length > 500) marks.shift();
    for (const fn of listeners) fn({ type: "mark", mark: m });
    return m;
  }

  function sample() {
    try {
      takeSample();
    } catch (e) {
      // one bad reading must not stop the recording
      mark("recorder-error", String(e.message ?? e));
    }
  }

  // sent-late is a steady state, not an event: say it at most this often, with what it added up to
  const LATE_EVERY_MS = 5000;
  let lateSounds = 0, lateWorst = 0, lateSaid = 0;

  function takeSample() {
    const session = hooks.session();
    const at = performance.now();
    const s = { at, clock: session?.clockNow() ?? null };
    const tasks = longTasks;
    longTasks = [];
    s.page = {
      longTasks: tasks.length,
      longTaskMsMax: tasks.reduce((m, t) => Math.max(m, t.ms), 0),
      frames,
      frameGapMsMax: frameMax,
      jsHeapBytes: performance.memory?.usedJSHeapSize ?? null,
      hidden: document.hidden,
    };
    frames = 0;
    frameMax = 0;
    s.gui = perfTake();
    if (session) {
      s.runtime = session.takePerf();
      if (s.runtime.headroomMin === Infinity) s.runtime.headroomMin = null;
      s.runtime.heapBytes = session.runtimeHeapBytes();
      const ac = session.audioContext;
      s.audio = { state: ac?.state, baseLatency: ac?.baseLatency ?? null, outputLatency: ac?.outputLatency ?? null };
      try {
        const all = session.engineMetrics();
        s.engine = {};
        for (const k of ENGINE_KEYS) if (all[k] !== undefined) s.engine[k] = all[k];
      } catch (e) {
        s.engine = { error: String(e.message ?? e) };
      }
      // the moments worth finding later
      const e = s.engine, p = prevEngine;
      if (p && e.glitchCount > p.glitchCount) mark("glitch", `${e.glitchCount - p.glitchCount} dropout(s), ${e.glitchDurationMs - p.glitchDurationMs} ms silent`, s.clock);
      if (p && e.engineSchedulerLates > p.engineSchedulerLates) mark("late-bundles", `${e.engineSchedulerLates - p.engineSchedulerLates} played late, last ${e.engineSchedulerLastLateMs} ms`, s.clock);
      if (p && (e.engineMessagesDropped > p.engineMessagesDropped || e.engineSchedulerDropped > p.engineSchedulerDropped)) mark("dropped", `${(e.engineMessagesDropped - p.engineMessagesDropped) + (e.engineSchedulerDropped - p.engineSchedulerDropped)} messages`, s.clock);
      if (p && e.engineSequenceGaps > p.engineSequenceGaps) mark("gaps", `${e.engineSequenceGaps - p.engineSequenceGaps} lost in transit`, s.clock);
      // Sounds going out late is the steady state of a program cued from outside (use_real_time gives a note
      // the moment the cue arrived, so there is no headroom to lose), and a mark ten times a second is not a
      // moment worth finding — it is a flood that buries the marks that are, and a line in the Logs pane for
      // every one. So they are counted and said at most once a stretch, with the worst of the stretch.
      lateSounds += s.runtime.sentLate;
      if (s.runtime.headroomMin != null && s.runtime.headroomMin < lateWorst) lateWorst = s.runtime.headroomMin;
      if (lateSounds > 0 && s.at - lateSaid >= LATE_EVERY_MS) {
        mark("sent-late", `${lateSounds} sounds left after their time in the last ${Math.round((s.at - lateSaid) / 1000)} s; worst ${Math.round(lateWorst * 1000)} ms`, s.clock);
        lateSaid = s.at;
        lateSounds = 0;
        lateWorst = 0;
      }
      prevEngine = e;
    }
    if (s.page.longTaskMsMax >= 100) mark("stall", `${Math.round(s.page.longTaskMsMax)} ms task`, s.clock);
    samples.push(s);
    if (samples.length > KEEP_SAMPLES) samples.shift();
    for (const fn of listeners) fn({ type: "sample", sample: s });
  }
  setInterval(sample, SAMPLE_MS);

  /** One runtime record, kept compactly for the report. */
  function record(r) {
    const c = { time: r.time, kind: r.kind, job: r.job, thread: r.thread, name: r.name || undefined, line: r.line };
    if (r.kind === "synth") { c.synth = r.synth; c.note = r.args?.note; c.buf = r.args?.buf; }
    else if (r.kind === "midi") { c.path = r.path; }
    else if (r.kind === "sleep") { c.beats = r.beats; }
    else if (r.kind === "cue") { c.address = r.address; }
    else if (r.kind === "error") { c.message = r.message; }
    else if (r.kind === "output" || r.kind === "log") return;
    records.push(c);
    if (records.length > KEEP_RECORDS) records.splice(0, records.length - KEEP_RECORDS);
  }

  /** Totals over the samples since a time (performance.now ms). */
  function summary(since = 0) {
    const ss = samples.filter((s) => s.at >= since);
    const sum = { samples: ss.length, seconds: ss.length * SAMPLE_MS / 1000, sounds: 0, records: 0, headroomMinMs: null, headroomAvgMs: null, sentLate: 0, tight: 0,
      tickMsMax: 0, tickMsAvg: null, statusMsMax: 0, wakeLateMsMax: 0, recordMsMax: 0, longTasks: 0, longTaskMsMax: 0, frameGapMsMax: 0,
      engineLates: 0, engineMaxLateMs: 0, dropped: 0, gaps: 0, glitches: null, glitchMs: null, schedulerPeakDepth: 0, gui: {} };
    let headroomSum = 0, ticks = 0, tickMs = 0;
    let prev = null;
    for (const s of ss) {
      const r = s.runtime;
      if (r) {
        sum.sounds += r.sounds;
        sum.records += r.records;
        headroomSum += r.headroomSum;
        if (r.headroomMin != null) sum.headroomMinMs = Math.min(sum.headroomMinMs ?? Infinity, r.headroomMin * 1000);
        sum.sentLate += r.sentLate;
        sum.tight += r.tight;
        sum.tickMsMax = Math.max(sum.tickMsMax, r.tickMsMax);
        sum.statusMsMax = Math.max(sum.statusMsMax, r.statusMsMax);
        sum.wakeLateMsMax = Math.max(sum.wakeLateMsMax, r.wakeLateMsMax);
        sum.recordMsMax = Math.max(sum.recordMsMax, r.recordMsMax);
        ticks += r.ticks;
        tickMs += r.tickMs;
      }
      sum.longTasks += s.page.longTasks;
      sum.longTaskMsMax = Math.max(sum.longTaskMsMax, s.page.longTaskMsMax);
      sum.frameGapMsMax = Math.max(sum.frameGapMsMax, s.page.frameGapMsMax);
      for (const [name, g] of Object.entries(s.gui || {})) {
        const t = (sum.gui[name] ??= { count: 0, totalMs: 0, maxMs: 0 });
        t.count += g.count;
        t.totalMs += g.totalMs;
        t.maxMs = Math.max(t.maxMs, g.maxMs);
      }
      if (s.engine) sum.schedulerPeakDepth = Math.max(sum.schedulerPeakDepth, s.engine.engineSchedulerPeakDepth ?? 0);
      // the engine's max late is since boot; the window's is the last late seen whenever the count rose
      if (s.engine && prev?.engine && s.engine.engineSchedulerLates > prev.engine.engineSchedulerLates) {
        sum.windowMaxLateMs = Math.max(sum.windowMaxLateMs ?? 0, s.engine.engineSchedulerLastLateMs ?? 0);
      }
      prev = s;
    }
    const first = ss.find((s) => s.engine && !s.engine.error)?.engine, last = [...ss].reverse().find((s) => s.engine && !s.engine.error)?.engine;
    if (first && last) {
      sum.engineLates = last.engineSchedulerLates - first.engineSchedulerLates;
      sum.engineMaxLateMs = sum.windowMaxLateMs ?? 0;   // within this window, not since boot
      sum.dropped = (last.engineMessagesDropped - first.engineMessagesDropped) + (last.engineSchedulerDropped - first.engineSchedulerDropped);
      sum.gaps = last.engineSequenceGaps - first.engineSequenceGaps;
      if (last.hasPlaybackStats) {
        sum.glitches = last.glitchCount - first.glitchCount;
        sum.glitchMs = last.glitchDurationMs - first.glitchDurationMs;
      }
    }
    if (sum.sounds) sum.headroomAvgMs = (headroomSum / sum.sounds) * 1000;
    if (ticks) sum.tickMsAvg = tickMs / ticks;
    return sum;
  }

  /** Everything, for analysis. */
  function report() {
    const session = hooks.session();
    const ac = session?.audioContext;
    const windows = marks.filter((m) => m.clock != null).map((m) => [m.clock - AROUND_MARK, m.clock + AROUND_MARK]);
    const lastClock = samples.length ? samples[samples.length - 1].clock : null;
    if (lastClock != null) windows.push([lastClock - 10, lastClock + 1]);
    const keep = records.filter((r) => windows.some(([a, b]) => r.time >= a && r.time <= b));
    return {
      format: "sonic-pi flight report",
      version: 1,
      saved: new Date().toISOString(),
      meta: {
        userAgent: navigator.userAgent,
        hardwareConcurrency: navigator.hardwareConcurrency ?? null,
        deviceMemoryGb: navigator.deviceMemory ?? null,
        crossOriginIsolated: self.crossOriginIsolated ?? null,
        sampleRate: ac?.sampleRate ?? null,
        baseLatency: ac?.baseLatency ?? null,
        outputLatency: ac?.outputLatency ?? null,
        sampleMs: SAMPLE_MS,
        ...hooks.versions(),
      },
      summary: summary(samples.length ? samples[0].at : 0),
      marks,
      programs: hooks.programs(),
      samples,
      records: keep,
    };
  }

  function save() {
    const blob = new Blob([JSON.stringify(report())], { type: "application/json" });
    const a = document.createElement("a");
    a.href = URL.createObjectURL(blob);
    a.download = `sonic-pi-flight-${new Date().toISOString().replace(/[:.]/g, "-")}.json`;
    document.body.appendChild(a);
    a.click();
    setTimeout(() => { URL.revokeObjectURL(a.href); a.remove(); }, 1000);
  }

  return {
    mark,
    record,
    summary,
    report,
    save,
    on: (fn) => (listeners.add(fn), () => listeners.delete(fn)),
    latest: () => samples[samples.length - 1] ?? null,
  };
}
