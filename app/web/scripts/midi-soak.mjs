#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// A controller held down for minutes, in a real browser: does it stay on time, and does anything grow?
//
// A fake MIDI device (no hardware needed: MidiManager takes its access injected) sends control changes at a
// player's rate while a live_loop syncs on them and plays. The flight recorder is read every half second, and
// the run is reported minute by minute: what a tick cost, how late a sound left, what the engine did with it,
// and every heap we can see. A leak or a slide shows as a trend across the minutes; a healthy run is flat.
//
//   node scripts/serve.mjs --https &
//   node scripts/midi-soak.mjs [https://127.0.0.1:8460/web/] [--minutes 6] [--rate 380] [--program plain|puts]
import fs from "node:fs";
import { quiet } from "./lib/quiet.mjs";

const BASE = process.argv.find((a) => a.startsWith("http")) ?? "https://127.0.0.1:8460/web/";
const opt = (n, d) => { const i = process.argv.indexOf(n); return i < 0 ? d : process.argv[i + 1]; };
const MINUTES = Number(opt("--minutes", 6));
const RATE = Number(opt("--rate", 380));           // messages a second, as a knob spun hard sends
const WHICH = opt("--program", "puts");
const GC = process.argv.includes("--gc");   // collect before every heap reading, so each is a post-GC floor
const CYCLE = Number(opt("--cycle", 0));          // seconds between Stop/Run, as live coding does; 0 never restarts
const { chromium } = await import("playwright");

const PROGRAMS = {
  // a player's program: puts on every message is part of the load
  puts: 'with_fx :reverb, room: 1 do\n  live_loop :foo do\n    use_real_time\n    n, v = sync "/midi:midi_fighter_twister:1/control_change"\n    puts [n, v]\n    play v, release: 0.1\n  end\nend',
  // the same without the log line, to say how much of the cost is the record stream
  plain: 'with_fx :reverb, room: 1 do\n  live_loop :foo do\n    use_real_time\n    n, v = sync "/midi:midi_fighter_twister:1/control_change"\n    play v, release: 0.1\n  end\nend',
};

const browser = quiet(await chromium.launch({
  args: ["--autoplay-policy=no-user-gesture-required", "--mute-audio", "--disable-background-timer-throttling",
         "--disable-renderer-backgrounding", "--disable-backgrounding-occluded-windows", "--js-flags=--expose-gc"],
}));
const page = await browser.newPage({ ignoreHTTPSErrors: true });
// The page's own performance.memory is quantised AND cached (Chrome refreshes it about every 20 minutes), so a
// soak reading it sees one number all run and then a step. CDP answers with the real used heap each time.
const cdp = await page.context().newCDPSession(page);
await cdp.send("Performance.enable").catch(() => {});
// A heap read between collections says whatever GC happened to be doing. Collecting first makes every reading
// a floor, and a floor that climbs is a leak — the only way to tell one from ordinary churn.
const collect = async () => { try { await cdp.send("HeapProfiler.collectGarbage"); } catch {} };
const heapNow = async () => {
  try {
    const { metrics } = await cdp.send("Performance.getMetrics");
    const by = Object.fromEntries(metrics.map((m) => [m.name, m.value]));
    // TaskDuration is the main thread's cumulative busy time. Against wall clock it says what share of the
    // thread the page is using — the one number that cannot be faked by a throttled timer or a missing observer.
    return { js: by.JSHeapUsedSize ?? 0, total: by.JSHeapTotalSize ?? 0, nodes: by.Nodes ?? 0,
             listeners: by.JSEventListeners ?? 0, task: by.TaskDuration ?? 0, ts: by.Timestamp ?? 0 };
  } catch { return { js: 0, total: 0, nodes: 0, listeners: 0, task: 0, ts: 0 }; }
};
const errors = [];
page.on("pageerror", (e) => errors.push(String(e).slice(0, 200)));
page.on("console", (m) => { if (m.type() === "error") errors.push(m.text().slice(0, 200)); });

await page.addInitScript(() => {
  const mk = (id, name) => ({ id, name, state: "connected", onmidimessage: null, send() {} });
  const input = mk("in1", "Midi Fighter Twister"), output = mk("out1", "Midi Fighter Twister");
  const access = { inputs: new Map([["in1", input]]), outputs: new Map([["out1", output]]), onstatechange: null };
  globalThis.__fakeMidi = { access, input, output, sent: 0 };
  navigator.requestMIDIAccess = async () => access;
  try { localStorage.setItem("sp-midi", "true"); } catch {}
});

console.log(`${BASE}  ${MINUTES} min at ${RATE} msg/s, program "${WHICH}"`);
await page.goto(BASE + "#app");
await page.waitForFunction(() => /ready|Error/.test(document.getElementById("status-engine")?.textContent ?? ""), null, { timeout: 60000 });
await page.evaluate((code) => globalThis.sonicPi.editor.setCode(code), PROGRAMS[WHICH]);
await page.click("#btn-run");
await page.waitForFunction(() => globalThis.sonicPi.engine?.midi != null, null, { timeout: 60000 });
console.log("engine up, MIDI up, program running\n");

// The controller: a self-correcting loop that keeps to the rate without blocking the thread it measures.
await page.evaluate((rate) => {
  const f = globalThis.__fakeMidi;
  f.sent = 0;
  const started = performance.now();
  const step = () => {
    const due = Math.floor(((performance.now() - started) / 1000) * rate);
    for (let i = f.sent; i < due; i++) {
      f.input.onmidimessage({ data: new Uint8Array([0xb0, 5, 40 + (i % 60)]), timeStamp: performance.now() });
    }
    f.sent = Math.max(f.sent, due);
    f.timer = setTimeout(step, 2);
  };
  step();
}, RATE);

const rows = [];
const t0 = Date.now();
let last = null, lastCycle = Date.now(), cycles = 0;
while (Date.now() - t0 < MINUTES * 60000) {
  await page.waitForTimeout(500);
  // Live coding: the same buffer stopped and run again, over and over, while the controller keeps sending.
  if (CYCLE && Date.now() - lastCycle >= CYCLE * 1000) {
    lastCycle = Date.now();
    cycles++;
    await page.click("#btn-stop").catch(() => {});
    await page.waitForTimeout(250);
    await page.click("#btn-run").catch(() => {});
  }
  if (GC) await collect();
  const hp = await heapNow();
  const s = await page.evaluate(() => {
    const l = globalThis.sonicPi.flight.latest();
    if (!l) return null;
    return { at: l.at, tickMs: l.runtime?.tickMs ?? 0, ticks: l.runtime?.ticks ?? 0, tickMsMax: l.runtime?.tickMsMax ?? 0,
      headroomMin: l.runtime?.headroomMin, sentLate: l.runtime?.sentLate ?? 0, sounds: l.runtime?.sounds ?? 0,
      records: l.runtime?.records ?? 0, wasmHeap: l.runtime?.heapBytes ?? 0, jsHeap: l.page?.jsHeapBytes ?? 0,
      longTasks: l.page?.longTasks ?? 0, longTaskMsMax: l.page?.longTaskMsMax ?? 0,
      recordMsMax: l.runtime?.recordMsMax ?? 0, frameGapMsMax: l.page?.frameGapMsMax ?? 0,
      lates: l.engine?.engineSchedulerLates ?? 0, dropped: (l.engine?.engineMessagesDropped ?? 0) + (l.engine?.engineSchedulerDropped ?? 0),
      glitches: l.engine?.glitchCount ?? 0, cues: globalThis.__fakeMidi.sent,
      procRows: (() => { try { return globalThis.sonicPi.session?.processTable()?.length ?? 0; } catch { return 0; } })() };
  });
  if (s && s.at !== last) { rows.push({ ...s, cdpJs: hp.js, cdpNodes: hp.nodes, cdpListeners: hp.listeners, cdpTask: hp.task, cdpTs: hp.ts, minute: Math.floor((Date.now() - t0) / 60000) }); last = s.at; }
}
await page.evaluate(() => clearTimeout(globalThis.__fakeMidi.timer));
// What the page itself said while this ran: its own watchdog and the engine's complaints are worth more than
// a counter, since they name what went wrong.
const said = await page.evaluate(() => {
  const out = {};
  for (const sec of document.querySelectorAll(".logs-source")) {
    const name = sec.querySelector(".logs-title")?.textContent ?? "?";
    const lines = [...sec.querySelector(".logs-body").children].map((n) => n.textContent);
    out[name] = { lines: lines.length,
      held: lines.filter((l) => /main thread held/.test(l)).length,
      suspended: lines.filter((l) => /audio suspended/.test(l)).length,
      late: lines.filter((l) => /\bLATE\b/.test(l)).length,
      sentLate: lines.filter((l) => /sent-late/.test(l)).length,
      worstHeld: Math.max(0, ...lines.map((l) => Number((/main thread held for ([\d.]+) s/.exec(l) ?? [])[1] ?? 0))) };
  }
  return out;
});
console.log("\nwhat the page logged:");
for (const [k, v] of Object.entries(said)) console.log(`  ${k}: ${v.lines} lines · held ${v.held} (worst ${v.worstHeld}s) · suspended ${v.suspended} · engine LATE ${v.late} · sent-late marks ${v.sentLate}`);
await page.waitForTimeout(1000);

// Per minute: what it cost and what it cost us.
const mins = [...new Set(rows.map((r) => r.minute))].sort((a, b) => a - b);
console.log(`min  msg/s  ms/tick  worst tick  worst late  sounds late  eng late  drop  glitch  wasm MB  js MB  longtask  proc rows  DOM  listeners  rec ms max  frame gap  main busy${CYCLE ? `   (Stop/Run every ${CYCLE}s)` : ""}`);
const fmt = [];
for (const m of mins) {
  const R = rows.filter((r) => r.minute === m);
  if (!R.length) continue;
  const ticks = R.reduce((a, r) => a + r.ticks, 0), tickMs = R.reduce((a, r) => a + r.tickMs, 0);
  const heads = R.map((r) => r.headroomMin).filter((h) => h != null);
  const row = {
    minute: m + 1,
    rate: Math.round((R[R.length - 1].cues - R[0].cues) / Math.max(1, (R[R.length - 1].at - R[0].at) / 1000)),
    msPerTick: ticks ? tickMs / ticks : 0,
    worstTick: Math.max(...R.map((r) => r.tickMsMax)),
    worstLateMs: heads.length ? Math.min(...heads) * 1000 : null,
    soundsLate: R.reduce((a, r) => a + r.sentLate, 0),
    engLate: R[R.length - 1].lates - R[0].lates,
    dropped: R[R.length - 1].dropped - R[0].dropped,
    glitches: R[R.length - 1].glitches - R[0].glitches,
    wasmMB: R[R.length - 1].wasmHeap / 1048576,
    jsMB: R[R.length - 1].cdpJs / 1048576,
    domNodes: R[R.length - 1].cdpNodes,
    mainBusy: (R.length > 1 && R[R.length - 1].cdpTs > R[0].cdpTs)
      ? (R[R.length - 1].cdpTask - R[0].cdpTask) / (R[R.length - 1].cdpTs - R[0].cdpTs) : 0,
    listeners: R[R.length - 1].cdpListeners,
    longTasks: R.reduce((a, r) => a + r.longTasks, 0),
    recordMsMax: Math.max(...R.map((r) => r.recordMsMax)),
    frameGapMsMax: Math.max(...R.map((r) => r.frameGapMsMax)),
  };
  fmt.push(row);
  row.procRows = R[R.length - 1].procRows;
  console.log([String(row.minute).padStart(3), String(row.rate).padStart(6), row.msPerTick.toFixed(3).padStart(8),
    row.worstTick.toFixed(2).padStart(11), (row.worstLateMs == null ? "-" : row.worstLateMs.toFixed(2)).padStart(11),
    String(row.soundsLate).padStart(12), String(row.engLate).padStart(9), String(row.dropped).padStart(5),
    String(row.glitches).padStart(7), row.wasmMB.toFixed(1).padStart(8), row.jsMB.toFixed(1).padStart(6),
    String(row.longTasks).padStart(9), String(row.procRows).padStart(9),
    String(row.domNodes).padStart(7), String(row.listeners).padStart(6),
    row.recordMsMax.toFixed(2).padStart(10), row.frameGapMsMax.toFixed(0).padStart(9),
    `${(row.mainBusy * 100).toFixed(0)}%`.padStart(10)].join(" "));
}

// A verdict rather than a table to read: first minute against last.
if (fmt.length >= 2) {
  const a = fmt[0], z = fmt[fmt.length - 1];
  const drift = (x, y) => (x === 0 ? (y === 0 ? 0 : Infinity) : (y - x) / x);
  const say = (name, ok, detail) => console.log(`${ok ? "pass" : "FAIL"} ${name}: ${detail}`);
  console.log();
  say("a tick costs the same at the end as at the start", drift(a.msPerTick, z.msPerTick) < 0.25,
    `${a.msPerTick.toFixed(3)} ms → ${z.msPerTick.toFixed(3)} ms`);
  say("the wasm heap does not grow", z.wasmMB - a.wasmMB < 4, `${a.wasmMB.toFixed(1)} MB → ${z.wasmMB.toFixed(1)} MB`);
  const floor = (rs) => Math.min(...rs.map((r) => r.cdpJs / 1048576));
  const firstThird = rows.filter((r) => r.minute < Math.max(1, Math.floor(mins.length / 3)));
  const lastThird = rows.filter((r) => r.minute >= mins.length - Math.max(1, Math.floor(mins.length / 3)));
  const f0 = floor(firstThird), f1 = floor(lastThird);
  say("the page heap does not grow", f1 - f0 < 12, `floor ${f0.toFixed(1)} MB → ${f1.toFixed(1)} MB${GC ? " (forced GC)" : " (CDP)"}`);
  say("the DOM does not grow", z.domNodes <= Math.max(a.domNodes * 1.5, a.domNodes + 500), `${a.domNodes} nodes → ${z.domNodes}`);
  say("listeners do not accumulate", z.listeners <= Math.max(a.listeners * 1.5, a.listeners + 200), `${a.listeners} → ${z.listeners}`);
  say("sounds do not go out later as it runs", (z.worstLateMs ?? 0) >= (a.worstLateMs ?? 0) - 2,
    `worst ${a.worstLateMs?.toFixed(2)} ms → ${z.worstLateMs?.toFixed(2)} ms`);
  say("the engine plays no more late as it runs", z.engLate <= Math.max(2, a.engLate), `${a.engLate} → ${z.engLate} per minute`);
  say("nothing is dropped", fmt.reduce((s, r) => s + r.dropped, 0) === 0, `${fmt.reduce((s, r) => s + r.dropped, 0)} messages`);
  say("the process table does not grow", z.procRows <= Math.max(a.procRows * 1.5, a.procRows + 30),
    `${a.procRows} rows → ${z.procRows} rows${cycles ? ` over ${cycles} Stop/Run cycles` : ""}`);
  const busy = fmt.reduce((a, r) => Math.max(a, r.mainBusy), 0);
  say("the main thread is not saturated", busy < 0.8, `${(busy * 100).toFixed(0)}% busy at worst`);
  say("no page errors", errors.length === 0, errors.slice(0, 2).join(" | ") || "none");
}
fs.writeFileSync(process.env.SOAK_OUT ?? "/tmp/midi-soak.json", JSON.stringify({ base: BASE, minutes: MINUTES, rate: RATE, program: WHICH, rows, byMinute: fmt, errors }, null, 1));
await browser.close();
