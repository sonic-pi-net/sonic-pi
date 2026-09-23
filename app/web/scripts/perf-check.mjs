#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// A repeatable load test. Two passes:
//
//   runtime  the wasm runtime alone under Node, on a simulated clock: how many
//            milliseconds of CPU a second of each program costs, and the
//            longest single tick. The language's own headroom.
//   browser  the app in headless Chromium against a running dev server: each
//            program played for a while, with the Threads view off and on,
//            measured in its steady state and, separately, across its Run and
//            Stop (late bundles as a program starts or stops),
//            read back from the flight recorder: the headroom sounds left
//            with, sounds sent late, bundles the engine played late, drops,
//            tick cost, stalls, and what each view cost to draw. Headless
//            Chromium has no audio device, so dropouts heard (glitches) are
//            not measured here; everything that causes them is.
//
// Each browser run's full flight report is written to build/perf/.
//
//   node scripts/serve.mjs &
//   node scripts/perf-check.mjs [--seconds 20] [--base http://127.0.0.1:8460/web/] [--only runtime|browser] [--strict]
//
// --strict exits 1 when any sound was sent late, the engine played a bundle
// late, or a message was dropped.
import { quiet } from "./lib/quiet.mjs";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { flacInfo } from "./lib/flac-info.mjs";
import { decode, forEachFrame, FRAME_GUI } from "../web/osc.js";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const args = process.argv.slice(2);
const opt = (name, fallback) => { const i = args.indexOf(name); return i >= 0 ? args[i + 1] : fallback; };
const SECONDS = Number(opt("--seconds", 20));
const BASE = opt("--base", "http://127.0.0.1:8460/web/");
const ONLY = opt("--only", null);
const STRICT = args.includes("--strict");
const OUT = path.join(ROOT, "build/perf");

const PROGRAMS = {
  "two loops": `live_loop :kick do
  sample :bd_haus
  sleep 0.5
end
live_loop :hat do
  sample :drum_cymbal_closed, amp: 0.5
  sleep 0.25
end`,
  busy: `use_bpm 120
live_loop :kick do
  sample :bd_haus, amp: 1.5
  sleep 1
end
live_loop :snare do
  sleep 1
  sample :sn_dolf
  sleep 1
end
live_loop :hats do
  sample :drum_cymbal_closed, amp: rrand(0.2, 0.6), rate: rrand(0.9, 1.1)
  sleep 0.25
end
live_loop :acid do
  use_synth :tb303
  play (scale :e2, :minor_pentatonic).choose, release: 0.2, cutoff: rrand(60, 120), res: 0.8
  sleep 0.25
end
live_loop :chords do
  use_synth :prophet
  play chord(:e3, :minor7).shuffle.take(3), release: 1.5, amp: 0.4
  sleep 2
end
live_loop :bells do
  sync :tick
  use_synth :pretty_bell
  play (scale :e5, :minor).choose, release: 0.3, amp: 0.3
end
live_loop :clock do
  cue :tick
  sleep 0.5
end`,
  dense: Array.from({ length: 12 }, (_, i) => `live_loop :l${i} do
  use_synth :${["beep", "saw", "tb303", "pluck"][i % 4]}
  play rrand_i(40, 90), release: 0.05, amp: 0.1, cutoff: rrand(60, 110)
  sleep 0.125
end`).join("\n") + `
live_loop :drums do
  sample :bd_haus
  sample :drum_cymbal_closed, amp: 0.3
  sleep 0.125
end`,
};

const SCENARIOS = [
  { name: "two loops", program: "two loops", view: null },
  { name: "busy", program: "busy", view: null },
  { name: "busy, Threads: processes", program: "busy", view: "processes" },
  { name: "busy, Threads: timeline", program: "busy", view: "timeline" },
  { name: "busy, Threads: piano roll", program: "busy", view: "roll" },
  { name: "dense", program: "dense", view: null },
  { name: "dense, Threads: processes", program: "dense", view: "processes" },
];

const fmt = (v, digits = 1) => (v == null ? "–" : typeof v === "number" ? (Number.isInteger(v) ? String(v) : v.toFixed(digits)) : String(v));
function table(headers, rows) {
  const widths = headers.map((h, i) => Math.max(h.length, ...rows.map((r) => String(r[i]).length)));
  const line = (cells) => cells.map((c, i) => String(c).padEnd(widths[i])).join("  ");
  console.log(line(headers));
  console.log(widths.map((w) => "-".repeat(w)).join("  "));
  for (const r of rows) console.log(line(r));
}

// ── Runtime ─────────────────────────────────────────────────────────────────

async function runtimePass() {
  const { default: load } = await import(path.join(ROOT, "build/runtime/sp_runtime.mjs"));
  const m = await load();
  if (m._sp_init() !== 0) throw new Error("the runtime did not boot");
  const TABLES = { white: "rand-stream.wav", pink: "rand-stream-pink.wav", light_pink: "rand-stream-light-pink.wav", dark_pink: "rand-stream-dark-pink.wav", perlin: "rand-stream-perlin.wav" };
  for (const [source, file] of Object.entries(TABLES)) {
    const bytes = fs.readFileSync(path.join(ROOT, "../../etc/buffers", file));
    const ptr = m._malloc(bytes.length);
    m.HEAPU8.set(bytes, ptr);
    m.ccall("sp_install_table", "number", ["string", "number", "number"], [source, ptr, bytes.length]);
    m._free(ptr);
  }
  const samplesDir = path.join(ROOT, "../../etc/samples");
  m.ccall("sp_set_samples_dir", "number", ["string"], [samplesDir]);
  for (const f of fs.readdirSync(samplesDir).sort()) {
    if (!f.endsWith(".flac")) continue;
    const { rate, chans, frames } = flacInfo(fs.readFileSync(path.join(samplesDir, f)));
    m.ccall("sp_install_sample", "number", ["string", "number", "number", "number", "number", "number"], [path.join(samplesDir, f), frames, chans, rate, 0, 0]);
  }
  const MUSIC = 30;
  const rows = [];
  for (const [name, code] of Object.entries(PROGRAMS)) {
    let records = 0, errors = 0;
    // what each call left in the outbox: its records, and any error among them
    const count = () => {
      const len = m._sp_out_len();
      if (len) forEachFrame(m.HEAPU8, m._sp_out_ptr(), len, (kind, start, size) => {
        if (kind !== FRAME_GUI) return;
        records++;
        if (decode(m.HEAPU8.subarray(start, start + size))[0] === "/sonic-pi/error") errors++;
      });
    };
    m._sp_live_boot();
    let now = 100;
    m.ccall("sp_run", "number", ["string", "number"], [code, now]);
    count();
    let cpu = 0, worst = 0, ticks = 0;
    while (now < 100 + MUSIC) {
      const t0 = performance.now();
      const next = m._sp_tick(now);
      const ms = performance.now() - t0;
      count();
      cpu += ms;
      worst = Math.max(worst, ms);
      ticks++;
      if (next < 0) break;
      now = Math.max(now, next);
    }
    m._sp_stop_all();
    rows.push([name, fmt(cpu / MUSIC, 2), fmt(worst, 2), fmt(records / MUSIC, 0), ticks, errors]);
  }
  console.log(`\nruntime: ${MUSIC} s of music each, on a simulated clock (no audio, no page)\n`);
  table(["program", "cpu ms per s", "worst tick ms", "records/s", "ticks", "errors"], rows);
}

// ── Browser ─────────────────────────────────────────────────────────────────

async function browserPass() {
  const { chromium } = await import(process.env.PLAYWRIGHT ?? "playwright");
  fs.mkdirSync(OUT, { recursive: true });
  const browser = quiet(await chromium.launch({ args: ["--autoplay-policy=no-user-gesture-required", "--mute-audio"] }));
  const page = await browser.newPage({ viewport: { width: 1400, height: 900 } });
  const pageErrors = [];
  page.on("pageerror", (e) => pageErrors.push(e.message));
  await page.goto(BASE);
  await page.evaluate(() => localStorage.clear());
  await page.goto(BASE);
  await page.waitForFunction(() => /ready/.test(document.getElementById("status-engine").textContent), null, { timeout: 60000 });
  const rows = [];
  let failed = false;
  for (const sc of SCENARIOS) {
    await page.evaluate((view) => {
      const drawer = document.body.dataset.drawer;
      // as native: Help opens the help pane and its rail picks threads; Help again closes it
      if (view && drawer !== "insight") {
        if (!drawer) document.getElementById("btn-help").click();
        document.querySelector("#drawer-rail button[data-drawer='insight']").click();
      }
      if (!view && drawer === "insight") document.getElementById("btn-help").click();
      if (view) document.querySelector(`.insight-views button[data-view='${view}']`).click();
    }, sc.view);
    await page.evaluate((code) => window.sonicPi.editor.setCode(code), PROGRAMS[sc.program]);
    const started = await page.evaluate(() => performance.now());
    // DOM clicks: under load a page is slow to be "actionable", and that is the data, not a reason to stop
    await page.evaluate(() => document.getElementById("btn-run").click());
    await page.waitForFunction(() => /running/.test(document.getElementById("status-engine").textContent), null, { timeout: 90000 });
    await page.waitForTimeout(1500);                                   // past the first loads
    const since = await page.evaluate(() => performance.now());
    await page.waitForTimeout(SECONDS * 1000);
    const summary = await page.evaluate((t) => window.sonicPi.flight.summary(t), since);
    const beforeStop = Date.now();
    await page.evaluate(() => document.getElementById("btn-stop").click());
    const stopMs = Date.now() - beforeStop;
    await page.waitForTimeout(1200);
    // the whole run, from the click on Run to after Stop: what starting and stopping cost
    const whole = await page.evaluate((t) => window.sonicPi.flight.summary(t), started);
    const report = await page.evaluate(() => window.sonicPi.flight.report());
    const file = path.join(OUT, `${sc.name.replace(/[^a-z0-9]+/gi, "-").toLowerCase()}.json`);
    fs.writeFileSync(file, JSON.stringify({ scenario: sc, summary, whole, report }));
    const edges = whole.engineLates - summary.engineLates;
    const gui = (k) => (summary.gui[k] ? fmt(summary.gui[k].maxMs) : "–");
    rows.push([sc.name, fmt(summary.sounds / summary.seconds, 0), `${fmt(summary.headroomMinMs, 0)} / ${fmt(summary.headroomAvgMs, 0)}`, summary.sentLate, summary.tight,
      summary.engineLates, `${edges}${edges ? ` (max ${fmt(whole.engineMaxLateMs, 0)})` : ""}`, summary.dropped + summary.gaps, fmt(summary.glitches),
      `${fmt(summary.tickMsMax)} / ${fmt(summary.tickMsAvg, 2)}`, fmt(summary.wakeLateMsMax, 0), `${summary.longTasks} (${fmt(summary.longTaskMsMax, 0)})`, fmt(summary.frameGapMsMax, 0),
      `${gui("processTree")} / ${gui("timeline")} / ${gui("pianoRoll")} / ${gui("scope")}`]);
    if (summary.sentLate > 0 || summary.engineLates > 0 || summary.dropped > 0 || edges > 0) failed = true;
    console.log(`  ${sc.name}: ${summary.sounds} sounds, headroom min ${fmt(summary.headroomMinMs, 0)} ms, late ${summary.engineLates} steady / ${edges} at run and stop, Stop answered in ${stopMs} ms`);
  }
  await browser.close();
  console.log(`\nbrowser: ${SECONDS} s each in headless Chromium (no audio device: glitches not measured); reports in ${path.relative(ROOT, OUT)}/\n`);
  table(["scenario", "sounds/s", "headroom min/avg ms", "sent late", "tight", "engine late", "late at run/stop", "dropped", "glitches", "tick max/avg ms", "wake late ms", "long tasks (max ms)", "frame gap ms", "draw max ms tree/timeline/roll/scope"], rows);
  if (pageErrors.length) console.log(`\npage errors: ${pageErrors.slice(0, 3).join(" | ")}`);
  return failed;
}

let failed = false;
if (ONLY !== "browser") await runtimePass();
if (ONLY !== "runtime") failed = await browserPass();
if (STRICT && failed) {
  console.log("\nstrict: sounds were late or dropped");
  process.exit(1);
}
