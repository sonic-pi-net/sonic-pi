#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// Where the page's nodes are, over time: which container grows while a controller plays into a live loop.
import { quiet } from "./lib/quiet.mjs";
const BASE = process.argv.find((a) => a.startsWith("http")) ?? "https://127.0.0.1:8460/web/";
const MINUTES = Number(process.argv[process.argv.indexOf("--minutes") + 1] || 4);
const { chromium } = await import("playwright");
const browser = quiet(await chromium.launch({ args: ["--autoplay-policy=no-user-gesture-required", "--mute-audio", "--disable-background-timer-throttling", "--disable-renderer-backgrounding"] }));
const page = await browser.newPage({ ignoreHTTPSErrors: true });
await page.addInitScript(() => {
  const mk = (id, name) => ({ id, name, state: "connected", onmidimessage: null, send() {} });
  const input = mk("in1", "Midi Fighter Twister"), output = mk("out1", "Midi Fighter Twister");
  const access = { inputs: new Map([["in1", input]]), outputs: new Map([["out1", output]]), onstatechange: null };
  globalThis.__fakeMidi = { access, input, output, sent: 0 };
  navigator.requestMIDIAccess = async () => access;
  try { localStorage.setItem("sp-midi", "true"); } catch {}
});
await page.goto(BASE + "#app");
await page.waitForFunction(() => /ready|Error/.test(document.getElementById("status-engine")?.textContent ?? ""), null, { timeout: 60000 });
await page.evaluate(() => globalThis.sonicPi.editor.setCode('live_loop :foo do\n  use_real_time\n  n, v = sync "/midi:midi_fighter_twister:1/control_change"\n  puts [n, v]\n  play v, release: 0.1\nend'));
await page.click("#btn-run");
await page.waitForFunction(() => globalThis.sonicPi.engine?.midi != null, null, { timeout: 60000 });
await page.evaluate(() => {
  const f = globalThis.__fakeMidi; f.sent = 0;
  const t0 = performance.now();
  const step = () => {
    const due = Math.floor(((performance.now() - t0) / 1000) * 380);
    for (let i = f.sent; i < due; i++) f.input.onmidimessage({ data: new Uint8Array([0xb0, 5, 40 + (i % 60)]), timeStamp: performance.now() });
    f.sent = Math.max(f.sent, due);
    f.timer = setTimeout(step, 2);
  };
  step();
});
const census = () => page.evaluate(() => {
  const counts = {};
  // every element with an id, and how much of the document hangs off it
  for (const el of document.querySelectorAll("[id]")) {
    const n = el.getElementsByTagName("*").length;
    if (n > 200) counts[el.id] = n;
  }
  counts["__total"] = document.getElementsByTagName("*").length;
  counts["__detached_rows"] = document.querySelectorAll(".cue-row").length;
  return counts;
});
const snaps = [];
for (let i = 0; i * 30 < MINUTES * 60; i++) {
  snaps.push({ t: i * 30, ...(await census()) });
  await page.waitForTimeout(30000);
  if (i % 2 === 1) { await page.click("#btn-stop").catch(() => {}); await page.waitForTimeout(300); await page.click("#btn-run").catch(() => {}); }
}
const keys = [...new Set(snaps.flatMap((s) => Object.keys(s)))].filter((k) => k !== "t");
console.log(["t(s)", ...keys].join("\t"));
for (const s of snaps) console.log([s.t, ...keys.map((k) => s[k] ?? 0)].join("\t"));
console.log("\ngrowth over the run:");
for (const k of keys) {
  const a = snaps[0][k] ?? 0, z = snaps[snaps.length - 1][k] ?? 0;
  if (z - a > 100) console.log(`  ${k}: ${a} → ${z}  (+${z - a})`);
}
await browser.close();
