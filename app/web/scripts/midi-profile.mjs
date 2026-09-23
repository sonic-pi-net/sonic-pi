#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// What the page's main thread spends a controller's messages on: a CPU profile taken while a fake device
// plays into a live loop, reported as self time per function.
import { quiet } from "./lib/quiet.mjs";
const BASE = process.argv.find((a) => a.startsWith("http")) ?? "https://127.0.0.1:8460/web/";
const opt = (n, d) => { const i = process.argv.indexOf(n); return i < 0 ? d : Number(process.argv[i + 1]); };
const SECONDS = opt("--seconds", 20);
const RATE = opt("--rate", 380);
if (!Number.isFinite(SECONDS) || !Number.isFinite(RATE)) { console.error("--seconds and --rate want numbers"); process.exit(2); }
const { chromium } = await import("playwright");
const browser = quiet(await chromium.launch({ args: ["--autoplay-policy=no-user-gesture-required", "--mute-audio", "--disable-background-timer-throttling", "--disable-renderer-backgrounding"] }));
const page = await browser.newPage({ ignoreHTTPSErrors: true });
await page.addInitScript(() => {
  const mk = (id, name) => ({ id, name, state: "connected", onmidimessage: null, send() {} });
  const input = mk("in1", "Midi Fighter Twister"), output = mk("out1", "Midi Fighter Twister");
  globalThis.__fakeMidi = { access: { inputs: new Map([["in1", input]]), outputs: new Map([["out1", output]]), onstatechange: null }, input, output, sent: 0 };
  navigator.requestMIDIAccess = async () => globalThis.__fakeMidi.access;
  try { localStorage.setItem("sp-midi", "true"); } catch {}
});
await page.goto(BASE + "#app");
await page.waitForFunction(() => /ready|Error/.test(document.getElementById("status-engine")?.textContent ?? ""), null, { timeout: 60000 });
await page.evaluate(() => globalThis.sonicPi.editor.setCode('live_loop :foo do\n  use_real_time\n  n, v = sync "/midi:midi_fighter_twister:1/control_change"\n  play v, release: 0.1\nend'));
await page.click("#btn-run");
await page.waitForFunction(() => globalThis.sonicPi.engine?.midi != null, null, { timeout: 60000 });
await page.evaluate((rate) => { const f = globalThis.__fakeMidi; const t0 = performance.now();
  const step = () => { const due = Math.floor(((performance.now()-t0)/1000)*rate);
    for (let i=f.sent;i<due;i++) f.input.onmidimessage({ data:new Uint8Array([0xb0,5,40+(i%60)]), timeStamp: performance.now() });
    f.sent = Math.max(f.sent,due); f.timer = setTimeout(step,2); }; step(); }, RATE);
await page.waitForTimeout(3000);

const cdp = await page.context().newCDPSession(page);
await cdp.send("Profiler.enable");
await cdp.send("Profiler.setSamplingInterval", { interval: 200 });
await cdp.send("Profiler.start");
await page.waitForTimeout(SECONDS * 1000);
const { profile } = await cdp.send("Profiler.stop");
const sent = await page.evaluate(() => globalThis.__fakeMidi.sent);
if (sent < RATE * SECONDS * 0.5) { console.error(`only ${sent} messages went in ${SECONDS}s: the profile is of an idle page`); process.exit(3); }

const byId = new Map(profile.nodes.map((n) => [n.id, n]));
const self = new Map();
const total = profile.samples.length;
for (const id of profile.samples) {
  const n = byId.get(id); if (!n) continue;
  const f = n.callFrame;
  const where = `${f.functionName || "(anonymous)"}  ${(f.url || "").split("/").pop()}:${f.lineNumber + 1}`;
  self.set(where, (self.get(where) ?? 0) + 1);
}
console.log(`main thread, ${SECONDS}s at ${RATE} msg/s — ${total} samples\n`);
console.log("  self%   where");
for (const [where, n] of [...self.entries()].sort((a, b) => b[1] - a[1]).slice(0, 22)) {
  console.log(`  ${((n / total) * 100).toFixed(1).padStart(5)}%  ${where}`);
}
await browser.close();
