#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// The MIDI path end to end, against a fake controller: the page asks the engine for MIDI, SuperSonic's
// MidiManager (clockwork/js/lib/midi_manager.js) parses and names it with the same Rust core the native app
// uses, and what comes out reaches the Time State and the Cues pane. Web MIDI needs no device here: the
// manager takes its access injected, and this hands it one.
//
//   node scripts/serve.mjs &
//   node scripts/midi-check.mjs [http://127.0.0.1:8460/web/]
//
// Needs a SuperSonic with Clockwork#enableMidi: MIDI comes up when the player asks, not at boot.
import { quiet } from "./lib/quiet.mjs";
const BASE = process.argv[2] ?? "http://127.0.0.1:8461/web/";
const { chromium, webkit } = await import(process.env.PLAYWRIGHT ?? "playwright");
let pass = 0, fail = 0;
const check = (name, ok, detail = "") => { console.log(`${ok ? "pass" : "FAIL"} ${name}${ok ? "" : ": " + detail}`); ok ? pass++ : fail++; };

const FAKE = () => {
  const mk = (id, name) => ({ id, name, onmidimessage: null, state: "connected", send() {} });
  const input = mk("in1", "Midi Fighter Twister");
  const output = mk("out1", "Midi Fighter Twister");
  output.sent = [];
  output.send = (bytes, at) => output.sent.push({ bytes: Array.from(bytes), at });
  const access = {
    inputs: new Map([["in1", input]]), outputs: new Map([["out1", output]]),
    onstatechange: null, sysexEnabled: false,
  };
  globalThis.__fakeMidi = { access, input, output };
  navigator.requestMIDIAccess = async () => access;
  try { localStorage.setItem("sp-midi", "true"); } catch {}
};

for (const [name, kind] of [["chromium", chromium], ["webkit", webkit]]) {
  let browser;
  try { browser = quiet(await kind.launch(name === "chromium" ? { args: ["--autoplay-policy=no-user-gesture-required", "--mute-audio"] } : {})); }
  catch (e) { console.log(`skip ${name}: ${e.message.split("\n")[0]}`); continue; }
  console.log(`\n── ${name} ──`);
  const page = await browser.newPage({ ignoreHTTPSErrors: BASE.startsWith("https:") });
  const errors = [];
  page.on("pageerror", (e) => errors.push(String(e)));
  page.on("console", (m) => { if (m.type() === "error") errors.push(m.text()); });
  await page.addInitScript(FAKE);
  await page.goto(BASE + "#app");
  await page.waitForFunction(() => /ready|Error/.test(document.getElementById("status-engine")?.textContent ?? ""), null, { timeout: 60000 });
  // The engine boots on the first Run, and the MIDI the player had on last time comes up with it.
  await page.evaluate(() => globalThis.sonicPi.editor.setCode("sleep 0"));
  await page.click("#btn-run");
  const up = await page.waitForFunction(() => globalThis.sonicPi.engine?.midi != null, null, { timeout: 40000 })
    .then(() => true).catch(() => false);
  check("the engine brings MIDI up on demand (enableMidi)", up,
    await page.evaluate(() => String(globalThis.sonicPi.engine?.midiError ?? "midi still null")).catch(() => "?"));
  if (up) {
    const ports = await page.evaluate(() => globalThis.sonicPi.engine.midi.portLists());
    check("the port's name is normalised as native does", ports.ins[0]?.[0] === "midi_fighter_twister", JSON.stringify(ports.ins));
    check("the input is open (ports are closed until opened)", ports.ins[0]?.[1] === true, JSON.stringify(ports.ins));
    // a knob: 20 control_change messages on channel 1
    await page.evaluate(async () => {
      for (let i = 0; i < 20; i++) {
        globalThis.__fakeMidi.input.onmidimessage({ data: new Uint8Array([0xb0, 5, 60 + i]), timeStamp: performance.now() });
        await new Promise((r) => setTimeout(r, 5));
      }
    });
    await page.waitForTimeout(600);
    const seen = await page.evaluate(() => [...document.querySelectorAll(".cue-row .cue-path")].map((n) => n.textContent));
    const want = "/midi:midi_fighter_twister:1/control_change";
    check("a knob reaches the Cues pane at the native address", seen.filter((s) => s === want).length >= 15,
      `${seen.filter((s) => s === want).length} of ${seen.length}: ${[...new Set(seen)].slice(0, 3)}`);
    const known = await page.evaluate((w) => globalThis.sonicPi.api.cueValue?.(w) ?? null, want);
    check("its value reaches the completion's Time State", known != null, String(known));

    // The values are the message's own, and only those: the moment it arrived rides along as a trailing
    // timetag (midi_in_osc_at) and decodes to a number like the rest, so it is easy to let it in by mistake.
    const row = await page.evaluate(() => [...document.querySelectorAll(".cue-row")]
      .map((n) => [n.querySelector(".cue-path")?.textContent, n.querySelector(".cue-data")?.textContent])
      .filter(([a]) => a?.startsWith("/midi:")).pop());
    check("a cue carries the message's values and nothing else", row && /^\[\s*5,\s*\d+\s*\]$/.test(row[1] ?? ""),
      JSON.stringify(row));

    // Out: a program's midi_note_on reaches the hardware through the manager's sink, which opens the port on
    // first use as it does natively, and carries the moment to send at for the browser to schedule.
    await page.evaluate(() => { globalThis.__fakeMidi.output.sent = []; globalThis.sonicPi.editor.setCode('midi_note_on 64, 100, port: "midi_fighter_twister"'); });
    await page.click("#btn-run");
    await page.waitForTimeout(1500);
    const sent = await page.evaluate(() => globalThis.__fakeMidi.output.sent);
    check("a program's MIDI reaches the port through the sink", sent.length > 0 && sent[0].bytes[0] === 0x90 && sent[0].bytes[1] === 64,
      JSON.stringify(sent.slice(0, 2)));
    check("the send carries a time for the browser to schedule", sent.length > 0 && typeof sent[0].at === "number" && sent[0].at > 0,
      JSON.stringify(sent[0] ?? null));
    const opened = await page.evaluate(() => globalThis.sonicPi.engine.midi.portLists().outs);
    check("the sink opened the output, as it does natively", opened[0]?.[1] === true, JSON.stringify(opened));

    // Hotplug: a controller plugged in mid-session is heard without asking. The page opens whatever appears,
    // and opening pushes the ports again — so this also says that answering a push by opening does not spin.
    const plugged = await page.evaluate(async () => {
      let pushes = 0;
      const m = globalThis.sonicPi.engine.midi;
      const was = m._onPorts;
      m.onPorts((...a) => { pushes++; return was?.(...a); });
      const f = globalThis.__fakeMidi;
      const two = { id: "in2", name: "Launchpad Mini MK3", state: "connected", onmidimessage: null, send() {} };
      f.access.inputs.set("in2", two);
      f.access.onstatechange?.({ port: two });
      await new Promise((r) => setTimeout(r, 400));
      return { pushes, ins: m.portLists().ins };
    });
    check("a controller plugged in mid-session is opened", plugged.ins.some(([n, on]) => n === "launchpad_mini_mk3" && on),
      JSON.stringify(plugged.ins));
    check("opening what appeared does not spin", plugged.pushes > 0 && plugged.pushes <= 4, `${plugged.pushes} ports pushes`);

    // The second device plays too, at its own address.
    await page.evaluate(() => {
      const two = globalThis.__fakeMidi.access.inputs.get("in2");
      for (let i = 0; i < 5; i++) two.onmidimessage({ data: new Uint8Array([0x91, 60 + i, 100]), timeStamp: performance.now() });
    });
    await page.waitForTimeout(500);
    const both = await page.evaluate(() => [...new Set([...document.querySelectorAll(".cue-row .cue-path")].map((n) => n.textContent))]);
    check("the second controller cues at its own address and channel",
      both.includes("/midi:launchpad_mini_mk3:2/note_on"), JSON.stringify(both.slice(0, 4)));
  }
  check("no page errors", errors.length === 0, errors.slice(0, 2).join(" | "));
  await browser.close();
}
console.log(`\n${pass} pass, ${fail} fail`);
process.exit(fail ? 1 : 0);
