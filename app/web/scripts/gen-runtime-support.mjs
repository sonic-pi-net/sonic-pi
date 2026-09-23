#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// Writes web/data/runtime-support.json: which documented Sonic Pi functions
// the mruby runtime does not define yet, asked of the built runtime itself
// (build/runtime, via runtime/bin/trace-wasm.mjs), and which the web build
// will never have. The docs mark both rather than hiding them.
//
//   node scripts/gen-runtime-support.mjs
import { execFileSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
// What the web build does not have, and why: "never" (a browser cannot), or "later" (not built yet). The runtime
// defines each only to say so (runtime/lib/sonic_pi/lang.rb, lang_more.rb); the docs and the completions label them.
const OSC = "a browser cannot send or receive OSC (it travels over UDP)";
const FILES = "there are no files on the web";
const WEB = {
  osc: ["never", OSC], osc_send: ["never", OSC], use_osc: ["never", OSC], with_osc: ["never", OSC],
  use_osc_logging: ["never", OSC], with_osc_logging: ["never", OSC],
  load_buffer: ["never", FILES], run_file: ["never", FILES], eval_file: ["never", FILES],
  link_audio: ["never", "Link Audio shares audio between apps over the local network, which a browser cannot join"],
  ...Object.fromEntries(["live_track", "use_track", "with_track", "current_track", "tracks", "with_send", "track_control", "track_midi",
    "track_midi_note_on", "track_midi_note_off", "track_midi_cc", "track_midi_pitch_bend", "track_midi_all_notes_off"]
    .map((fn) => [fn, ["never", "tracks host audio plugins (Surge and the like), which only the desktop app can run"]])),
  buffer: ["later", "named buffers are not built yet"],
  midi_sync: ["later", "following an incoming MIDI clock is not built yet"],
  midi_clock_sources: ["later", "following an incoming MIDI clock is not built yet"],
};
const NOT_ON_WEB = Object.keys(WEB);

// Pages of the reference that document something other than a function.
const DOC_ONLY = ["ring_and_list_methods"];

const lang = JSON.parse(fs.readFileSync(path.join(ROOT, "web/data/reference/lang.json"), "utf8")).pages.map((p) => p.key);
const probe = path.join(os.tmpdir(), `sp-support-${process.pid}.rb`);
fs.writeFileSync(probe, `names = ${JSON.stringify(lang)}\nputs names.reject { |n| respond_to?(n.to_sym, true) }.join(" ")\n`);
let out;
try {
  out = JSON.parse(execFileSync(process.execPath, [path.join(ROOT, "runtime/bin/trace-wasm.mjs"), probe], { encoding: "utf8", maxBuffer: 64 << 20 }));
} finally {
  fs.rmSync(probe, { force: true });
}
if (out.errors?.length || !out.output?.length) throw new Error(`the probe failed: ${JSON.stringify(out.errors ?? out)}`);
const missing = out.output[0].text.replace(/^"|"$/g, "").split(/\s+/).filter((n) => n && !NOT_ON_WEB.includes(n) && !DOC_ONLY.includes(n));
const result = { functions: lang.length, notOnWeb: NOT_ON_WEB.filter((n) => lang.includes(n)), why: Object.fromEntries(NOT_ON_WEB.filter((n) => lang.includes(n)).map((n) => [n, { kind: WEB[n][0], reason: WEB[n][1] }])), missing };
fs.writeFileSync(path.join(ROOT, "web/data/runtime-support.json"), JSON.stringify(result) + "\n");
console.log(`runtime support: ${lang.length - missing.length - result.notOnWeb.length} of ${lang.length} documented functions; ${missing.length} missing from the runtime; ${NOT_ON_WEB.filter((n) => WEB[n][0] === "never").length} never on the web, ${NOT_ON_WEB.filter((n) => WEB[n][0] === "later").length} not on the web yet`);
