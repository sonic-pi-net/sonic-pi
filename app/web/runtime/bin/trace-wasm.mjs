#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// The wasm runtime as an adapter: the same program, traced by the module
// the browser will load, run here under Node.
//
//   node runtime/bin/trace-wasm.mjs specs/random/rrand.rb
//   ADAPTER="node runtime/bin/trace-wasm.mjs" ruby scripts/check.rb specs/random
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../..");
const { default: load } = await import(path.join(ROOT, "build/runtime/sp_runtime.mjs"));
const m = await load();
if (m._sp_init() !== 0) { console.error("the runtime did not boot"); process.exit(1); }

const TABLES = { white: "rand-stream.wav", pink: "rand-stream-pink.wav", light_pink: "rand-stream-light-pink.wav",
                 dark_pink: "rand-stream-dark-pink.wav", perlin: "rand-stream-perlin.wav" };
for (const [source, file] of Object.entries(TABLES)) {
  const bytes = fs.readFileSync(path.join(ROOT, "../../etc/buffers", file));
  const ptr = m._malloc(bytes.length);
  m.HEAPU8.set(bytes, ptr);
  if (m.ccall("sp_install_table", "number", ["string", "number", "number"], [source, ptr, bytes.length]) !== 0) process.exit(1);
  m._free(ptr);
}

// The built-in samples: the oracle's folder, described by their headers.
const samplesDir = path.join(ROOT, "../../etc/samples");
m.ccall("sp_set_samples_dir", "number", ["string"], [samplesDir]);
function flacInfo(buf) {                      // STREAMINFO: rate 20 | chans-1 3 | bps-1 5 | total samples 36
  const len = buf.readUIntBE(5, 3), info = buf.subarray(8, 8 + len);
  const hi = info.readUInt32BE(10), lo = info.readUInt32BE(14);
  return { rate: hi >>> 12, chans: ((hi >>> 9) & 7) + 1, frames: ((hi & 0xf) * 4294967296) + lo };
}
for (const f of fs.readdirSync(samplesDir).sort()) {
  if (!f.endsWith(".flac")) continue;
  const { rate, chans, frames } = flacInfo(fs.readFileSync(path.join(samplesDir, f)));
  m.ccall("sp_install_sample", "number", ["string", "number", "number", "number", "number", "number"], [path.join(samplesDir, f), frames, chans, rate, 0, 0]);
}

const file = process.argv[2];
const code = fs.readFileSync(file, "utf8");
process.stdout.write(m.ccall("sp_trace", "string", ["string", "string"], [code, file]) + "\n");
