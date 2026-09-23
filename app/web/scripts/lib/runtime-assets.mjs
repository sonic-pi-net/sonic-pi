// SPDX-License-Identifier: AGPL-3.0-or-later
// What the page needs to run the runtime in the browser: the wasm module,
// the five random tables, and the built-in samples described by their
// headers (the runtime never opens a file).
import fs from "node:fs";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { flacInfo } from "./flac-info.mjs";

export const TABLES = ["rand-stream.wav", "rand-stream-pink.wav", "rand-stream-light-pink.wav", "rand-stream-dark-pink.wav", "rand-stream-perlin.wav"];

export function samplesJSON(ROOT) {
  const dir = path.join(ROOT, "../../etc/samples");
  const out = [];
  for (const f of fs.readdirSync(dir).sort()) {
    if (!f.endsWith(".flac")) continue;
    const { rate, chans, frames } = flacInfo(fs.readFileSync(path.join(dir, f)));
    out.push({ file: f, frames, chans, rate });
  }
  return JSON.stringify(out);
}

// ── SuperSonic: from its released packages on the CDN, or the local build ──
// The page imports ./supersonic/supersonic.js and reads ./supersonic/version.json
// beside it, which says where the rest is (web/runtime.js). "local" is the
// dist/ that scripts/build-web.sh writes in a SuperSonic checkout (see
// SUPERSONIC_DISTS below, or SUPERSONIC_DIST) — for developing the engine.
// "cdn[@version]" is jsdelivr's copy of the npm packages: the client, the
// core (wasm and worklet), the synthdefs and the samples; a public host then
// serves none of SuperSonic's bytes, and a web deploy needs no engine build.
// The assets (synthdefs, samples: 35 MB that rarely change) can come from the
// CDN with a local engine.
export const SUPERSONIC_CDN = "https://cdn.jsdelivr.net/npm/";

// The standalone checkout first, then the submodule: engine work is done in a
// SuperSonic checkout beside this repository and the submodule is a pinned copy,
// so a local run should serve what is being worked on. SUPERSONIC_DIST wins over both.
const SUPERSONIC_DISTS = ["../../../supersonic/dist", "../external/supersonic/dist"];
const usableDist = (dist) => fs.existsSync(path.join(dist, "supersonic.js")) && fs.existsSync(path.join(dist, "wasm/scsynth-nrt.wasm"));

export function supersonicDist(ROOT) {
  if (process.env.SUPERSONIC_DIST) {
    const dist = path.resolve(process.env.SUPERSONIC_DIST);
    if (usableDist(dist)) return dist;
    throw new Error(`no SuperSonic build at ${dist} (SUPERSONIC_DIST): run scripts/build-web.sh in the SuperSonic checkout`);
  }
  for (const rel of SUPERSONIC_DISTS) {
    const dist = path.resolve(path.join(ROOT, rel));
    if (usableDist(dist)) return dist;
  }
  throw new Error(`no SuperSonic build in ${SUPERSONIC_DISTS.map((r) => path.resolve(path.join(ROOT, r))).join(" or ")}: run scripts/build-web.sh in the SuperSonic checkout, or set SUPERSONIC_DIST=<its dist/>`);
}

/** The checkout's version and commit. */
export function supersonicVersionOf(dist) {
  const src = path.dirname(dist);
  let version = null, commit = null;
  try { version = JSON.parse(fs.readFileSync(path.join(src, "package.json"), "utf8")).version; } catch {}
  try { commit = execFileSync("git", ["-C", src, "rev-parse", "--short", "HEAD"], { encoding: "utf8" }).trim(); } catch {}
  return { version, commit };
}

// The version the CDN is asked for when none is named: the one package.json pins (supersonicVersion), so a build
// of a commit is always the same engine and a new one arrives by a commit of its own; with no pin, npm's latest (the
// checkout's own may not be published), or offline the checkout's, or "latest".
async function cdnVersion(ROOT) {
  try { const pin = JSON.parse(fs.readFileSync(path.join(ROOT, "package.json"), "utf8")).supersonicVersion; if (pin) return pin; } catch {}
  try {
    const r = await fetch("https://registry.npmjs.org/supersonic-scsynth/latest", { signal: AbortSignal.timeout(5000) });
    if (r.ok) return (await r.json()).version;
  } catch {}
  try { return supersonicVersionOf(supersonicDist(ROOT)).version || "latest"; } catch { return "latest"; }
}

const cdnBases = (version) => ({
  base: `${SUPERSONIC_CDN}supersonic-scsynth@${version}/dist/`,
  core: `${SUPERSONIC_CDN}supersonic-scsynth-core@${version}/`,
  synthdefs: `${SUPERSONIC_CDN}supersonic-scsynth-synthdefs@${version}/synthdefs/`,
  samples: `${SUPERSONIC_CDN}supersonic-scsynth-samples@${version}/samples/`,
});

/**
 * Where SuperSonic comes from. want: "local", "cdn", "cdn@<version>", or "auto"
 * (local when a build is there, else the CDN); assets: the same for the
 * synthdefs and samples, or null to follow. Resolves to
 *   { source: "local", dist, version, commit, synthdefs?, samples? }
 *   { source: "cdn", version, base, core, synthdefs, samples }
 */
export async function resolveSupersonic(ROOT, want = "auto", assets = null) {
  const [kind, at] = String(want).split("@");
  let r;
  if (kind === "local" || (kind === "auto" && (() => { try { supersonicDist(ROOT); return true; } catch { return false; } })())) {
    const dist = supersonicDist(ROOT);
    r = { source: "local", dist, ...supersonicVersionOf(dist) };
  } else if (kind === "cdn" || kind === "auto") {
    const version = at || await cdnVersion(ROOT);
    r = { source: "cdn", version, ...cdnBases(version) };
  } else {
    throw new Error(`SuperSonic from where? "${want}" is not local, cdn or cdn@<version>`);
  }
  if (assets) {
    const [akind, aat] = String(assets).split("@");
    if (akind === "cdn") { const { synthdefs, samples } = cdnBases(aat || r.version || await cdnVersion(ROOT)); Object.assign(r, { synthdefs, samples }); }
    else if (akind === "local") { if (r.source === "cdn") throw new Error("local assets need the local build"); delete r.synthdefs; delete r.samples; }
    else throw new Error(`assets from where? "${assets}" is not local, cdn or cdn@<version>`);
  }
  return r;
}

/** A line saying what was resolved. */
export const describeSupersonic = (r) => r.source === "local"
  ? `SuperSonic: local build ${r.version ?? "?"}${r.commit ? ` @ ${r.commit}` : ""} at ${r.dist}${r.samples ? " (synthdefs and samples from the CDN)" : ""}`
  : `SuperSonic: ${r.version} from the CDN (${r.base})`;

/** version.json for the page: the source, its version, and the bases it does not derive itself. */
export function supersonicVersionJSON(r) {
  const { source, version, commit, base, core, synthdefs, samples } = r;
  const built = r.dist ? fs.statSync(path.join(r.dist, "supersonic.js")).mtime.toISOString() : undefined;
  return JSON.stringify({ source, version, commit, built, base, core, synthdefs, samples });
}

/** The modules the page and its workers import by path (supersonic/<name>): on the CDN, re-exported from there.
 *  A module missing here is a 404 on the CDN build only, where the local dist serves everything: a new import
 *  of SuperSonic's (live-worker.js) is added here too. */
const SHIMMED = ["supersonic.js", "osc_channel.js", "osc_in_pump.js", "midi_event.js", "metrics_component.js"];
export const supersonicShims = (r) => (r.source === "cdn"
  ? Object.fromEntries(SHIMMED.map((name) => [name, `export * from "${r.base}${name}";\n`]))
  : {});

/** The option value after a flag, "" when the flag stands alone, null when absent. */
export const flagValue = (argv, name) => { const i = argv.indexOf(name); return i < 0 ? null : (argv[i + 1] && !argv[i + 1].startsWith("--") ? argv[i + 1] : ""); };

/**
 * A random table with its repeats taken off. Sonic Pi's :white table is one second of numbers written ten times
 * over — 861 kB where 86 kB says the same thing — and a stream indexes 0...441000 whatever the table holds, since
 * Rand::Table wraps on its own length. So the browser is sent one copy of whatever repeats, and reads the same
 * number at every index it ever asks for. A table that does not repeat (the pink ones, perlin) is sent whole.
 */
export function trimTable(wav) {
  let pos = 12, data = null;
  while (pos + 8 <= wav.length) {                       // find the data chunk, skipping any others
    const id = wav.toString("latin1", pos, pos + 4), size = wav.readUInt32LE(pos + 4);
    if (id === "data") { data = wav.subarray(pos + 8, pos + 8 + size); break; }
    pos += 8 + size + (size & 1);
  }
  if (!data) return wav;
  const samples = data.length >> 1;
  let period = samples;
  for (let p = 1; p * p <= samples; p++) {              // the smallest block the rest is made of
    if (samples % p) continue;
    for (const cand of [p, samples / p]) {
      if (cand >= period || cand < 1024) continue;
      const block = data.subarray(0, cand * 2);
      let same = true;
      for (let at = cand * 2; at < data.length && same; at += cand * 2) same = block.equals(data.subarray(at, at + cand * 2));
      if (same) period = cand;
    }
  }
  if (period === samples) return wav;
  const head = Buffer.alloc(44);                        // a plain 16-bit mono header, which is all these are
  head.write("RIFF", 0); head.writeUInt32LE(36 + period * 2, 4); head.write("WAVEfmt ", 8);
  head.writeUInt32LE(16, 16); head.writeUInt16LE(1, 20); head.writeUInt16LE(1, 22);
  head.writeUInt32LE(44100, 24); head.writeUInt32LE(88200, 28); head.writeUInt16LE(2, 32); head.writeUInt16LE(16, 34);
  head.write("data", 36); head.writeUInt32LE(period * 2, 40);
  return Buffer.concat([head, data.subarray(0, period * 2)]);
}

/** Sonic Pi's own synthdefs (etc/synthdefs/compiled) into the build, as web/sonic_pi.js asks for them: the web plays
 *  the same build of each synth as the desktop app, rather than the set SuperSonic bundles (which drifts). */
export function copySynthdefs(ROOT, OUT) {
  const from = path.resolve(ROOT, "../../etc/synthdefs/compiled");
  if (!fs.existsSync(from)) return 0;
  const names = fs.readdirSync(from).filter((f) => f.endsWith(".scsyndef"));
  fs.mkdirSync(path.join(OUT, "synthdefs"), { recursive: true });
  for (const f of names) fs.copyFileSync(path.join(from, f), path.join(OUT, "synthdefs", f));
  return names.length;
}

export function copyRuntime(ROOT, OUT) {
  const built = path.join(ROOT, "build/runtime");
  if (!fs.existsSync(path.join(built, "sp_runtime.wasm"))) return false;
  fs.mkdirSync(path.join(OUT, "runtime"), { recursive: true });
  for (const f of ["sp_runtime.mjs", "sp_runtime.wasm"]) fs.copyFileSync(path.join(built, f), path.join(OUT, "runtime", f));
  fs.mkdirSync(path.join(OUT, "buffers"), { recursive: true });
  for (const f of TABLES) fs.writeFileSync(path.join(OUT, "buffers", f), trimTable(fs.readFileSync(path.join(ROOT, "../../etc/buffers", f))));
  fs.writeFileSync(path.join(OUT, "samples.json"), samplesJSON(ROOT));
  return true;
}
