// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
/**
 * The mruby runtime (the language, as wasm): loading it with its random
 * tables and the built-in samples' facts, and what a program will need from
 * the engine before it runs. Nothing here touches SuperSonic or the page, so
 * a worker loads it as the page does (live-worker.js).
 */

// SuperSonic is supersonic/ beside this page: the client module the page
// imports, and version.json, which says where the rest is — the local build
// of the vendored source (the dev server maps it there, a build copies it),
// or its released packages on the CDN, which supersonic.js then re-exports
// and version.json names as base, core, synthdefs and samples
// (scripts/lib/runtime-assets.mjs). SUPERSONIC_VERSION says which once
// supersonicInfo() has read it.
export const SUPERSONIC_BASE = new URL("./supersonic/", import.meta.url).href;
export let SUPERSONIC_VERSION = "…";
let info = null;
/** version.json, read once: { source, version, commit?, built?, base?, core?, synthdefs?, samples? } (empty when there is none). */
export function supersonicInfo() {
  return (info ??= fetch(`${SUPERSONIC_BASE}version.json`).then((r) => r.json()).catch(() => ({})).then((v) => {
    SUPERSONIC_VERSION = `${v.version ?? "?"}${v.commit ? ` @ ${v.commit}` : ""} (${v.source === "cdn" ? "CDN" : "local build"})`;
    return v;
  }));
}
export const supersonicVersion = () => supersonicInfo().then(() => SUPERSONIC_VERSION);
export const SAMPLES_DIR = "/samples";
const TABLES = { white: "rand-stream.wav", pink: "rand-stream-pink.wav", light_pink: "rand-stream-light-pink.wav",
                 dark_pink: "rand-stream-dark-pink.wav", perlin: "rand-stream-perlin.wav" };
const PLAYERS = ["basic_stereo_player", "basic_mono_player", "stereo_player", "mono_player"];

/**
 * The process table's row: what the runtime keeps about each thread.
 * kind: 0 run, 1 a run's main thread, 2 live_loop, 3 named thread, 4 thread.
 * state: 0 running, 1 sleeping, 2 waiting on sync, 3 done, 4 error, 5 stopped.
 * wake, active and ended are on the engine's clock (-1 for none).
 * Beside the threads: kind 6 a with_fx block (state 0 running, 1 its block
 * ended and waiting on its threads and sounds, 3 freed; events the threads it
 * waits on), 7 a synth, 8 a sample (0 sounding, 3 ended). Each hangs from the
 * block or thread it was made in; node is its synth's, for its name.
 */
export const PROCESS_FIELDS = ["uid", "parent", "job", "kind", "state", "line", "wake", "beat", "bpm", "active", "events", "redefs", "ended", "node", "group"];   // group: the run's (Scheduler#stop_group)

/**
 * The mruby runtime: its wasm, the random tables and the built-in samples' facts.
 *
 * The tables are 840 kB each and there are five of them, which is most of what a first visit weighs. `sources`
 * says which to install now: all of them by default, for a harness that runs anything; the live runtime asks for
 * none and installs :white when the engine boots, then whichever else a program names before it runs
 * (installTable, programNeeds). Nobody waits on a table they never draw from.
 */
export async function loadRuntime(base = "./", { sources = Object.keys(TABLES) } = {}) {
  const root = new URL(base, location.href);
  const { default: load } = await import(new URL("runtime/sp_runtime.mjs", root).href);
  const m = await load();
  if (m._sp_init() !== 0) throw new Error("the runtime did not boot");
  const installed = new Map();          // source -> the promise that put it there, so it is fetched once
  const installTable = (source) => {
    const file = TABLES[source];
    if (!file) throw new Error(`no such random source: ${source}`);
    if (!installed.has(source)) installed.set(source, (async () => {
      const bytes = new Uint8Array(await (await fetch(new URL(`buffers/${file}`, root))).arrayBuffer());
      const ptr = m._malloc(bytes.length);
      m.HEAPU8.set(bytes, ptr);
      const ok = m.ccall("sp_install_table", "number", ["string", "number", "number"], [source, ptr, bytes.length]) === 0;
      m._free(ptr);
      if (!ok) throw new Error(`random table ${source}`);
    })());
    return installed.get(source);
  };
  await Promise.all(sources.map(installTable));
  m.ccall("sp_set_samples_dir", "number", ["string"], [SAMPLES_DIR]);
  const samples = await (await fetch(new URL("samples.json", root))).json();
  for (const s of samples) {
    m.ccall("sp_install_sample", "number", ["string", "number", "number", "number", "number", "number"], [`${SAMPLES_DIR}/${s.file}`, s.frames, s.chans, s.rate, 0, 0]);
  }
  return {
    module: m,
    version: m.ccall("sp_version", "string", [], []),
    samples: samples.map((s) => s.file),
    /** A random table, fetched and installed once. The live runtime calls this as it needs them. */
    installTable,
    /** A loaded synthdef's metadata (the .json beside its .scsyndef), installed so the synth plays and is checked as a
     *  built-in is; with none, what its .scsyndef says of its controls, for the GUI's dials alone. Fetched before each
     *  run that loads it (the browser's cache keeps that cheap), so an edited file is seen on the next Run.
     *  → { url, name, meta?, derived?, error?, text? } */
    async installSynthMeta(url) {
      const name = synthdefFileName(url);
      let text = null;
      try { const r = await fetch(sidecarOf(url), { cache: "no-cache" }); if (r.ok) text = await r.text(); } catch {}
      if (text == null) {
        try { return { url, name, derived: parseSynthdefControls(new Uint8Array(await (await fetch(url)).arrayBuffer())) }; }
        catch { return { url, name }; }
      }
      let meta;
      try { meta = JSON.parse(text); } catch (e) { return { url, name, error: `${sidecarName(url)} is not JSON: ${e.message}` }; }
      if (meta?.name !== name) return { url, name, error: `${sidecarName(url)} names its synth ${JSON.stringify(meta?.name)}, but its synthdef is ${name}.scsyndef: they must match ("name": "${name}")` };
      const said = m.ccall("sp_install_synth", "string", ["string"], [text]);
      return said ? { url, name, error: `${sidecarName(url)}: ${said}` } : { url, name, meta, text };
    },
    /** The synthdef a built-in synth (or fx) plays, by the name a program calls it: "" for one that is not built in. */
    synthdefFor: (name, fx = false) => m.ccall("sp_synthdef_for", "string", ["string", "number"], [name, fx ? 1 : 0]),
    /** NRT: the program's whole schedule, as a trace. */
    trace(code, file = "buffer") {
      const t0 = performance.now();
      const out = JSON.parse(m.ccall("sp_trace", "string", ["string", "string"], [code, file]));
      if (out.host_error) throw new Error(out.host_error);
      return { trace: out, ms: performance.now() - t0 };
    },
  };
}

// ── A user's synths: the .scsyndef a program loads, and the metadata beside it ──

/** The .scsyndef URLs a program loads (load_synthdef, load_synthdefs), as written in it. */
export function programSynthdefUrls(code) {
  if (!/\bload_synthdefs?\b/.test(code)) return [];
  return [...new Set([...code.matchAll(/(["'])((?:https?:)?\/\/[^"'\s]+?\.scsyndef(?:[?#][^"'\s]*)?)\1/gi)].map((m) => m[2]))];
}
const synthdefFileName = (url) => (String(url).split(/[?#]/, 1)[0].split("/").pop() || "").replace(/\.scsyndef$/i, "");
/** Where a synthdef's metadata lives: beside it, the same name, .json for .scsyndef. */
export const sidecarOf = (url) => String(url).replace(/\.scsyndef(?=[?#]|$)/i, ".json");
const sidecarName = (url) => `${synthdefFileName(url)}.json`;

/** What a .scsyndef says of its synth's controls (SuperCollider's SCgf format, versions 1 and 2): its name and each
 *  control's name and default, for a GUI to make dials of when there is no metadata to say more. */
export function parseSynthdefControls(bytes) {
  const v = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  if (String.fromCharCode(...bytes.subarray(0, 4)) !== "SCgf") throw new Error("not a synthdef");
  const version = v.getInt32(4);
  let p = 10;   // after the magic, the version and the number of defs
  const int = () => { const x = version >= 2 ? v.getInt32(p) : v.getInt16(p); p += version >= 2 ? 4 : 2; return x; };
  const pstr = () => { const n = v.getUint8(p); const t = new TextDecoder().decode(bytes.subarray(p + 1, p + 1 + n)); p += 1 + n; return t; };
  const name = pstr();
  const nConstants = int();                    // (read first: p += 4 * int() would read p before int() moves it)
  p += 4 * nConstants;                         // the constants
  const nParams = int();
  const values = [];
  for (let i = 0; i < nParams; i++, p += 4) values.push(v.getFloat32(p));
  const nNames = int();
  const controls = [];
  for (let i = 0; i < nNames; i++) { const n = pstr(); const at = int(); controls.push({ name: n, default: Math.round(values[at] * 1e6) / 1e6 }); }
  return { name, controls };
}

/**
 * The random tables a program will draw from, beyond :white, which is always there. A source named as a symbol is
 * taken as read; a program that changes source some other way (a variable, a choose) gets all of them, since
 * guessing wrong means the run stops on a table that is not installed.
 */
export function programTables(code) {
  if (!/\b(?:use_random_source|with_random_source)\b/.test(code)) return [];
  const named = [...code.matchAll(/\b(?:use_random_source|with_random_source)\s*\(?\s*:([a-z_]+)/g)].map((m) => m[1]);
  const uses = (code.match(/\b(?:use_random_source|with_random_source)\b/g) ?? []).length;
  const wanted = named.length === uses ? named : Object.keys(TABLES);
  return wanted.filter((s) => s in TABLES && s !== "white");
}

/**
 * What a program is likely to need loaded before it runs: a sample loads on
 * first use otherwise, half a second before its trigger, and a slow fetch
 * would miss the first hit. Any `:name` that is a built-in sample, the synths
 * named after use_synth/with_synth/synth, the fx a with_fx names, the default
 * synth and the studio's mixers (which every sound plays through), a live
 * loop's scope, and the sample players when the program samples at all.
 * @returns {{synthdefs: string[], samples: string[], tables: string[]}} synthdef names, sample files and random tables
 */
// synthdefFor(name, fx) is the runtime's (loadRuntime): the synthdef each name plays, as the runtime will ask for it
// — an alias plays another's (:sine is sonic-pi-beep), and a name that is not built in is the program's own to load.
export function programNeeds(code, sampleFiles, synthdefFor) {
  const known = new Set(sampleFiles);
  const synthdefs = new Set(["sonic-pi-beep", "sonic-pi-mixer", "sonic-pi-basic_mixer"]);
  const want = (name, fx) => { const def = synthdefFor(name, fx); if (def) synthdefs.add(def); };
  const samples = new Set();
  let sampling = /\bsample\b/.test(code);
  for (const m of code.matchAll(/:([A-Za-z_][A-Za-z0-9_]*)/g)) {
    const file = `${m[1]}.flac`;
    if (known.has(file)) { samples.add(file); sampling = true; }
  }
  for (const m of code.matchAll(/\b(?:use_synth|with_synth|synth)\s*\(?\s*:([A-Za-z0-9_]+)/g)) want(m[1], false);
  for (const m of code.matchAll(/\bwith_fx\s*\(?\s*:([A-Za-z0-9_]+)/g)) want(m[1], true);
  if (/\blive_loop\b/.test(code)) synthdefs.add("sonic-pi-fx_scope_out");   // every loop plays through its scope
  if (sampling) for (const p of PLAYERS) synthdefs.add(`sonic-pi-${p}`);
  return { synthdefs: [...synthdefs], samples: [...samples], tables: programTables(code) };
}
