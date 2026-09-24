#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// Builds the site as static files in build/web: the app (index.html, app.js,
// app.css, built from app/src by scripts/build-app.mjs), its data and themes,
// the spec browser (specs.html, specs.json: every spec's program and expected
// trace) and, when scripts/build-runtime.sh has run, the mruby runtime with
// its random tables and sample facts. Any static host can serve the result:
// both pages run programs on the mruby runtime in the browser and play them
// through SuperSonic. Only the oracle needs scripts/serve.mjs.
//
// SuperSonic comes from its released packages on the CDN, at the version
// package.json pins (supersonicVersion), unless the build says otherwise: supersonic/ then holds only version.json and two re-exporting
// modules, and a public host serves none of the engine's bytes. --supersonic
// local copies the local build in instead, for a deployment that needs an
// unreleased engine; --assets cdn[@version] still leaves the synthdefs and
// samples (35 MB that rarely change) to the CDN (lib/runtime-assets.mjs).
//
//   node scripts/build-web.mjs [--out build/web] [--supersonic cdn[@version]|local] [--assets cdn[@version]|local]
import fs from "node:fs";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { specsJSON } from "./specs-json.mjs";
import { copyRuntime, copySynthdefs, resolveSupersonic, describeSupersonic, supersonicVersionJSON, supersonicShims, flagValue } from "./lib/runtime-assets.mjs";

const ROOT = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const i = process.argv.indexOf("--out");
const OUT = path.resolve(i >= 0 ? process.argv[i + 1] : path.join(ROOT, "build/web"));
execFileSync(process.execPath, [path.join(ROOT, "scripts/build-app.mjs")], { stdio: "inherit" });
// a fresh build: a page the site no longer has (a renamed one) is not left behind to be published. Only under this
// project's build/, so an --out elsewhere is never emptied
if (OUT.startsWith(path.join(ROOT, "build") + path.sep)) fs.rmSync(OUT, { recursive: true, force: true });
fs.mkdirSync(OUT, { recursive: true });
for (const f of ["app.js", "app.js.map", "app.css", "specs.html", "sonic_pi.js", "runtime.js", "live-core.js", "live-worker.js", "osc.js", "gui-stream.js", "examples.js", "manifest.webmanifest", "simple-keyboard.css"]) fs.copyFileSync(path.join(ROOT, "web", f), path.join(OUT, f));
fs.cpSync(path.join(ROOT, "web/chunks"), path.join(OUT, "chunks"), { recursive: true });   // what app.js loads when first used (build-app.mjs)
for (const d of ["data", "theme", "info", "fonts"]) {   // info: the About, Supporters and License pages (build-info.mjs); fonts: Hack, the code's
  const from = path.join(ROOT, "web", d);
  if (fs.existsSync(from)) fs.cpSync(from, path.join(OUT, d), { recursive: true });
}
// SuperSonic: the page's supersonic/ is version.json and, from the CDN, two re-exporting
// modules; a local build is copied in (its type stubs stay behind, and so do the assets the CDN serves)
const ss = await resolveSupersonic(ROOT, flagValue(process.argv, "--supersonic") || "cdn", flagValue(process.argv, "--assets"));
const ssDir = path.join(OUT, "supersonic");
fs.rmSync(ssDir, { recursive: true, force: true });
fs.mkdirSync(ssDir, { recursive: true });
if (ss.dist) {
  const left = new Set([ss.synthdefs && "synthdefs", ss.samples && "samples"].filter(Boolean));
  fs.cpSync(ss.dist, ssDir, { recursive: true, filter: (s) => !s.endsWith(".d.ts") && !left.has(path.relative(ss.dist, s).split(path.sep)[0]) });
}
for (const [name, source] of Object.entries(supersonicShims(ss))) fs.writeFileSync(path.join(ssDir, name), source);
fs.writeFileSync(path.join(ssDir, "version.json"), supersonicVersionJSON(ss));
// the Info card's pages, from site/ (scripts/build-site.mjs)
execFileSync(process.execPath, [path.join(ROOT, "scripts/build-site.mjs"), "--out", path.join(OUT, "site")], { stdio: "inherit" });
const json = specsJSON(ROOT);
fs.writeFileSync(path.join(OUT, "specs.json"), json);
const synthdefs = copySynthdefs(ROOT, OUT, ss);   // Sonic Pi's own synths: all, or those the CDN does not hold as they are here
const withRuntime = copyRuntime(ROOT, OUT);
console.log(`built ${OUT}: the app, its data and themes, ${synthdefs} synthdefs${ss.synthdefs ? ` (the other ${fs.readdirSync(path.resolve(ROOT, "../../etc/synthdefs/compiled")).filter((f) => f.endsWith(".scsyndef")).length - synthdefs} from the CDN)` : ""}, specs.html + specs.json (${JSON.parse(json).length} specs, ${(json.length / 1024).toFixed(0)} KB)` +
  (withRuntime ? " + the mruby runtime (wasm, random tables, sample facts)" : "; no build/runtime yet, so no in-browser runtime (scripts/build-runtime.sh)"));
console.log(describeSupersonic(ss));
