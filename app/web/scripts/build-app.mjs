#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// Builds the web GUI: app/src → web/app.js, web/app.css. The pages themselves,
// the app's document with a page of the site in each, are scripts/build-site.mjs's.
// CodeMirror and the GUI's own modules are bundled; web/sonic_pi.js (the
// runtime, engine and bridge) and SuperSonic (served as web/supersonic/, from
// the CDN or a local build) stay separate.
//
//   node scripts/build-app.mjs            # once
//   node scripts/build-app.mjs --watch    # rebuild on change
import * as esbuild from "esbuild";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";
import { themeDefaultsCSS } from "./lib/theme-defaults.mjs";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const SRC = path.join(ROOT, "app/src");
const WEB = path.join(ROOT, "web");

const copyStatic = async () => {
  // the app's stylesheet, what the app and the site share (shared.css), the components' own (app/src/ui/*.css), then
  // the site's pages' (site.css): each page is the app with a page of the site in it (scripts/build-site.mjs writes them)
  const ui = fs.readdirSync(path.join(SRC, "ui")).filter((f) => f.endsWith(".css")).sort().map((f) => fs.readFileSync(path.join(SRC, "ui", f), "utf8")).join("\n");
  fs.writeFileSync(path.join(WEB, "app.css"), [await themeDefaultsCSS(ROOT), fs.readFileSync(path.join(SRC, "style.css"), "utf8"), fs.readFileSync(path.join(SRC, "shared.css"), "utf8"), ui, fs.readFileSync(path.join(SRC, "site.css"), "utf8")].join("\n"));
  // the code keyboard's own stylesheet (app/src/keyboard.js)
  fs.copyFileSync(path.join(ROOT, "node_modules/simple-keyboard/build/css/index.css"), path.join(WEB, "simple-keyboard.css"));
  // the Info dialog's pages from the native checkout (scripts/build-info.mjs)
  try { execFileSync(process.execPath, [path.join(ROOT, "scripts/build-info.mjs")], { stdio: "inherit" }); } catch (e) { console.warn("build-info failed:", e.message); }
};

// Imports of sonic_pi.js and https:// URLs resolve in the browser, not here.
const externals = {
  name: "web-externals",
  setup(build) {
    build.onResolve({ filter: /sonic_pi\.js$/ }, () => ({ path: "./sonic_pi.js", external: true }));
    build.onResolve({ filter: /^\.\/runtime\.js$/ }, () => ({ path: "./runtime.js", external: true }));   // web/runtime.js: the one sonic_pi.js imports, so one copy of its state
    build.onResolve({ filter: /^https:\/\// }, (args) => ({ path: args.path, external: true }));
    build.onResolve({ filter: /^fonts\/.*\.woff2$/ }, (args) => ({ path: args.path, external: true }));   // web/fonts/, beside app.css (checked in)
  },
};

fs.rmSync(path.join(WEB, "chunks"), { recursive: true, force: true });   // the last build's: their names change with their contents

const options = {
  entryPoints: [path.join(SRC, "main.js")],
  bundle: true,
  format: "esm",
  target: "es2022",
  // app.js and app.css, and beside them in chunks/ what the app loads only when it is first used (the code keyboard,
  // the QR codes): named for their contents, so a server can keep them for good (package-web.mjs's nginx lines)
  outdir: WEB,
  entryNames: "app",
  chunkNames: "chunks/[name]-[hash]",
  splitting: true,
  sourcemap: true,
  minify: !process.argv.includes("--watch") && !process.argv.includes("--raw"),   // --raw: names kept, for profiling
  plugins: [externals, { name: "static", setup(b) { b.onEnd(async (r) => { if (!r.errors.length) { await copyStatic(); console.log("built web/app.js, web/app.css"); } }); } }],
};

if (process.argv.includes("--watch")) {
  const ctx = await esbuild.context(options);
  await ctx.watch();
  fs.watch(SRC, { recursive: true }, (_, f) => { if (f && /\.(html|css)$/.test(f)) { try { copyStatic(); } catch {} } });
  console.log("watching app/src");
} else {
  await esbuild.build(options);
}
