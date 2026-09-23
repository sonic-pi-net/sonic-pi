#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// The Info dialog's pages (the λ button, main.js showAbout), as native's InfoWidget has them: Community, Core
// Team, Contributors, License and History, read from the Sonic Pi checkout beside this repository (NATIVE_DIR)
// — COMMUNITY.md, CORETEAM.html, CONTRIBUTORS.md and CHANGELOG.md — and this repository's own LICENSE.md (the
// web app's, with the SuperSonic it serves) into web/info/*.html, with the
// images they show. The About page is the app's own (index.html).
//
//   node scripts/build-info.mjs
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { marked } from "marked";
import { execFileSync } from "node:child_process";
import { patreonSupporters } from "./lib/supporters.mjs";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const NATIVE = path.resolve(process.env.NATIVE_DIR || path.join(ROOT, "../.."));
const OUT = path.join(ROOT, "web/info");

const PAGES = [
  // the supporters on Patreon's "in the credits" tier (lib/supporters.mjs, as the site's Patreon page lists them)
  ["supporters", "CONTRIBUTORS.md", NATIVE, () => {
    const s = patreonSupporters(NATIVE);
    if (!s) return null;
    return `# Patreon Supporters\n\n${s.intro}\n\n<div class="info-names">\n\n${s.names.map((n) => `* ${n}`).join("\n")}\n\n</div>\n\n` +
      `<div class="info-dl info-join"><a class="info-support" href="https://patreon.com/samaaron"><svg class="tb-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M19.5 12.572l-7.5 7.428l-7.5 -7.428a5 5 0 1 1 7.5 -6.566a5 5 0 1 1 7.5 6.572"/></svg><span>Join them on Patreon</span></a></div>\n`;
  }],
  ["license", "LICENSE.md", NATIVE],   // one licence for Sonic Pi: the web app has its own section in it
];

// The SuperSonic this copy serves (scripts/lib/runtime-assets.mjs: the checkout beside this repository, or
// SUPERSONIC_DIST), and its clockwork: which commits, whether they carry changes not yet committed, and whether those
// commits are published: the AGPL's source offer (LICENSE.md, "AGPL Compliance") must reach what is served
const SUPERSONIC = path.resolve(process.env.SUPERSONIC_DIST ? path.join(process.env.SUPERSONIC_DIST, "..") : path.join(ROOT, "../external/supersonic"));
function gitState(dir, name) {
  const git = (...args) => { try { return execFileSync("git", ["-C", dir, ...args], { encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] }).trim(); } catch { return ""; } };
  const commit = git("rev-parse", "HEAD");
  if (!commit) return null;
  const dirty = git("status", "--porcelain", "--untracked-files=no").split("\n").some((l) => l && !/ (clockwork|node_modules)$/.test(l));   // the submodule's own state is its own line
  const published = !!git("branch", "-r", "--contains", commit);
  return { name, commit, dirty, published };
}
function checkPublished() {
  for (const p of [gitState(SUPERSONIC, "SuperSonic"), gitState(path.join(SUPERSONIC, "clockwork"), "clockwork")].filter(Boolean))
    if (!p.published || p.dirty) console.warn(`${p.name} at ${p.commit.slice(0, 12)}: ${[p.dirty && "uncommitted changes", !p.published && "not on its remote"].filter(Boolean).join(", ")} — publish it before this build is distributed (AGPL)`);
}

fs.rmSync(OUT, { recursive: true, force: true });
fs.mkdirSync(OUT, { recursive: true });

// native's resource images a page names, brought local
const images = new Set();
function localImages(html) {
  return html.replace(/:\/images\/([\w./-]+)/g, (_, rel) => { images.add(rel); return `info/images/${rel}`; })
    .replace(/\s(height|width)="\d+dx"/g, "");   // Qt's dp sizes: the stylesheet sizes them here
}

let made = 0;
for (const [key, file, from = NATIVE, pick] of PAGES) {
  const src = path.join(from, file);
  if (!fs.existsSync(src)) { console.warn(`no ${file} at ${from}: the ${key} page is left out`); continue; }
  let text = fs.readFileSync(src, "utf8");
  if (pick) { text = pick(text); if (text == null) { console.warn(`no ${key} in ${file}: the page is left out`); continue; } }
  let html = file.endsWith(".md") ? marked.parse(text, { gfm: true, breaks: false }) : text;
  html = localImages(html);
  if (key === "license") checkPublished();   // a check for whoever builds, in the build's output: not the page's to say
  // a path in the repository (LICENSE.md's app/gui/…) is not on the site: it is the repository's page on GitHub
  html = html.replace(/<a href="(?![a-z]+:|#|\/)([^"]+)"/g, '<a href="https://github.com/sonic-pi-net/sonic-pi/blob/dev/$1"');
  // every link out opens outside; a mailto stays as it is
  html = html.replace(/<a href="(https?:\/\/[^"]+)"/g, '<a href="$1" target="_blank" rel="noopener"');
  fs.writeFileSync(path.join(OUT, `${key}.html`), html);
  made++;
}
for (const rel of images) {
  const from = path.join(NATIVE, "app/gui/images", rel);
  if (!fs.existsSync(from)) { console.warn(`no image ${rel}`); continue; }
  fs.mkdirSync(path.dirname(path.join(OUT, "images", rel)), { recursive: true });
  fs.copyFileSync(from, path.join(OUT, "images", rel));
}
console.log(`built web/info: ${made} pages, ${images.size} images, from ${NATIVE}`);
