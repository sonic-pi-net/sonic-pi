#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// The share link, checked: every program the app ships round-trips through
// app/src/share.js, and the examples' links are measured against the places a
// link has to fit — a QR code (byte mode, as base64url needs; the size the
// phone can read) and a tweet (every URL is 23 of its 280 characters, however
// long, up to the 4096 X shortens).
//
//   node scripts/share-check.mjs [--all]   # --all lists every program, not just the examples
import { encodeCode, decodeCode, loadShareCodec } from "../app/src/share.js";
await loadShareCodec();
import { corpus } from "./build-share-table.mjs";

const HOME = "https://sonic-pi.net/#code=";
// QR byte-mode capacities at error correction L (M) by version: 10 is 57×57 modules, 25 is 117×117, 40 is 177×177
const QR = [["v10", 271, 213], ["v20", 858, 666], ["v25", 1273, 997], ["v40", 2953, 2331]];
const TCO = 4096;
const programs = corpus();
let bad = 0;
const rows = [];
for (const p of programs) {
  const link = encodeCode(p.code);
  let back;
  try { back = decodeCode(link); } catch (e) { back = String(e); }
  if (back !== p.code) { bad++; console.log(`✗ ${p.key} does not round-trip`); }
  rows.push({ key: p.key, bytes: new TextEncoder().encode(p.code).length, url: HOME.length + link.length });
}
const shown = process.argv.includes("--all") ? rows : rows.filter((r) => !/^card:|\.json:/.test(r.key));
const fit = (n) => `${QR.map(([v, l, m]) => (n <= m ? `${v} M` : n <= l ? `${v} L` : "")).find(Boolean) ?? "no QR"}${n <= TCO ? "" : "  too long for X"}`;
console.log(`${"program".padEnd(28)} ${"source".padStart(7)} ${"link".padStart(6)}  fits`);
for (const r of shown) console.log(`${r.key.padEnd(28)} ${String(r.bytes).padStart(6)}B ${String(r.url).padStart(6)}  ${fit(r.url)}`);
const urls = rows.map((r) => r.url).sort((a, b) => a - b), ex = shown.map((r) => r.url).sort((a, b) => a - b);
const q = (v, k) => v[Math.floor(v.length * k)];
console.log(`\n${programs.length} programs round-trip${bad ? ` — ${bad} DO NOT` : ""}; whole link, examples: median ${q(ex, 0.5)}, 90th ${q(ex, 0.9)}, max ${ex[ex.length - 1]} characters; everything: median ${q(urls, 0.5)}`);
for (const [v, l, m] of QR) console.log(`  ${v}: ${rows.filter((r) => r.url <= l).length}/${rows.length} at L, ${rows.filter((r) => r.url <= m).length} at M`);
console.log(`  a tweet: ${rows.filter((r) => r.url <= TCO).length}/${rows.length} (a URL counts 23 characters whatever its length)`);
process.exit(bad ? 1 : 0);
