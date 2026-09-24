#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// The share link's token table (app/src/share-table.js): Sonic Pi's
// vocabulary ranked by what it saves across Sonic Pi programs, and a 32 KB
// dictionary for the deflate that follows — the pieces of the token stream the
// most programs share (COVER, below), trained on the language's own programs
// (the examples, the quickstart cards, the tutorial, the docs' examples, the
// specs) and, where scripts/fetch-share-corpus.mjs has fetched them, its
// players' (in-thread.sonic-pi.net's posts, the Mehackit course). A table, once
// shipped, is fixed — every link written with it has to read back — so this is
// run to make a *new* format: bump the version character, run it, and the codec
// (app/src/share.js) keeps reading the old one too.
//
//   node scripts/fetch-share-corpus.mjs            # the players' programs, into build/share-corpus (not committed)
//   node scripts/build-share-table.mjs [--version 2] [--out app/src/share-table.js]
import fs from "node:fs";
import path from "node:path";
import { makeCodec } from "../app/src/share.js";

const ROOT = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const arg = (name, fallback) => { const i = process.argv.indexOf(name); return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback; };
const VERSION = arg("--version", "1");
const OUT = path.resolve(arg("--out", path.join(ROOT, "app/src/share-table.js")));
if (!/^[0-9A-Za-z]$/.test(VERSION)) throw new Error("the format is one character, a digit or a letter");

const ref = (f) => JSON.parse(fs.readFileSync(path.join(ROOT, `web/data/reference/${f}.json`), "utf8"));

// ── the programs the ranking reads ──
export function corpus() {
  const out = [];
  for (const g of ref("examples").groups) for (const e of g.examples) out.push({ key: e.key, code: e.code });
  for (const d of ref("quickstart").decks) for (const c of d.cards) out.push({ key: `card:${c.title}`, code: c.code });
  const tut = path.join(ROOT, "web/data/tutorial/en");
  for (const f of fs.readdirSync(tut).sort()) {
    const d = JSON.parse(fs.readFileSync(path.join(tut, f), "utf8"));
    for (const b of d.blocks ?? []) if (b.type === "code" && b.source) out.push({ key: `${f}:${out.length}`, code: b.source });
  }
  const site = path.join(ROOT, "web/site/site-examples.json");   // the site's own cards, when built
  if (fs.existsSync(site)) for (const g of JSON.parse(fs.readFileSync(site, "utf8")).groups) for (const e of g.examples) if (!out.some((o) => o.key === e.key)) out.push({ key: e.key, code: e.code });
  return out;
}

// ── the programs the dictionary learns from: the corpus above, and more ──
// every program once: the docs' examples, native's example files, the specs (their comments of the harness's own
// left off), the players' programs fetched into build/share-corpus, and sets as the Share menu writes them
function walk(dir, out = []) { if (!fs.existsSync(dir)) return out; for (const e of fs.readdirSync(dir, { withFileTypes: true })) { const f = path.join(dir, e.name); if (e.isDirectory()) walk(f, out); else if (f.endsWith(".rb")) out.push(f); } return out; }
export async function trainingCorpus() {
  const seen = new Set(), out = [];
  const add = (key, code) => { code = code.replace(/\r/g, ""); if (code.trim() && !seen.has(code)) { seen.add(code); out.push({ key, code }); } };
  for (const p of corpus()) add(p.key, p.code);
  for (const e of ref("lang").pages.flatMap((p) => p.examples ?? [])) add("lang", e.code);
  for (const f of walk(path.join(ROOT, "../../etc/examples"))) add(f, fs.readFileSync(f, "utf8"));
  for (const f of walk(path.join(ROOT, "specs")).filter((f) => !f.includes(`${path.sep}bpm${path.sep}`))) add(f, fs.readFileSync(f, "utf8").split("\n").filter((l) => !/^# (horizon|expected|racy)/.test(l)).join("\n"));
  const fetched = path.join(ROOT, "build/share-corpus");
  const players = [];
  for (const f of fs.existsSync(fetched) ? fs.readdirSync(fetched).filter((f) => f.endsWith(".json")).sort() : []) for (const b of JSON.parse(fs.readFileSync(path.join(fetched, f), "utf8")).blocks) { const n = out.length; add(`${f}:${b.topic ?? b.page}`, b.code); if (out.length > n) players.push(out[out.length - 1]); }
  const { serialise } = await import("../app/src/set-bundle.js");
  const real = [...corpus().filter((p) => !/\.json:/.test(p.key)), ...players];
  for (let i = 0; i + 3 <= real.length && i < 240; i += 3) add(`set:${i}`, serialise([real[i].code, real[i + 1].code, "", real[i + 2].code], i % 4, [], i % 2 ? { name: "My set", description: "" } : {}));
  return { programs: out, players: players.length };
}

// ── the vocabulary: every name the language has, Ruby's words, a few phrases ──
const RUBY = ["do", "end", "if", "else", "elsif", "unless", "while", "until", "case", "when", "then", "return", "def", "true", "false", "nil", "and", "or", "not", "begin", "rescue", "ensure", "yield", "self", "puts", "print", "each", "map", "times", "loop", "lambda", "->", "..", "...", "=>", "==", "!=", "<=", ">=", "+=", "-=", "*=", "||", "&&", "||="];
const PHRASES = ["live_loop :", "with_fx :", "use_synth :", "define :", "in_thread do", ".times do", " do |", "sync :", "cue :", ".choose", ".tick", ".look", ".ring", ".shuffle", ".reverse", ".rotate", ".length", ".to_s", ".to_i", ".first", ".last", "chord(", "scale(", "rrand(", "rand(", "rrand_i(", "rand_i(", "0.5", "0.25", "0.125", "0.75", "1.5", "0.1", "0.2", "0.3", "0.8", "0.9", ":minor", ":major", ":minor_pentatonic", ":major_pentatonic", ":e", ":c", ":a", ":d", ":g", ":f", ":b", "[0]", "[1]", "# "];
function vocabulary() {
  const words = new Set(RUBY);
  for (const p of ref("lang").pages) words.add(p.key);
  for (const p of ref("synths").pages) { words.add(`:${p.key}`); for (const o of p.opts) words.add(`${o.name}:`); }
  for (const p of ref("fx").pages) { words.add(`:${p.key}`); for (const o of p.opts) words.add(`${o.name}:`); }
  for (const g of ref("samples").groups) for (const s of g.samples) words.add(`:${s}`);
  for (const p of PHRASES) words.add(p);
  return [...words].filter((t) => t.length >= 2 && !t.includes("\n"));
}

// ── ranked: the whole vocabulary tokenises the corpus, then each token's hits × the bytes it saves ──
export function rank(programs) {
  const all = vocabulary().sort();
  const probe = makeCodec({ vocab: all, dict: "" });
  const hits = new Map(all.map((t) => [t, 0]));
  const count = (k) => hits.set(all[k], hits.get(all[k]) + 1);
  for (const p of programs) {
    const b = probe.pack(p.code);
    for (let i = 0; i < b.length; i++) {
      if (b[i] >= 0xF0) count(96 + b[i] - 0xF0);
      else if (b[i] >= 0xE0) {}
      else if (b[i] >= 0x80) count(b[i] - 0x80);
      else if (b[i] === 0x01) { count(112 + b[i + 1]); i++; }
      else if (b[i] === 0x02) { count(368 + (b[i + 1] << 8) + b[i + 2]); i += 2; }
      else if (b[i] === 0x03) i++;
    }
  }
  const worth = (t) => hits.get(t) * (t.length - 1);
  return all.sort((a, b) => worth(b) - worth(a) || hits.get(b) - hits.get(a) || (a < b ? -1 : 1));
}

// ── the dictionary: COVER over the token stream ──
// The segments (k bytes) whose d-byte pieces the most programs share, chosen greedily, each piece counted once: the
// best last, nearest the text being compressed, which is the cheapest place to point back to. Nearest of all, a set's
// own fixed text (the Share menu's sets: set-bundle.js), which every set link has. Filling deflate's whole window
// (32 KB): judged on programs it was not trained on (5-fold), it made the median link 22% shorter than the common-line
// dictionary it replaced, the players' programs' 20%.
class MaxHeap {
  constructor() { this.a = []; }
  push(x) { const a = this.a; a.push(x); let i = a.length - 1; while (i) { const p = (i - 1) >> 1; if (a[p].s >= a[i].s) break; [a[p], a[i]] = [a[i], a[p]]; i = p; } }
  pop() { const a = this.a, top = a[0], last = a.pop(); if (a.length) { a[0] = last; let i = 0; for (;;) { const l = 2 * i + 1, r = l + 1; let m = i; if (l < a.length && a[l].s > a[m].s) m = l; if (r < a.length && a[r].s > a[m].s) m = r; if (m === i) break; [a[m], a[i]] = [a[i], a[m]]; i = m; } } return top; }
  get size() { return this.a.length; }
}
const SET_TAIL = `#-- Sonic Pi Set v1\n#-- meta {"current":0,"description":"","name":"","zooms":[2,2,2,2,2,2,2,2,2,2]}\n#-- buffer 0\n#-- buffer 1\n#-- buffer 2\n#-- buffer 3\n##| `;
export function dictionary(programs, vocab, { size = 32768, k = 32, d = 6 } = {}) {
  const codec = makeCodec({ vocab, dict: "" });
  const samples = programs.map((p) => Buffer.from(codec.pack(p.code)));
  const tail = Buffer.from(codec.pack(SET_TAIL));
  const key = (s, j) => s.subarray(j, j + d).toString("latin1");
  const freq = new Map();   // d-byte piece → how many programs have it
  for (const s of samples) { const own = new Set(); for (let i = 0; i + d <= s.length; i++) own.add(key(s, i)); for (const m of own) freq.set(m, (freq.get(m) || 0) + 1); }
  for (const [m, n] of freq) if (n < 2) freq.delete(m);
  const score = (si, i) => { const s = samples[si]; let sc = 0; const used = new Set(); for (let j = i; j + d <= Math.min(i + k, s.length); j++) { const m = key(s, j); if (!used.has(m)) { used.add(m); sc += freq.get(m) || 0; } } return sc; };
  const heap = new MaxHeap();
  samples.forEach((s, si) => { for (let i = 0; i + d <= s.length; i += 2) { const sc = score(si, i); if (sc > 0) heap.push({ si, i, s: sc }); } });
  const chosen = []; let total = tail.length;
  while (heap.size && total < size) {
    const top = heap.pop(), fresh = score(top.si, top.i);
    if (fresh < top.s) { if (fresh > 0) { top.s = fresh; heap.push(top); } continue; }   // stale: back in at its true score
    const seg = samples[top.si].subarray(top.i, Math.min(top.i + k, samples[top.si].length));
    chosen.push(seg); total += seg.length;
    for (let j = 0; j + d <= seg.length; j++) freq.set(key(seg, j), 0);
  }
  const all = Buffer.concat([...chosen.reverse(), tail]);
  return all.subarray(Math.max(0, all.length - size));
}

if (process.argv[1] && path.resolve(process.argv[1]) === new URL(import.meta.url).pathname) {
  const { programs, players } = await trainingCorpus();
  const vocab = rank(programs), dict = dictionary(programs, vocab);
  const table = { version: VERSION, vocab, dictionary: dict.toString("base64") };
  const codec = makeCodec(table);
  for (const p of programs) if (codec.unpack(codec.pack(p.code)) !== p.code || codec.decode(codec.encode(p.code)) !== p.code) throw new Error(`not lossless: ${p.key}`);
  const trained = `${programs.length} programs (${players} of them players', from build/share-corpus), ${new Date().toISOString().slice(0, 10)}`;
  fs.writeFileSync(OUT, `// Generated by scripts/build-share-table.mjs — the share link's format ${VERSION}. Do not edit: links depend on it.\n// Trained on ${trained}. The format: docs/share-link-format.md.\nexport default ${JSON.stringify(table)};\n`);
  console.log(`wrote ${path.relative(ROOT, OUT)}: format ${VERSION}, ${vocab.length} tokens (${vocab.slice(0, 8).join(" ")} …), a ${dict.length} B dictionary; trained on ${trained}; all round-trip. A new table is a new format: its hash is pinned in app/test/share-link.test.mjs`);
}
