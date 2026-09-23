#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// The share link's token table (app/src/share-table.js): Sonic Pi's
// vocabulary ranked by what it saves across the language's own programs (the
// examples, the quickstart cards, the tutorial's snippets), and a dictionary
// of their most common lines for the deflate that follows. A table, once
// shipped, is fixed — every link written with it has to read back — so this
// is run to make a *new* format: bump the version character, run it, and the
// codec (app/src/share.js) keeps reading the old one too.
//
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

// ── the dictionary: the corpus's most common lines, the commonest last (nearest, the cheapest to point at) ──
export function dictionary(programs, cap = 6144) {
  const lines = new Map();
  for (const p of programs) for (const l of p.code.split("\n")) { const t = l.trimEnd(); if (t.trim()) lines.set(t, (lines.get(t) ?? 0) + 1); }
  const ranked = [...lines].filter(([, n]) => n >= 2).sort((a, b) => b[1] - a[1] || (a[0] < b[0] ? -1 : 1)).map(([l]) => l);
  const keep = [];
  let size = 0;
  for (const l of ranked) { if (size + l.length + 1 > cap) break; keep.push(l); size += l.length + 1; }
  return keep.reverse().join("\n") + "\n";
}

if (process.argv[1] && path.resolve(process.argv[1]) === new URL(import.meta.url).pathname) {
  const programs = corpus();
  const vocab = rank(programs), dict = dictionary(programs);
  const table = { version: VERSION, vocab, dict };
  const codec = makeCodec(table);
  for (const p of programs) if (codec.unpack(codec.pack(p.code)) !== p.code || codec.decode(codec.encode(p.code)) !== p.code) throw new Error(`not lossless: ${p.key}`);
  fs.writeFileSync(OUT, `// Generated by scripts/build-share-table.mjs — the share link's format ${VERSION}. Do not edit: links depend on it.\nexport default ${JSON.stringify(table)};\n`);
  const chars = programs.map((p) => Math.ceil(codec.encode(p.code).length * 4 / 3) + 1).sort((a, b) => a - b);
  console.log(`wrote ${path.relative(ROOT, OUT)}: format ${VERSION}, ${vocab.length} tokens (${vocab.slice(0, 8).join(" ")} …), a ${dict.length} B dictionary of ${dict.split("\n").length - 1} lines; ${programs.length} programs round-trip, median link ${chars[Math.floor(chars.length / 2)]} characters after the #code=`);
}
