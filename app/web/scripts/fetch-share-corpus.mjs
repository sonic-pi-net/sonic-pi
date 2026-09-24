#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// Sonic Pi programs its players have written, for training the share link's dictionary (build-share-table.mjs):
// every code block in in-thread.sonic-pi.net's posts (Discourse's public JSON), kept when it is recognisably Sonic
// Pi, and every one in the Mehackit course (sonic-pi.mehackit.org). Polite: one request at a time, a few a second,
// backing off when asked. Resumable: topics already read are skipped. Into build/share-corpus, which is not
// committed — the table built from it is.
//
//   node scripts/fetch-share-corpus.mjs [--only in-thread|mehackit]
import fs from "node:fs";
import path from "node:path";

const ROOT = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const DIR = path.join(ROOT, "build/share-corpus");
const only = (() => { const i = process.argv.indexOf("--only"); return i >= 0 ? process.argv[i + 1] : null; })();
fs.mkdirSync(DIR, { recursive: true });

const wait = (ms) => new Promise((r) => setTimeout(r, ms));
const UA = { "user-agent": "Sonic Pi share-link corpus (https://sonic-pi.net)" };
async function get(url, { json = false } = {}) {
  for (let tries = 0; tries < 5; tries++) {
    await wait(300);
    const r = await fetch(url, { headers: { ...UA, accept: json ? "application/json" : "text/html" } }).catch(() => null);
    if (r?.ok) return json ? r.json() : r.text();
    if (r?.status === 429) { await wait(15000); continue; }
    if (r?.status === 404 || r?.status === 403) return null;
    await wait(2000);
  }
  return null;
}
const entities = { "&lt;": "<", "&gt;": ">", "&amp;": "&", "&quot;": "\"", "&#39;": "'", "&#x27;": "'", "&nbsp;": " " };
const text = (html) => html.replace(/<[^>]+>/g, "").replace(/&(lt|gt|amp|quot|nbsp|#39|#x27);/g, (m) => entities[m] ?? m).replace(/&#(\d+);/g, (_, n) => String.fromCodePoint(+n)).replace(/\r/g, "").replace(/\n+$/, "");

// ── in-thread: the categories people post programs to ──
async function inThread() {
  const BASE = "https://in-thread.sonic-pi.net", OUT = path.join(DIR, "in-thread.json");
  const CATS = [["creations-and-ideas", 8], ["support-help-resources", 16], ["support", 7], ["performances-streams", 11], ["educators", 14], ["events-workshops-lessons", 13], ["general", 6], ["development", 10]];
  const sonicPi = /\b(play|sample|sleep|live_loop|synth|use_synth|with_fx|use_bpm|in_thread|play_pattern|define|ring|tick)\b/;
  const state = fs.existsSync(OUT) ? JSON.parse(fs.readFileSync(OUT, "utf8")) : { topics: {}, blocks: [] };
  let read = 0;
  for (const [slug, id] of CATS) {
    for (let page = 0; page < 100; page++) {
      const list = await get(`${BASE}/c/${slug}/${id}.json?page=${page}`, { json: true });
      const topics = list?.topic_list?.topics ?? [];
      for (const t of topics) {
        if (state.topics[t.id]) continue;
        const d = await get(`${BASE}/t/${t.id}.json`, { json: true });
        state.topics[t.id] = true;
        for (const p of d?.post_stream?.posts ?? []) for (const m of (p.cooked ?? "").matchAll(/<pre><code[^>]*>([\s\S]*?)<\/code><\/pre>/g)) {
          const code = text(m[1]);
          if (code.length >= 20 && sonicPi.test(code)) state.blocks.push({ topic: t.id, post: p.post_number, code });
        }
        if (++read % 25 === 0) { fs.writeFileSync(OUT, JSON.stringify(state)); console.log(`in-thread ${slug}: ${read} topics read, ${state.blocks.length} code blocks`); }
      }
      if (!topics.length || !list.topic_list.more_topics_url) break;
    }
  }
  fs.writeFileSync(OUT, JSON.stringify(state));
  console.log(`in-thread: ${Object.keys(state.topics).length} topics, ${state.blocks.length} code blocks → ${path.relative(ROOT, OUT)}`);
}

// ── Mehackit: every exercise page the front page links ──
async function mehackit() {
  const BASE = "https://sonic-pi.mehackit.org", OUT = path.join(DIR, "mehackit.json");
  const front = (await get(`${BASE}/`)) ?? "";
  const pages = [...new Set([...front.matchAll(/href="(\/exercises\/en\/[^"#]+\.html)"/g)].map((m) => m[1]))];
  const blocks = [];
  for (const p of pages) for (const m of ((await get(BASE + p)) ?? "").matchAll(/<pre[^>]*>\s*<code[^>]*>([\s\S]*?)<\/code>\s*<\/pre>/g)) { const code = text(m[1]); if (code.trim().length >= 10) blocks.push({ page: p, code }); }
  fs.writeFileSync(OUT, JSON.stringify({ pages, blocks }));
  console.log(`mehackit: ${pages.length} pages, ${blocks.length} code blocks → ${path.relative(ROOT, OUT)}`);
}

if (!only || only === "mehackit") await mehackit();
if (!only || only === "in-thread") await inThread();
