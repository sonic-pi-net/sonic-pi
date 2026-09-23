#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// sonic-pi.net: every page a whole HTML document, the site's content in it as HTML and the app beside it, so a
// page reads with no script at all and Launch opens the editor in the same page (the engine starts in the tap).
//
//   site/pages/<page>.html    a page's sections, its {{…}} filled in (scripts/lib/site/template.mjs)
//   site/partials/*.html      what more than one page has, by {{include name}}
//   site/data/*.json          what the pages show that is data rather than prose
//   site/css/landing.css      the pages' stylesheet
//   site/media/               the images the pages use
//   app/src/index.html        the app: each page is it, with the page's content in its Info card
//
// A page is its sections, each one shape: a <div class="wrapper …"> holding an <article id> whose header's <h1>
// names it (data-title says otherwise), data-icon its icon in the page's list, data-group the list's group it
// starts; then an optional <footer>. The page's list is made from them here, and a section that breaks the shape
// stops the build. The tutorial is pages of its own, a page a chapter (tutorial.html, tutorial-02.html …), reached by the bar's book icon.
//
// Out: web/<page>.html (index.html is Home, code.html the editor), and web/site/ with what they use: css/, media/.
//
//   node scripts/build-site.mjs [--out web/site]      the pages go beside the directory given
import fs from "node:fs";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { render } from "./lib/site/template.mjs";
import { icon } from "./lib/site/icons.mjs";
import { icon as appIcon } from "../app/src/icons.js";
import { logo } from "./lib/site/logo.mjs";
import { patreonSupporters } from "./lib/supporters.mjs";
import { highlightHTML } from "../app/src/highlight.js";

const ROOT = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const arg = (name, fallback) => { const i = process.argv.indexOf(name); return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback; };
const SITE = path.join(ROOT, "site");
const OUT = path.resolve(arg("--out", path.join(ROOT, "web/site")));
const PAGES_OUT = path.dirname(OUT);
const NATIVE = path.resolve(process.env.NATIVE_DIR || path.join(ROOT, "../.."));   // the Sonic Pi checkout around app/web
// what the build fetches (a video's thumbnail), kept so a build needs no network
const CACHE = path.join(SITE, "fetched");
const read = (...p) => fs.readFileSync(path.join(...p), "utf8");

// The desktop release the download rows offer
const RELEASE = "5.0.0";

// ── the pages, in the order of the bar's tabs ──
const PAGES = [
  { key: "about", title: "Home", tip: "Discover Sonic Pi", file: "index.html", lead: "Discover", description: "Sonic Pi is a new kind of code-based instrument for a new generation of musicians. Use the same code for both composition and performance. Sonic Pi is free and open source software." },
  { key: "examples", title: "Examples", tip: "Pieces of music to play, change and learn from", file: "examples.html", list: "All examples", description: "Explore some of the different ways to work with Sonic Pi" },
  { key: "learn", title: "Learn", tip: "The tutorial, lessons, books and talks", file: "learn.html", lead: "Start here", description: "Learn Sonic Pi: the tutorial, the video course, the book, and Sonic Pi in the classroom." },
  { key: "support", title: "Support", tip: "Help keep Sonic Pi free for everyone", file: "support.html", lead: "Support", description: "Sonic Pi is free because it is supported by the community." },
];

// the editor's own document: the app with Home in its card, put away (the Code tab's, and the tutorial's, in its docs)
const CODE = { file: "code.html", title: "Code · Sonic Pi", tip: "The editor: write and run your own music", description: "Sonic Pi's code editor: write and live code music in your browser." };

// ── what the pages are filled in with ──
const mark = logo(path.join(NATIVE, "app/gui/images/logo-square.svg"));
if (!mark) throw new Error(`no logo at ${NATIVE}/app/gui/images/logo-square.svg (NATIVE_DIR=<a Sonic Pi checkout>)`);

// the tutorial's chapters, as the app's docs pane has them (scripts/gen-editor-data.rb): Learn's way straight in to each
const tutorialIndex = JSON.parse(read(ROOT, "web/data/tutorial/en/index.json")).chapters;

// ── the tutorial: a page a chapter, its sections the chapter's parts (scripts/gen-editor-data.rb writes each part's
// blocks: headings, prose, code, lists, pictures) ──
// a part's chapter is its key's first word (02.1-Your-First-Beeps: 02; A.03-coded-beats: A); chapter 1 is tutorial.html
const chapterOf = (key) => key.split(/[.-]/)[0];
const tutId = (key) => `tut-${key.toLowerCase().replace(/[^a-z0-9]+/g, "-")}`;   // main.js makes the same, for the old #docs/tutorial/<key> links
const TUTORIAL = [];
// each chapter's mark in the list: what it is about, at a glance (Tabler's names)
const CHAPTER_ICON = { "01": "player-play", "02": "wave-sine", "03": "vinyl", "04": "dice-5", "05": "brackets", "06": "wand",
  "07": "adjustments-horizontal", "08": "list-details", "09": "repeat", "10": "clock", "11": "piano", "12": "network",
  "13": "volume", "99": "flag", A: "news", B: "bulb" };
for (const c of tutorialIndex) {
  const code = chapterOf(c.key);
  let t = TUTORIAL.find((x) => x.code === code);
  if (!t) {
    const [, n, name] = /^(\S+) (?:Appendix \S+ - )?(.+)$/.exec(c.title) ?? [null, code, c.title];
    const first = !TUTORIAL.length;
    t = { code, n, name, key: first ? "tutorial" : `tutorial-${code.toLowerCase()}`, file: first ? "tutorial.html" : `tutorial-${code.toLowerCase()}.html`, tab: "tutorial", chapter: true, parts: [] };
    t.title = first ? "Tutorial" : `${name} · Tutorial`;
    t.description = `The Sonic Pi tutorial${first ? ", from your first beep to live coding a set" : `: ${n} ${name}`}.`;
    TUTORIAL.push(t);
  }
  t.parts.push({ key: c.key, title: c.title, id: tutId(c.key) });
}
const tutorialPart = (key) => JSON.parse(read(ROOT, `web/data/tutorial/en/${key}.json`));
// a picture the text names where the tutorial's source keeps it (etc/doc/images) is the app's copy, under data/images
const tutImage = (src) => src.replace(/^(?:\.\.\/)*etc\/doc\/images\//, "data/images/");
// what the tutorial teaches that the web cannot do yet: said at the top of the part, which is the app's text as it is
const WEB_ONLY_NOTE = "your own sample files don't play here yet, only the built-in samples. " +
  "Everything in this section works in the <a href=\"index.html#get-sonic-pi\">Sonic Pi app</a> for Windows, macOS, Linux and Raspberry Pi.";
const WEB_NOTES = { "03.6-External-Samples": WEB_ONLY_NOTE, "03.7-Sample-Packs": WEB_ONLY_NOTE };
// the tutorial's code in its sentences (`amp:`, `play 60`), coloured as the editor colours it: written into the page, so
// it reads the same with no script at all
const unescape = (x) => x.replace(/&(amp|lt|gt|quot|#39);/g, (_, e) => ({ amp: "&", lt: "<", gt: ">", quot: '"', "#39": "'" })[e]);
const inlineCode = (html) => html.replace(/<code>([^<]*)<\/code>/g, (_, c) => `<code class="sp-inline">${highlightHTML(unescape(c))}</code>`);
function tutorialBlocks(part) {
  let n = 0, heading = part.title.replace(/^\S+ /, "");   // a card is named for the heading over it, the part's own at first
  return tutorialPart(part.key).blocks.map((b, i) => {
    if (b.type === "heading") { if (i === 0 && b.level === 1) return ""; heading = b.text; const h = `h${Math.min(Math.max(b.level, 2), 4)}`; return `<${h}>${esc(b.text)}</${h}>`; }   // its first heading is the part's own (its h1, numbered); the rest step down from it, ## an h2
    if (b.type === "prose") return inlineCode(b.html).replace(/(<img\b[^>]*\ssrc=")([^"]+)"/g, (_, pre, src) => `${pre}${tutImage(src)}" loading="lazy"`);
    if (b.type === "list") return `<${b.ordered ? "ol" : "ul"}>${b.items.map((x) => `<li>${inlineCode(x)}</li>`).join("")}</${b.ordered ? "ol" : "ul"}>`;
    if (b.type === "image") return `<img class="doc-image" src="data/images/${esc(b.path)}" alt="${esc(b.alt ?? "")}" loading="lazy">`;
    if (b.type === "code") return `<pre class="sp-card" data-key="${part.id}-${++n}" data-title="${esc(heading)}"${b.runnable ? "" : " data-still"}><code>${esc(b.source.replace(/\n$/, ""))}</code></pre>`;
    return "";
  }).join("\n");
}
function tutorialBody(page) {
  // the list: every chapter and its parts, the other chapters' on their own pages
  const entries = [];
  for (const t of TUTORIAL) {
    // the chapter's own opening part IS the chapter: its entry heads the list's group, named as the chapter is, and
    // the group's own heading stays out of sight (the page bar still reads it, to name the chapter it steps through)
    entries.push({ group: `${t.n} ${t.name}`, hidden: true });
    for (const part of t.parts) {
      const opener = part === t.parts[0];
      entries.push({ id: part.id, title: opener ? `${t.n} ${t.name}` : part.title.replace(/^\S+ /, ""), icon: opener ? CHAPTER_ICON[t.code] ?? "book" : null,
                     cls: opener ? "docs-chapter" : "sub", file: t === page ? null : t.file });
    }
  }
  // the page bar: this chapter's parts
  bars.set(page.key, pageBar([{ group: `${page.n} ${page.name}` }, ...page.parts.map((part) => ({ id: part.id, title: part.title }))]));
  const main = page.parts.map((part) => `<div class="wrapper style1 tut-wrap"><article class="container tut-part" id="${part.id}" data-icon="book">\n<header><h1>${esc(part.title)}</h1></header>\n${WEB_NOTES[part.key] ? `<p class="tut-note"><strong>On the web:</strong> ${WEB_NOTES[part.key]}</p>\n` : ""}${tutorialBlocks(part)}\n</article></div>`).join("\n");
  // the way on, as a book's: the chapter before and the one after
  const at = TUTORIAL.indexOf(page), step = (t, cls, label) => (t ? `<a class="tut-step ${cls}" href="${t.file}"><span class="tut-step-k">${label}</span><span>${esc(`${t.n} ${t.name}`)}</span></a>` : "<span></span>");
  const way = `<nav class="tut-next" aria-label="The tutorial, before and after this chapter">${step(TUTORIAL[at - 1], "prev", "Previous chapter")}${step(TUTORIAL[at + 1], "next", "Next chapter")}</nav>`;
  return `<div class="ic-page ic-split" data-tab="tutorial">${sideList(entries, "The tutorial")}<main class="ic-main tut-page" id="${page.key}-content" tabindex="-1" aria-label="${esc(page.title)}">\n${main}\n${way}\n</main></div>`;
}

const supporters = patreonSupporters(NATIVE);
if (!supporters) console.warn("no Patreon supporters in native's CONTRIBUTORS.md");
const esc = (t) => String(t).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");

// Sonic Pi's history, counted at each build and rounded down (200+, 12k+) so a figure stays true until a milestone
function stats() {
  const git = (...args) => { try { return execFileSync("git", ["-C", NATIVE, ...args], { encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] }).trim(); } catch { return ""; } };
  const commits = Number(git("rev-list", "--count", "HEAD")) || 0;
  const contributors = (/^## Developer Contributions\n([\s\S]*?)(?=^## )/m.exec(read(NATIVE, "CONTRIBUTORS.md"))?.[1].match(/^\* /gm) ?? []).length;
  const atLeast = (x) => { const step = x >= 100 ? 50 : 10; const f = Math.floor(x / step) * step; return f > 0 ? `${f}+` : ""; };
  const figures = [["Since 2012", "open source"], [atLeast(contributors), "contributors"], [commits >= 1000 ? `${Math.floor(commits / 1000)}k+` : "", "commits"]];
  return `<dl class="os-stats">${figures.filter(([v]) => v).map(([v, what]) => `<div><dt>${v}</dt><dd>${what}</dd></div>`).join("")}</dl>`;
}

// The examples: the app's own, in its levels, each with its site card's blurb and width; a site card the
// app has no example for (its own code in the data) leads the first level. Each is its code in a <pre>, which the
// page's script makes a card to play and edit (site.js); without a script it is the code to read.
const norm = (s) => s.toLowerCase().replace(/[^a-z]/g, "");
const exampleLevels = (() => {
  const levels = JSON.parse(read(ROOT, "web/data/reference/examples.json")).groups;
  const cards = JSON.parse(read(SITE, "data/example-cards.json"));
  const cardFor = (e) => cards.find((c) => norm(c.title).startsWith(norm(e.title)));   // "Haunted" is the site's "Haunted Bells"
  return levels.map((g, i) => ({
    title: g.title,
    examples: [
      ...(i === 0 ? cards.filter((c) => c.code).map((c) => ({ key: norm(c.title), title: c.title, code: c.code, blurb: c.blurb, wide: !!c.wide })) : []),
      ...g.examples.map((e) => { const c = cardFor(e); return { key: e.key || norm(e.title), title: c?.title ?? e.title, code: e.code, blurb: c?.blurb ?? "", wide: !!c?.wide }; }),
    ],
  }));
})();
const example = (e, { id = true } = {}) => `<pre class="sp-card"${id ? ` id="example-${e.key}"` : ""} data-key="${e.key}" data-title="${esc(e.title)}"${e.blurb ? ` data-blurb="${esc(e.blurb)}"` : ""}${e.wide ? " data-wide" : ""}><code>${esc(e.code.replace(/\n$/, ""))}</code></pre>`;
const levelId = (title) => `level-${title.toLowerCase().replace(/[^a-z0-9]+/g, "-")}`;

const videos = new Set();
const tick = icon("check");
const helpers = {
  version: RELEASE,
  icon: (name, opts) => icon(name, opts),
  logo: mark.punched,
  svg: (file) => read(SITE, file).trimEnd(),
  include: (name) => render(read(SITE, "partials", `${name}.html`), helpers, `site/partials/${name}.html`),
  stats,
  chapters: () => TUTORIAL.filter((t) => /^\d+$/.test(t.code)).map((t) => `<a class="tut-chapter" href="${t.file}"><span class="tut-num">${esc(t.code.replace(/^0/, ""))}</span>${esc(t.name)}</a>`).join(""),
  "supporters-intro": () => esc(supporters?.intro ?? "").replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2" rel="noopener" target="_blank">$1</a>'),
  supporters: () => (supporters?.names ?? []).map((n) => `          <li>${esc(n)}</li>`).join("\n"),
  // the hero's example: Pentatonic Bleeps, the site's own first card
  "hero-example": () => example({ ...exampleLevels.flatMap((g) => g.examples).find((e) => e.key === "pentatonicbleeps"), wide: true }, { id: false }),
  examples: () => exampleLevels.filter((g) => g.examples.length).map((g) => `<h3 class="qs-level" id="${levelId(g.title)}">${g.title}</h3>\n<div class="qs-grid">\n${g.examples.map((e) => example(e)).join("\n")}\n</div>`).join("\n"),
  // the App and Browser columns of the feature table
  yes: `<span class="conn-yes">${tick}<span class="sr-only">yes</span></span>`,
  no: `<span class="conn-no">${icon("minus")}<span class="sr-only">no</span></span>`,
  partly: (note) => `<span class="conn-yes">${tick}<span class="sr-only">yes</span></span><small class="conn-note">${note}</small>`,
  // YouTube cannot load under the cross-origin isolation the engine needs, so a video is its thumbnail and a play
  // disc, opening YouTube
  youtube: (id, title) => {
    videos.add(id);
    return `<a class="yt-lite" href="https://www.youtube.com/watch?v=${id}" target="_blank" rel="noopener" data-yt="${id}" title="${title}" style="background-image: url('site/media/yt/${id}.jpg')"><span class="yt-play" aria-hidden="true"><svg viewBox="0 0 24 24"><path d="M6 4v16a1 1 0 0 0 1.524 .852l13 -8a1 1 0 0 0 0 -1.704l-13 -8a1 1 0 0 0 -1.524 .852z"/></svg></span><span class="sr-only">${title} (YouTube)</span></a>`;
  },
};

// ── a page's sections, as its list reads them ──
const attr = (tag, name) => { const m = new RegExp(`\\s${name}(?:="([^"]*)")?[\\s>]`).exec(tag); return m ? m[1] ?? "" : null; };
function sections(html, where) {
  const found = [];
  for (const m of html.matchAll(/<article\b[^>]*>/g)) {
    const tag = m[0], id = attr(tag, "id");
    if (!id) throw new Error(`${where}: a section with no id — ${tag}`);
    const h1 = /<h1[^>]*>([\s\S]*?)<\/h1>/.exec(html.slice(m.index))?.[1];
    const title = attr(tag, "data-title") ?? h1?.replace(/<[^>]+>/g, "").replace(/\s+/g, " ").trim();
    if (!title) throw new Error(`${where}: #${id} has no <h1> (or data-title) to name it`);
    const icon = attr(tag, "data-icon");
    if (!icon) throw new Error(`${where}: #${id} has no data-icon`);
    found.push({ id, title, icon, group: attr(tag, "data-group") });
  }
  return found;
}

// ── a page's list: its sections, down the left on a wide screen, folded behind a chip on a phone (ui/submenu.js) ──
const CHEVRON = '<svg class="tb-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M6 9l6 6l6 -6"/></svg>';
function sideList(entries, label) {
  const items = entries.map((e) => (e.id == null
    ? `<div class="docs-group"${e.hidden ? " hidden" : ""}>${esc(e.group)}</div>`
    : `<a class="docs-item${e.cls ? ` ${e.cls}` : ""}" href="${e.file ?? ""}#${e.id}" data-key="${e.id}">${e.icon === "logo" ? '<img class="tb-icon logo-icon logo-icon-plain" src="data/icon-256.png" alt=""><img class="tb-icon logo-icon logo-icon-lit" alt="">' : e.icon ? icon(e.icon) : ""}<span>${esc(e.title)}</span></a>`));
  return `<div class="ic-fold-bar"><button class="ic-fold" type="button" aria-expanded="false"><span>${esc(label)}</span>${CHEVRON}</button><nav class="ic-side" aria-label="${esc(label)}"><div class="docs-list">${items.join("")}</div></nav></div>`;
}

function pageBody(page) {
  if (page.chapter) return tutorialBody(page);
  const where = `site/pages/${page.file === "index.html" ? "home.html" : page.file}`;
  const main = render(read(SITE, where.replace(/^site\//, "")), helpers, where);
  const entries = [];
  if (page.key === "examples") {
    for (const g of exampleLevels.filter((g) => g.examples.length)) { entries.push({ group: g.title }); for (const e of g.examples) entries.push({ id: `example-${e.key}`, title: e.title }); }
  } else {
    entries.push({ group: page.lead });
    for (const s of sections(main, where)) { if (s.group) entries.push({ group: s.group }); entries.push(s); }
  }
  if (page.key !== "examples") bars.set(page.key, pageBar(entries));   // the examples have their own, over their cards
  return `<div class="ic-page ic-split" data-tab="${page.key}">${sideList(entries, page.list ?? "Contents")}<main class="ic-main" id="${page.key}-content" tabindex="-1" aria-label="${esc(page.title)}">\n${main}\n</main></div>`;
}

// The page bar a page opens with (app/src/ui/pagebar.js makes the same, and takes this one over): at its first
// section, its group over it, ‹ with nowhere to go, and the line of its sections by group with the first filled.
// Written here so it is there at the first paint; made by the script it would arrive a moment late and move the page down.
const bars = new Map();
function pageBar(entries) {
  const groups = [];
  for (const e of entries) {
    if (e.id == null) groups.push({ title: e.group, n: 0 });
    else { if (!groups.length) groups.push({ title: "", n: 0 }); groups.at(-1).n++; }
  }
  const nonEmpty = groups.filter((g) => g.n);
  const first = entries.find((e) => e.id != null), eyebrow = groups.find((g) => g.n)?.title ?? "";
  let k = 0;
  const line = nonEmpty.map((g, i) => `<span class="pb-grp${i === 0 ? " here" : ""}" style="flex-grow: ${Math.max(1, g.n)};" title="${esc(g.title)}">${
    Array.from({ length: g.n }, () => `<button class="pb-tick" type="button" tabindex="-1" aria-hidden="true"><i style="width: ${k++ === 0 ? 100 : 0}%;"></i></button>`).join("")}</span>`).join("");
  return `<div class="pb pb-eased"><div class="pb-row">`
    + `<button class="pb-nav pb-prev" type="button" aria-label="Previous" disabled>${appIcon("chevron-left", "")}</button>`
    + `<button class="pb-now" type="button" title="Contents" aria-haspopup="true" aria-expanded="false"><span class="pb-words"><span class="pb-eyebrow"${eyebrow ? "" : " hidden"}>${esc(eyebrow)}</span><span class="pb-main">${esc(first?.title ?? "")}</span></span>${appIcon("chevron-down", "pb-jump")}</button>`
    + `<button class="pb-nav pb-next" type="button" aria-label="Next">${appIcon("chevron-right", "")}</button>`
    + `<span class="pb-said" aria-live="polite"></span></div><div class="pb-extra"></div><div class="pb-line"${nonEmpty.length ? "" : " hidden"}>${line}</div></div>`;
}

// ── each page: the app's own document, the page in its Info card and the site bar's tabs as links ──
const shell = read(ROOT, "app/src/index.html");
const pages = [...PAGES, ...TUTORIAL];
const bodies = new Map(pages.map((p) => [p.key, pageBody(p)]));
const idsOf = (html) => [...new Set([...html.replace(/<svg[\s\S]*?<\/svg>/g, "").matchAll(/\sid="([^"]+)"/g)].map((m) => m[1]))];   // an id inside a drawing (a mask, a glyph) is the drawing's own
// which page holds which anchor: an old link (sonic-pi.net/#mac, #learn) finds its page (main.js); and the editor's own
// address, code.html, the Code tab's (CODE)
const map = { pages: pages.map((p) => ({ key: p.key, file: p.file, ...(p.tab ? { tab: p.tab } : {}), ids: idsOf(bodies.get(p.key)) })), code: { file: CODE.file, title: CODE.title } };
// the pages a visitor has seen stay in the one document (info.js), so an id is the site's, not a page's
{ const seen = new Map(); for (const p of map.pages) for (const id of p.ids) { if (seen.has(id)) throw new Error(`#${id} is on both ${seen.get(id)} and ${p.file}: an id names one place on the site`); seen.set(id, p.file); } }

// the bar: the site's pages, then Code (the editor, code.html) — one row of tabs, lit alike (info.js lightTab)
// each tab says where it goes on hover (tooltip.js; a screen reader hears it as the link's description)
// Home is a wave at the row's start on a phone (the editor's code and the palette at its end are icons too); a wide
// screen has the wordmark for it
const tab = (key, file, title, tip, current) => key === "about"
  ? `<a class="ic-tab ic-tab-home${key === current ? " active" : ""}" href="${file}" data-tab="${key}" title="${esc(`${title}: ${tip}`)}" aria-label="${esc(title)}"${key === current ? ' aria-current="page"' : ""}>${icon("wave-sine", { class: "tb-icon home-glyph" })}</a>`
  : `<a class="ic-tab${key === current ? " active" : ""}" href="${file}" data-tab="${key}" title="${esc(tip)}"${key === current ? ' aria-current="page"' : ""}>${title}</a>`;
function tabs(current) {
  // Home, Examples, Learn, the Tutorial (its first chapter; every chapter lights it), Support; the editor is the code
  // icon beside the palette (index.html)
  const row = [...PAGES.slice(0, 3), { key: "tutorial", file: TUTORIAL[0].file, title: "Tutorial", tip: "From your first beep to live coding a set, a chapter at a time" }, ...PAGES.slice(3)];
  return row.map((p) => tab(p.key, p.file, p.title, p.tip, current)).join("");
}

// a page's document, or the editor's (code.html): the same shell, the editor's with Home in its card, put away, so λ
// opens on it at once and the editor is in reach
function document(page, { code = false } = {}) {
  const swap = (html, from, to) => { if (!html.includes(from)) throw new Error(`app/src/index.html has no ${from}`); return html.replace(from, () => to); };
  let html = shell;
  html = swap(html, "<title>Sonic Pi</title>", `<title>${esc(code ? CODE.title : page.key === "about" ? "Sonic Pi" : `${page.title} · Sonic Pi`)}</title>`);
  html = html.replace(/<meta name="description" content="[^"]*">/, `<meta name="description" content="${esc(code ? CODE.description : page.description)}">`);
  // the page's own stylesheet ahead of the app's: the app's win a tie
  html = swap(html, '<link rel="stylesheet" href="app.css">', '<link rel="stylesheet" href="site/css/landing.css">\n<link rel="stylesheet" href="app.css">');
  const head = [`<script type="application/json" id="site-map">${JSON.stringify({ ...map, page: page.key })}</script>`];
  html = swap(html, "</head>", `${head.join("\n")}\n</head>`);
  html = swap(html, '<div class="ic-tabs"></div>', `<div class="ic-tabs">${tabs(code ? "code" : page.tab ?? page.key)}</div>`);
  if (code) html = swap(html, '<a class="sn-code"', '<a class="sn-code active" aria-current="page"');   // the bar's code icon, lit in the editor
  if (!code && bars.has(page.key)) html = swap(html, '<div id="site-strip">\n</div>', `<div id="site-strip" class="on">${bars.get(page.key)}</div>`);
  html = swap(html, '<a class="skip-link" href="#app">', `<a class="skip-link" href="#${code ? "main" : `${page.key}-content`}">`);
  if (!code) {
    html = swap(html, "<body>", '<body class="info-open">');
    // the editor behind the page: out of reach of a keyboard and a screen reader while the page covers it (main.js
    // lifts this as the editor comes forward)
    for (const part of ['<div id="toolbar"', '<div id="main"', '<footer id="statusbar"']) html = swap(html, part, `${part} inert`);
    html = swap(html, '<div id="info-card" hidden>', '<div id="info-card">');
  }
  html = swap(html, '<div id="info-body" class="ic-body"></div>', `<div id="info-body" class="ic-body"><div class="site-body">${bodies.get(page.key)}</div></div>`);
  return html;
}

// ── write it all ──
fs.rmSync(OUT, { recursive: true, force: true });
fs.mkdirSync(OUT, { recursive: true });
for (const p of pages) fs.writeFileSync(path.join(PAGES_OUT, p.file), document(p));
fs.writeFileSync(path.join(PAGES_OUT, CODE.file), document(pages.find((p) => p.key === "about"), { code: true }));

fs.cpSync(path.join(SITE, "css"), path.join(OUT, "css"), { recursive: true });
fs.cpSync(path.join(SITE, "media"), path.join(OUT, "media"), { recursive: true });

fs.mkdirSync(path.join(CACHE, "yt"), { recursive: true });
fs.mkdirSync(path.join(OUT, "media/yt"), { recursive: true });
for (const id of videos) {
  // YouTube's 1280×720 still where the video has one (a sharp poster on a wide card or a retina screen), else its
  // 480×360 (letterboxed, soft once it fills a card)
  const f = path.join(CACHE, "yt", `${id}-hd.jpg`);
  if (!fs.existsSync(f)) {
    for (const size of ["maxresdefault", "hqdefault"]) {
      const r = await fetch(`https://i.ytimg.com/vi/${id}/${size}.jpg`).catch(() => null);
      if (r?.ok) { fs.writeFileSync(f, Buffer.from(await r.arrayBuffer())); break; }
    }
  }
  if (fs.existsSync(f)) fs.copyFileSync(f, path.join(OUT, "media/yt", `${id}.jpg`));
  else console.warn(`no thumbnail for video ${id}`);
}

// The tutorial's article pictures the pages show live in this repository, under etc/doc/images: taken from there
// rather than kept twice
const lost = [];
for (const body of bodies.values()) {
  for (const [rel] of body.matchAll(/site\/(media\/[A-Za-z0-9/._%-]+)/g).map((m) => [m[1]])) {
    const to = path.join(OUT, decodeURIComponent(rel));
    if (fs.existsSync(to)) continue;
    const from = path.join(NATIVE, "etc/doc/images", decodeURIComponent(rel).replace(/^media\/images\//, ""));
    if (fs.existsSync(from)) { fs.mkdirSync(path.dirname(to), { recursive: true }); fs.copyFileSync(from, to); }
    else lost.push(rel);
  }
}
if (lost.length) console.warn(`${lost.length} images the pages ask for are nowhere: ${lost.slice(0, 3).join(", ")}`);

console.log(`built ${pages.map((p) => p.file).join(", ")} in ${PAGES_OUT}, and ${OUT}; ${videos.size} videos`);
