#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// The picture a link to the site shows on social media (og:image, twitter:image; scripts/build-site.mjs names it):
// by default the chosen design, Sonic Pi's logo with the name and the address beneath it (logo-name); the others are
// kept to compare against (--options). Each is drawn from Sonic Pi's own pieces — the logo as its EPS draws it
// (app/gui/images/logo.eps), the logo's lettering, native's toolbar glyphs, a quickstart card as the site draws one
// (app/src/ui/card-html.js), the site's own CSS and theme — photographed at 1200×630, the shape every network crops a
// link's picture to, at twice the pixels, for a phone's screen. Written to site/media/images/social-card.png and committed: run it again
// when the card or the site's look changes.
//
//   node scripts/serve.mjs &                                        # the site, built (build-app, build-site)
//   node scripts/build-social-card.mjs [--design card-dark] [--base http://127.0.0.1:8460/web/]
//   node scripts/build-social-card.mjs --options <dir>              # every design, <dir>/<design>.png, to choose from
import fs from "node:fs";
import path from "node:path";
import { cardHTML } from "../app/src/ui/card-html.js";
import { logo } from "./lib/site/logo.mjs";

const ROOT = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const arg = (name, fallback) => { const i = process.argv.indexOf(name); return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback; };
const BASE = arg("--base", "http://127.0.0.1:8460/web/");
const OPTIONS = arg("--options", null);
const DESIGN = arg("--design", "logo-name");   // the one chosen (2026-09-24): the logo, the name, the address
const OUT = path.join(ROOT, "site/media/images/social-card.png");
const LOGO_FILE = path.join(ROOT, "../../app/gui/images/logo-square.svg");
const mark = logo(LOGO_FILE);
// native's toolbar glyphs, its "pro" ones at full size (masks: only their shape is used)
const GUI_IMAGES = path.join(ROOT, "../../app/gui/images");
const dataURL = (file) => `data:image/png;base64,${fs.readFileSync(file).toString("base64")}`;
const TOOLBAR = (name) => dataURL(path.join(GUI_IMAGES, "toolbar-fullsize/pro", `${name}.png`));
// Sonic Pi's logo as its EPS draws it (app/gui/images/logo.eps, a Quartz EPS that sets its glyphs in type): the tile
// (its path and fill) and each glyph (font, size, baseline), read from the page's drawing, in its own units, y down.
// The type is macOS's own (Marion, Cochin), so the card is made on a Mac.
function epsLogo(file) {
  const eps = fs.readFileSync(file, "latin1");
  const [, W, H] = /%%BoundingBox: 0 0 (\d+) (\d+)/.exec(eps).map(Number);
  const NAMES = { pi: "π", parenright: ")" };
  const fonts = eps.split("%RBIBeginFontSubset: ").slice(1).map((sub) => ({ name: /^\S+/.exec(sub)[0], chars: Object.fromEntries([...sub.matchAll(/dup (\d+) \/(\w+) put/g)].map(([, code, name]) => { if (!NAMES[name]) throw new Error(`logo.eps: no character for its glyph ${name}`); return [String.fromCharCode(+code), NAMES[name]]; })) }));
  const flip = (x, y) => `${+x} ${H - y}`;
  let fill = null, d = "", tile = null, font = null, size = 0, text = null, at = null;
  const glyphs = [];
  for (const line of eps.slice(eps.indexOf("%%EndPageSetup")).split("\n").map((l) => l.trim())) {
    let m;
    if ((m = /^([\d.]+) ([\d.]+) ([\d.]+) sc$/.exec(line))) fill = m.slice(1).map(Number);
    else if ((m = /^1 0 0 -1 (\S+) (\S+) cm$/.exec(line))) text = { e: +m[1], f: +m[2] };   // type's space: y down from f
    else if ((m = /^\/F(\d+)\.\d+\[ (\S+) /.exec(line))) { font = fonts[m[1] - 1]; size = +m[2]; }
    else if ((m = /^(\S+) (\S+) m$/.exec(line))) { if (text) at = [+m[1] + text.e, +m[2] - text.f + H]; else d += `M${flip(m[1], m[2])}`; }
    else if ((m = /^(\S+) (\S+) l$/.exec(line))) d += `L${flip(m[1], m[2])}`;
    else if ((m = /^(\S+) (\S+) (\S+) (\S+) (\S+) (\S+) c$/.exec(line))) d += `C${flip(m[1], m[2])} ${flip(m[3], m[4])} ${flip(m[5], m[6])}`;
    else if (line === "h") d += "Z";
    else if (line === "f") { if (fill && fill.some((v) => v < 1)) tile = { d, fill: `rgb(${fill.map((v) => Math.round(v * 255)).join(" ")})` }; d = ""; }   // white is the page behind it
    else if ((m = /^\((.)\)s$/.exec(line))) glyphs.push({ family: font.name.split("-")[0], italic: /Italic/.test(font.name), bold: /Bold/.test(font.name), size, x: at[0], y: at[1], char: font.chars[m[1]] });
  }
  if (!tile || !glyphs.length) throw new Error("logo.eps: no tile, or no glyphs, where the reader looks for them");
  return { W, H, tile, glyphs };
}
const LOGO = epsLogo(path.join(GUI_IMAGES, "logo.eps"));
const glyphText = (g, fill = "#fff") => `<text x="${g.x}" y="${g.y}" font-family="${g.family}" font-size="${g.size}"${g.italic ? ' font-style="italic"' : ""}${g.bold ? ' font-weight="bold"' : ""} fill="${fill}">${g.char}</text>`;
// "Sonic Pi" as the logo letters it: the logo's own wordmark glyphs alone (its viewBox fitted to them in the page)
const WORDMARK = (() => {
  const svg = fs.readFileSync(LOGO_FILE, "utf8");
  const open = /<svg[^>]*>/.exec(svg)[0], defs = /<defs>[\s\S]*?<\/defs>/.exec(svg)?.[0] ?? "";
  const words = [...svg.matchAll(/<g fill="rgb\(100%, 100%, 100%\)" fill-opacity="1">\s*<use xlink:href="#glyph-4[\s\S]*?<\/g>/g)].map((m) => m[0].replace('fill="rgb(100%, 100%, 100%)"', 'fill="currentColor"')).join("");
  return `${open.replace("<svg ", '<svg class="sc-wordmark" role="img" aria-label="Sonic Pi" ')}${defs}${words}</svg>`;
})();
const LOGO_SVG = `<svg viewBox="0 0 ${LOGO.W} ${LOGO.H}" role="img" aria-label="Sonic Pi's logo"><path d="${LOGO.tile.d}" fill="${LOGO.tile.fill}"/>${LOGO.glyphs.map((g) => glyphText(g)).join("")}</svg>`;
const { chromium } = await import(process.env.PLAYWRIGHT ?? "playwright");

// the programs: a beat and a melody side by side (what Sonic Pi is, at a glance), and one loop for the big type
const TWO_LOOPS = `live_loop :drums do
  sample :bd_haus, amp: 2
  sleep 0.5
end

live_loop :melody do
  use_synth :prophet
  play scale(:e3, :minor_pentatonic).choose
  sleep 0.25
end`;
const ONE_LOOP = `live_loop :bleeps do
  play scale(:e3, :minor_pentatonic).choose
  sleep 0.25
end`;
const TAGLINE = "Code music, live — now in your browser.";

// each design: its scheme, and the page it lays out (the card's markup made here, the rest in the page's CSS)
const card = (o) => cardHTML({ playable: true, level: 1, ...o });
const DESIGNS = {
  "card-dark": { scheme: "dark", what: "The card, dark: two loops, the tagline in its foot", html: () => `<main class="sc sc-card">${card({ title: "Sonic Pi", code: TWO_LOOPS, blurb: TAGLINE })}<p class="sc-url">sonic-pi.net</p></main>` },
  "card-light": { scheme: "light", what: "The same card in the light theme", html: () => `<main class="sc sc-card">${card({ title: "Sonic Pi", code: TWO_LOOPS, blurb: TAGLINE })}<p class="sc-url">sonic-pi.net</p></main>` },
  "card-logo": { scheme: "dark", what: "The card with Sonic Pi's mark in its title bar", html: () => `<main class="sc sc-card sc-with-logo">${card({ title: "Sonic Pi", code: TWO_LOOPS, blurb: TAGLINE })}<p class="sc-url">sonic-pi.net</p></main>`, logo: true },
  "split": { scheme: "dark", what: "The mark and the words on the left, a smaller card on the right", html: () => `<main class="sc sc-split"><div class="sc-side"><div class="sc-mark">${mark.punched}</div><h1>Sonic Pi</h1><p>${TAGLINE}</p><p class="sc-url">sonic-pi.net</p></div>${card({ title: "Pentatonic Bleeps", code: ONE_LOOP, blurb: "" })}</main>` },
  "big-code": { scheme: "dark", what: "One loop in big type: the code is the picture", html: () => `<main class="sc sc-big">${card({ title: "Sonic Pi", code: ONE_LOOP, blurb: TAGLINE })}</main>` },

  // ── the mark and Sonic Pi's own world: less a card, more what Sonic Pi looks and sounds like ──
  "pink-field": { scheme: "dark", what: "Sonic Pi pink, edge to edge: the π and its arcs, big, in white", html: () => `<main class="sc ab ab-pink"><div class="ab-glyph">${mark.glyph}</div><div class="ab-words"><h1>Sonic Pi</h1><p>Code music, live.</p></div><p class="ab-url">sonic-pi.net</p></main>` },
  "sound-arcs": { scheme: "dark", what: "The logo's arcs carry on across the frame as sound, pink into blue", html: () => `<main class="sc ab ab-arcs"><canvas width="2400" height="1260"></canvas><div class="ab-glyph">${mark.glyph}</div><div class="ab-words"><h1>Sonic Pi</h1><p>Code music, live.</p></div><p class="ab-url">sonic-pi.net</p></main>`, draw: "arcs" },
  "arcs-zoom": { scheme: "dark", what: "Sound arcs, the left half zoomed in: the π and its arcs twice the size, the name big beneath", html: () => `<main class="sc ab ab-arcs ab-zoom"><canvas width="2400" height="1260"></canvas><div class="ab-glyph">${mark.glyph}</div><div class="ab-words"><h1>Sonic Pi</h1><p>Code music, live.</p></div><p class="ab-url">sonic-pi.net</p></main>`, draw: "arcs" },
  "invite-play": { scheme: "dark", what: "For a link from a collaborator: the π and its sound, and the card's Play waiting in it", html: () => `<main class="sc ab ab-arcs ab-zoom ab-invite"><canvas width="2400" height="1260"></canvas><div class="ab-glyph">${mark.glyph}</div>${PLAY}<div class="ab-words"><h1>Sonic Pi</h1><p>Open it. Press play.</p></div><p class="ab-url">sonic-pi.net</p></main>`, draw: "arcs" },
  "invite-headline": { scheme: "dark", what: "For a link from a collaborator: “Press play.” is the headline, Sonic Pi signs it", html: () => `<main class="sc ab ab-arcs ab-headline"><canvas width="2400" height="1260"></canvas><div class="ab-glyph">${mark.glyph}</div><div class="ab-words"><h1>Press play.</h1><p>Someone made you some music in Sonic Pi.</p></div><p class="ab-url">sonic-pi.net</p></main>`, draw: "arcs" },
  // arcs-huge again, as asked (2026-09-24): the arcs are the logo's own bracket, grown on out by the logo's own ratio
  // (no drawn arcs beside it), pink morphing into Sonic Pi blue across the frame, no shadow — the arcs fade under
  // the name instead — and the name in three faces to choose between
  "arcs-v2-wordmark": { scheme: "dark", what: "The logo's own brackets carried on, pink into blue; the name as the logo letters it", html: () => ARCS_V2(`<div class="v2-name v2-wordmark">${WORDMARK}</div>`), brackets: true },
  "arcs-v2-hack": { scheme: "dark", what: "The same, the name in Hack, Sonic Pi's code font", html: () => ARCS_V2(`<div class="v2-name v2-hack">Sonic Pi</div>`), brackets: true },
  "arcs-v2-light": { scheme: "dark", what: "The same, the name in a light sans", html: () => ARCS_V2(`<div class="v2-name v2-light">Sonic Pi</div>`), brackets: true },
  // Sonic Pi's logo in the middle — where a network cropping to a square cuts — "Sonic Pi" beneath it; to its right its
  // brackets carried on, huge, pink into Sonic Pi blue; to its left the toolbar's glyphs, growing out the same way,
  // blue back into pink
  "icon-glyphs": { scheme: "dark", what: "Sonic Pi's logo in the middle (what a square crop keeps), the name beneath; its brackets carried on huge to the right; the toolbar's glyphs growing out to the left", html: () => ICON_FRAME(64), iconArcs: true },
  "icon-glyphs-big": { scheme: "dark", what: "The same, the glyphs bigger, running off the left edge as the arcs run off the right", html: () => ICON_FRAME(92), iconArcs: true },
  // the logo, the name and the address, and nothing else: centred, a little larger than beside the arcs
  "logo-name": { scheme: "dark", what: "Just Sonic Pi's logo, the name beneath it and the address, centred, room round the words", html: () => `<main class="sc ci ln"><div class="ln-logo">${LOGO_SVG}</div>${WORDMARK}<p class="v2-url">sonic-pi.net</p></main>`, wordmark: true },
  "arcs-huge": { scheme: "dark", what: "Sound arcs, zoomed further: the π and its arcs fill the height, the name in the corner", html: () => `<main class="sc ab ab-arcs ab-huge"><canvas width="2400" height="1260"></canvas><div class="ab-glyph">${mark.glyph}</div><div class="ab-words"><h1>Sonic Pi</h1><p>sonic-pi.net</p></div></main>`, draw: "arcs" },
  "scope": { scheme: "dark", what: "The app's scope: a pink trace and a blue one across the frame", html: () => `<main class="sc ab ab-scope"><canvas width="2400" height="1260"></canvas><div class="ab-tile">${mark.punched}</div><div class="ab-words"><h1>Sonic Pi</h1><p>Code music, live.</p></div><p class="ab-url">sonic-pi.net</p></main>`, draw: "scope" },
  "code-texture": { scheme: "dark", what: "Real Sonic Pi code, highlighted and dimmed, behind the pink tile", html: () => `<main class="sc ab ab-code"><pre class="ab-texture">${TEXTURE}</pre><div class="ab-centre"><div class="ab-tile">${mark.punched}</div><h1>Sonic Pi</h1><p>Code music, live.</p></div></main>` },
  "piano-roll": { scheme: "dark", what: "A pentatonic melody as the piano roll draws it, a playhead crossing it", html: () => `<main class="sc ab ab-roll"><canvas width="2400" height="1260"></canvas><div class="ab-words ab-top"><div class="ab-glyph ab-small">${mark.glyph}</div><h1>Sonic Pi</h1><p>Code music, live.</p></div><p class="ab-url">sonic-pi.net</p></main>`, draw: "roll" },
  // ── the home page's live synth: the real card, its dials and keys, moved into the frame as the page made it ──
  "synth-light": { scheme: "light", what: "The home page's live synth card, Pluck: its dials, its keys, its code", synth: { width: 884, scale: 1.24 } },
  "synth-dark": { scheme: "dark", what: "The same synth card in the dark theme", synth: { width: 884, scale: 1.24 } },
  "synth-split": { scheme: "dark", what: "The mark and the name on the left, the synth card beside them", synth: { width: 760, scale: 0.98, side: true } },
  "jam-pads": { scheme: "dark", what: "The jam pads, one lit, beside the mark: play code like an instrument", html: () => `<main class="sc ab ab-pads"><div class="ab-words"><div class="ab-glyph ab-small">${mark.glyph}</div><h1>Sonic Pi</h1><p>Code music, live.</p><p class="ab-url ab-inline">sonic-pi.net</p></div><div class="ab-grid">${[0, 1, 2, 3, 4, 5, 6, 7].map((i) => `<span class="${i === 2 ? "on" : i === 5 ? "hit" : ""}">${i}</span>`).join("")}</div></main>` },
};

// the brackets' frame: the glyph (π and brackets) in a full-frame layer the fade under the name masks, the name, the address
const ARCS_V2 = (name) => `<main class="sc ab v2"><div class="v2-art"><div class="v2-glyph">${mark.glyph}</div></div><div class="v2-words">${name}<p class="v2-url">sonic-pi.net</p></div></main>`;

// the logo's frame: the logo at ICON (css px), the arcs (ICON_ARCS) and glyphs in a layer the gradient colours
const ICON = { left: 440, top: 84, size: 320 };
const GLYPHS = ["run", "stop", "rec", "info", "help", "prefs"];   // the toolbar's, out from the logo: ▶ ■ ● λ Δ π
const ICON_FRAME = (first) => {
  // each glyph a little larger than the last, by the ratio the logo's brackets grow by (1.2), out to the left edge and
  // past it; the glyph's box is the middle 1090 px of native's 1719 (every glyph's drawn part is in it)
  let right = ICON.left - 34, size = first;
  const mid = ICON.top + ICON.size / 2;
  const glyphs = GLYPHS.map((g) => { const left = right - size, html = `<i class="ci-glyph" style="left: ${left}px; top: ${mid - size / 2}px; width: ${size}px; --glyph: url(${TOOLBAR(g)})"></i>`; right = left - size * 0.14; size *= 1.2; return html; });
  return `<main class="sc ci"><div class="ci-art"><svg class="ci-arcs" viewBox="0 0 1200 630" width="1200" height="630" aria-hidden="true"><g transform="translate(${ICON.left} ${ICON.top}) scale(${ICON.size / LOGO.W})"></g></svg>${glyphs.join("")}</div><div class="ci-icon" style="left: ${ICON.left}px; top: ${ICON.top}px; width: ${ICON.size}px">${LOGO_SVG}</div><div class="ci-name">${WORDMARK}<p class="v2-url">sonic-pi.net</p></div></main>`;
};

// the cards' Play, large: the pink disc and its ring, the triangle in white (app/src/ui/card.css .qs-run)
const PLAY = `<svg class="ab-play" viewBox="0 0 200 200" aria-hidden="true"><circle cx="100" cy="100" r="96" fill="none" stroke="rgba(255,255,255,0.28)" stroke-width="4"/><circle cx="100" cy="100" r="74" fill="#ff1493"/><path d="M84 66 L138 100 L84 134 Z" fill="#fff" stroke="#fff" stroke-width="8" stroke-linejoin="round"/></svg>`;

// the code-texture's code: Sonic Pi's own examples, highlighted as the editor does, many lines of them
const { highlightLinesHTML } = await import("../app/src/highlight.js");
const EXAMPLES = JSON.parse(fs.readFileSync(path.join(ROOT, "web/data/reference/examples.json"), "utf8")).groups.flatMap((g) => g.examples.map((e) => e.code));
const TEXTURE = highlightLinesHTML(EXAMPLES.join("\n").split("\n").filter((l) => l.trim() && !l.startsWith("#")).slice(0, 90).join("\n")).join("\n");

const CSS = `
  html, body { margin: 0; width: 1200px; height: 630px; overflow: hidden; background: var(--PaneBackground, var(--Background)); }
  .sc { box-sizing: border-box; width: 1200px; height: 630px; display: flex; }
  .sc .qs-card { flex: none; margin: 0; content-visibility: visible; }
  .sc .qs-card-action { display: none; }
  .sc .qs-card-body { overflow: visible; }
  .sc-url { margin: 0; font: 600 24px/1 var(--prose-font); letter-spacing: 0.04em; color: var(--mutedForeground); }
  /* the card, the whole picture */
  .sc-card { flex-direction: column; justify-content: center; gap: 14px; padding: 36px 56px; }
  .sc-card .qs-card { width: 100%; --qs-scope: 76px; }
  .sc-card .qs-card-title { font-size: 34px; }
  .sc-card .qs-card-body { font-size: 22px; padding: 10px 16px; }
  .sc-card .qs-blurb { font-size: 26px; }
  .sc-card .sc-url { text-align: right; }
  .sc-with-logo .qs-card-head { gap: 14px; }
  .sc-with-logo .qs-card-head .sc-title-mark { width: 44px; height: 44px; color: var(--accentContrastText); flex: none; }
  .sc-with-logo .qs-card-head .sc-title-mark svg { width: 100%; height: 100%; }
  /* the split */
  .sc-split { align-items: center; gap: 48px; padding: 48px 56px; }
  .sc-side { flex: 0 0 360px; display: flex; flex-direction: column; gap: 18px; }
  .sc-side .sc-mark { width: 150px; }
  .sc-side .sc-mark svg { width: 100%; height: auto; display: block; }
  .sc-side h1 { margin: 6px 0 0; font: 800 64px/1 var(--prose-font); color: var(--Foreground); }
  .sc-side p { margin: 0; font: 500 30px/1.25 var(--prose-font); color: var(--Foreground); }
  .sc-side .sc-url { margin-top: 6px; }
  .sc-split .qs-card { flex: 1; --qs-scope: 70px; }
  .sc-split .qs-card-title { font-size: 28px; }
  .sc-split .qs-card-body { font-size: 22px; padding: 14px 16px; }
  /* one loop, big */
  .sc-big { flex-direction: column; justify-content: center; padding: 40px 56px; }
  .sc-big .qs-card { width: 100%; --qs-scope: 96px; }
  .sc-big .qs-card-title { font-size: 40px; }
  .sc-big .qs-card-body { font-size: 36px; padding: 18px 22px; }
  .sc-big .qs-blurb { font-size: 30px; }
  /* ── the synth card ── */
  .sy { align-items: center; justify-content: center; }
  .sy-holder { flex: none; }
  .sy-holder .home-synth { margin: 0; }
  .sy-split { justify-content: flex-start; gap: 20px; padding: 0 44px 0 56px; }
  .sy-side { flex: 0 0 300px; display: flex; flex-direction: column; gap: 14px; }
  .sy-side .sy-mark { width: 120px; }
  .sy-side .sy-mark svg { display: block; width: 100%; height: auto; }
  .sy-side h1 { margin: 4px 0 0; font: 800 64px/1 var(--prose-font); color: var(--Foreground); }
  .sy-side p { margin: 0; font: 500 30px/1.2 var(--prose-font); color: var(--Foreground); }
  .sy-split .sy-holder { transform-origin: left center !important; }
  /* ── the abstract designs ── */
  .ab { position: relative; overflow: hidden; background: #000; color: #ededed; font-family: var(--prose-font); }
  .ab canvas { position: absolute; inset: 0; width: 1200px; height: 630px; }
  .ab h1 { margin: 0; font: 800 96px/0.95 var(--prose-font); letter-spacing: -0.03em; }
  .ab-arcs h1, .ab-arcs p { text-shadow: 0 0 18px #000, 0 0 36px #000, 0 0 6px #000; }   /* the arcs pass behind the words, not through them */
  .ab p { margin: 0; font: 500 38px/1.2 var(--prose-font); }
  .ab-url { position: absolute; right: 56px; bottom: 44px; font: 600 26px/1 var(--prose-font); letter-spacing: 0.04em; opacity: 0.8; }
  .ab-url.ab-inline { position: static; margin-top: 18px; opacity: 0.7; }
  .ab-glyph svg, .ab-tile svg { display: block; width: 100%; height: auto; }
  .ab-words { position: relative; display: flex; flex-direction: column; gap: 14px; }
  /* pink field */
  .ab-pink { background: #ff1493; color: #fff; align-items: center; gap: 56px; padding: 0 80px; }
  .ab-pink .ab-glyph { width: 460px; color: #fff; flex: none; }
  .ab-pink h1 { font-size: 118px; }
  .ab-pink .ab-url { opacity: 0.9; }
  /* arcs */
  .ab-arcs { align-items: flex-end; padding: 0 72px 64px; }
  .ab-arcs .ab-glyph { position: absolute; left: 72px; top: 70px; width: 250px; color: #ff1493; }
  .ab-zoom .ab-glyph { left: 56px; top: 22px; width: 470px; }
  .ab-invite .ab-play { position: absolute; right: 150px; top: 150px; width: 250px; height: 250px; filter: drop-shadow(0 0 40px rgba(255, 20, 147, 0.55)); }
  .ab-headline { align-items: flex-end; padding: 0 64px 70px; }
  .ab-headline .ab-glyph { position: absolute; left: 56px; top: 44px; width: 250px; }
  .ab-headline h1 { font-size: 150px; }
  .ab-headline p { font-size: 38px; opacity: 0.9; }
  .ab-zoom h1 { font-size: 124px; }
  .ab-zoom p { font-size: 44px; }
  .ab-huge { align-items: flex-end; justify-content: flex-end; padding: 0 64px 56px; }
  .ab-huge .ab-glyph { left: 40px; top: 50%; width: 760px; transform: translateY(-50%); }
  .ab-huge .ab-words { align-items: flex-end; gap: 8px; }
  .ab-huge h1 { font-size: 88px; }
  .ab-huge p { font-size: 30px; opacity: 0.8; letter-spacing: 0.03em; }
  .v2 { background: #000; }
  .v2-art { position: absolute; inset: 0; background: #000; isolation: isolate; }
  /* the morph: one gradient across the frame, Sonic Pi pink (the π, the first bracket) into Sonic Pi blue, laid over the
     white glyph by multiply — white takes the gradient's colour, black stays black */
  .v2-art::after { content: ""; position: absolute; inset: 0; background: linear-gradient(90deg, #ff1493 0%, #ff1493 28%, #4c83ff 92%); mix-blend-mode: multiply; }
  .v2-glyph { color: #fff; }
  .v2-glyph { position: absolute; left: 40px; top: 50%; width: 640px; transform: translateY(-62%); }   /* lifted: the name has the lower left, clear of the brackets */
  .v2-glyph svg { display: block; width: 100%; height: auto; }
  .v2-words { position: absolute; left: 64px; bottom: 44px; display: flex; flex-direction: column; align-items: flex-start; gap: 12px; color: #fff; }
  .v2-name { line-height: 1; }
  .v2-wordmark svg { display: block; width: 300px; height: auto; color: #fff; }
  .v2-hack { font: 700 64px/1 var(--code-font); letter-spacing: -0.01em; }
  .v2-light { font: 300 76px/1 var(--prose-font); letter-spacing: -0.02em; }
  .v2-url { margin: 0; font: 500 24px/1 var(--code-font); color: #4c83ff; letter-spacing: 0.02em; }
  .ci { background: #000; }
  .ci-art { position: absolute; inset: 0; background: #000; isolation: isolate; }
  /* blue at the edges, the logo's own pink at the middle */
  .ci-art::after { content: ""; position: absolute; inset: 0; background: linear-gradient(90deg, #4c83ff 0%, ${LOGO.tile.fill} 34%, ${LOGO.tile.fill} 64%, #4c83ff 100%); mix-blend-mode: multiply; }
  .ci-arcs { position: absolute; left: 0; top: 0; overflow: visible; }
  .ci-glyph { position: absolute; aspect-ratio: 1; background: #fff; -webkit-mask: var(--glyph) center / 157.7% no-repeat; mask: var(--glyph) center / 157.7% no-repeat; }
  .ci-icon { position: absolute; }
  .ci-icon svg { display: block; width: 100%; height: auto; overflow: visible; }
  .ci-name { position: absolute; left: 0; right: 0; top: ${ICON.top + ICON.size + 30}px; display: flex; flex-direction: column; align-items: center; gap: 18px; }
  .ci-name .sc-wordmark { width: 290px; }
  .ln { flex-direction: column; align-items: center; justify-content: center; }
  .ln-logo { width: 350px; margin-bottom: 46px; }
  .ln-logo svg { display: block; width: 100%; height: auto; }
  .ln .sc-wordmark { width: 330px; color: #fff; margin-bottom: 30px; }
  .ln .v2-url { font-size: 32px; }
  /* scope */
  .ab-scope { align-items: flex-end; gap: 40px; padding: 0 64px 60px; }
  .ab-scope .ab-tile { position: absolute; left: 64px; top: 56px; width: 120px; }
  /* code texture */
  .ab-code { align-items: center; justify-content: center; }
  .ab-texture { position: absolute; inset: -20px -40px; margin: 0; font: 22px/1.45 var(--code-font); opacity: 0.32; white-space: pre; transform: rotate(-4deg); }
  .ab-centre { position: relative; display: flex; flex-direction: column; align-items: center; gap: 16px; padding: 40px 72px 44px; border-radius: 28px; background: rgba(0, 0, 0, 0.82); box-shadow: 0 0 80px 40px rgba(0, 0, 0, 0.8); }
  .ab-centre .ab-tile { width: 150px; }
  .ab-centre p { opacity: 0.85; }
  /* piano roll */
  .ab-roll .ab-top { position: absolute; left: 64px; top: 52px; }
  .ab-small { width: 120px; color: #ff1493; }
  .ab-roll h1 { font-size: 84px; }
  .ab-roll p { font-size: 34px; }
  /* pads */
  .ab-pads { align-items: center; justify-content: space-between; padding: 0 80px; }
  .ab-grid { display: grid; grid-template-columns: repeat(4, 118px); gap: 18px; }
  .ab-grid span { display: flex; align-items: center; justify-content: center; aspect-ratio: 1; border: 4px solid #ff1493; border-radius: 20px; font: 500 48px/1 var(--code-font); color: #ff1493; }
  .ab-grid span.on { background: #ff1493; color: #000; }
  .ab-grid span.hit { border-color: #4c83ff; color: #4c83ff; box-shadow: 0 0 36px rgba(76, 131, 255, 0.55); }`;

// the canvases, drawn in the page at twice the pixels (one seeded random, so a design draws the same every time)
const DRAW = (kind) => {
  const c = document.querySelector("canvas"), g = c.getContext("2d");
  g.scale(2, 2);
  let seed = 7; const rnd = () => ((seed = (seed * 16807) % 2147483647) / 2147483647);
  const PINK = "#ff1493", BLUE = "#4c83ff", YELLOW = "#fbde2d", GREEN = "#61ce3c";
  if (kind === "arcs") {
    // the logo's arcs carried on out across the frame: continuing its third arc (the circle it lies on, and the logo's
    // own spacing between arcs, as fractions of the glyph's width, measured from logo-square.svg as drawn)
    const box = document.querySelector(".ab-glyph svg").getBoundingClientRect(), w = box.width;
    const cx = box.left + 0.59 * w, cy = box.top + 0.375 * w, r3 = 0.379 * w, step = 0.221 * w;
    const n = Math.ceil((1300 - cx - r3) / step) + 1;
    for (let i = 1; i <= n; i++) {
      const r = r3 + i * step, t = (i - 1) / Math.max(1, n - 1);
      g.strokeStyle = `rgba(${Math.round(255 * (1 - t) + 76 * t)}, ${Math.round(20 * (1 - t) + 131 * t)}, ${Math.round(147 * (1 - t) + 255 * t)}, ${0.95 - t * 0.7})`;
      g.lineWidth = (0.085 - t * 0.05) * w; g.lineCap = "round";
      const sweep = 0.78 + 0.22 * t;   // the inner arcs shorter: they pass above the name, not through it
      g.beginPath(); g.arc(cx, cy, r, -sweep, sweep); g.stroke();
    }
  }
  if (kind === "scope") {
    g.strokeStyle = "rgba(255,255,255,0.07)"; g.lineWidth = 1;
    for (let x = 0; x <= 1200; x += 60) { g.beginPath(); g.moveTo(x, 0); g.lineTo(x, 630); g.stroke(); }
    for (let y = 15; y <= 630; y += 60) { g.beginPath(); g.moveTo(0, y); g.lineTo(1200, y); g.stroke(); }
    const trace = (colour, width, f) => { g.strokeStyle = colour; g.lineWidth = width; g.shadowColor = colour; g.shadowBlur = 18; g.beginPath(); for (let x = 0; x <= 1200; x += 2) { const y = f(x); x ? g.lineTo(x, y) : g.moveTo(x, y); } g.stroke(); };
    trace(BLUE, 4, (x) => 300 + 70 * Math.sin(x / 38) * Math.sin(x / 310) + 22 * Math.sin(x / 9.5));
    trace(PINK, 6, (x) => 300 + 150 * Math.sin(x / 61) * (0.55 + 0.45 * Math.sin(x / 190)) + 18 * Math.sin(x / 13));
  }
  if (kind === "roll") {
    const rows = 14, top = 250, h = (630 - top - 40) / rows, colours = [PINK, PINK, BLUE, YELLOW, GREEN];
    g.fillStyle = "rgba(255,255,255,0.05)";
    for (let r = 0; r < rows; r += 2) g.fillRect(0, top + r * h, 1200, h);
    const penta = [0, 3, 5, 7, 10, 12, 15, 17, 19, 22, 24, 27, 29, 31];
    let x = 20, row = 6;
    while (x < 1180) {
      const len = [34, 34, 68, 34, 102][Math.floor(rnd() * 5)];
      row = Math.max(0, Math.min(rows - 1, row + Math.round((rnd() - 0.5) * 5)));
      g.fillStyle = colours[Math.floor(rnd() * colours.length)];
      g.globalAlpha = 0.9;
      g.beginPath(); g.roundRect(x, top + (rows - 1 - row) * h + 3, len - 6, h - 6, 5); g.fill();
      if (rnd() < 0.35) { const r2 = Math.min(rows - 1, row + 3); g.globalAlpha = 0.55; g.beginPath(); g.roundRect(x, top + (rows - 1 - r2) * h + 3, len - 6, h - 6, 5); g.fill(); }
      x += len;
    }
    g.globalAlpha = 1; g.fillStyle = PINK; g.shadowColor = PINK; g.shadowBlur = 20; g.fillRect(760, top - 20, 4, 630 - top);
  }
};

// The logo's brackets, carried on: the three it has measured as drawn (glyph-2, -1, -3, left to right), then copies of
// the largest, each grown by the ratio the logo grows by from its second to its third and set on by the spacing grown
// the same way, until they leave the frame (drawn in white: the layer's gradient colours them, .v2-art::after). The
// wordmark's viewBox fitted to its glyphs.
const BRACKETS = () => {
  const svg = document.querySelector(".v2-glyph svg");
  svg.style.overflow = "visible";
  const use = (id) => svg.querySelector(`use[*|href="#${id}"], use[href="#${id}"]`);
  const [b1, b2, b3] = ["glyph-2-0", "glyph-1-0", "glyph-3-0"].map((id) => use(id).getBBox());
  const ratio = b3.height / b2.height, cy = b3.y + b3.height / 2;
  const toUser = svg.getScreenCTM().inverse(), right = new DOMPoint(1260, 0).matrixTransform(toUser).x;
  const third = use("glyph-3-0").parentNode;
  let left = b3.x, gap = b3.x - b2.x, s = 1;
  for (let i = 0; i < 12 && left < right; i++) {
    gap *= ratio; left += gap; s *= ratio;
    const copy = third.cloneNode(true);
    copy.setAttribute("transform", `translate(${left} ${cy}) scale(${s}) translate(${-b3.x} ${-cy})`);
    third.parentNode.appendChild(copy);
  }
  const w = document.querySelector(".v2-wordmark svg");
  if (w) { const b = w.getBBox(); w.setAttribute("viewBox", `${b.x} ${b.y} ${b.width} ${b.height}`); w.removeAttribute("width"); w.removeAttribute("height"); }
};

// The logo's brackets, carried on out of its tile: each next one the last's glyph grown by the ratio the logo's last
// two grow by, set on by the gap they have (as a share of the size), its middle level with the last's. Those clear of
// the tile are drawn (white: the layer's gradient colours them); one it would half hide is a sliver stuck to its edge.
// The wordmark's viewBox fitted to its glyphs.
const ICON_ARCS = ({ brackets }) => {
  const g = document.querySelector(".ci-arcs g"), clear = document.querySelector(".ci-icon").getBoundingClientRect().right + 4;
  const [a, b] = brackets.slice(-2), r = b.size / a.size, gap = (b.x - a.x) / b.size;
  const el = [...document.querySelectorAll(".ci-icon text")].find((t) => +t.getAttribute("font-size") === b.size && t.textContent === ")");
  const box = el.getBBox(), middle = box.y + box.height / 2, below = (b.y - middle) / b.size;   // baseline below the middle, per size
  let t = { ...b }, drawn = 0;
  for (let i = 0; i < 24; i++) {
    const size = t.size * r;
    t = { ...t, size, x: t.x + gap * size, y: middle + below * size };
    const copy = el.cloneNode(true);
    copy.setAttribute("x", t.x); copy.setAttribute("y", t.y); copy.setAttribute("font-size", t.size); copy.setAttribute("fill", "#fff");
    g.appendChild(copy);
    const rect = copy.getBoundingClientRect();
    if (rect.left > 1200) { copy.remove(); break; }
    if (rect.left < clear) copy.remove(); else drawn++;
  }
  const w = document.querySelector(".sc-wordmark");
  if (w) { const bb = w.getBBox(); w.setAttribute("viewBox", `${bb.x} ${bb.y} ${bb.width} ${bb.height}`); w.removeAttribute("width"); w.removeAttribute("height"); }
  return { ratio: +r.toFixed(3), drawn };
};

async function render(browser, name, file) {
  const d = DESIGNS[name];
  if (!d) throw new Error(`no design ${name}: ${Object.keys(DESIGNS).join(", ")}`);
  const ctx = await browser.newContext({ viewport: { width: 1200, height: 630 }, deviceScaleFactor: 2 });
  await ctx.addInitScript((scheme) => { try { localStorage.setItem("sp-theme", JSON.stringify({ scheme })); localStorage.removeItem("sp-theme-vars"); } catch {} }, d.scheme);
  const page = await ctx.newPage();
  await page.goto(BASE + "index.html");   // the site's CSS and its theme's colours, as a visitor's page has them
  await page.waitForFunction(() => getComputedStyle(document.documentElement).getPropertyValue("--HighlightedBackground").trim() !== "");
  if (d.synth) {
    // the synth, live: the page makes it (info.js, docs.js createInstrument); moved, not copied, so it stays as drawn
    await page.waitForSelector(".home-synth.live .pg-face", { timeout: 30000 });
    await page.evaluate(({ css, synth, punched }) => {
      const s = document.querySelector(".home-synth.live");
      const frame = document.createElement("main");
      frame.className = `sc sy${synth.side ? " sy-split" : ""}`;
      if (synth.side) frame.innerHTML = `<div class="sy-side"><div class="sy-mark">${punched}</div><h1>Sonic Pi</h1><p>Code music, live.</p><p class="sc-url">sonic-pi.net</p></div>`;
      const holder = document.createElement("div");
      holder.className = "sy-holder";
      holder.style.cssText = `width: ${synth.width}px; transform: scale(${synth.scale}); transform-origin: center center;`;
      holder.append(s);
      frame.append(holder);
      document.body.className = "";
      document.body.replaceChildren(frame);
      const style = document.createElement("style");
      style.textContent = css;
      document.head.appendChild(style);
    }, { css: CSS, synth: d.synth, punched: mark.punched });
    await page.waitForTimeout(400);
  } else await page.evaluate(({ html, css, glyph, withLogo }) => {
    document.body.className = "";
    document.body.innerHTML = html;
    const style = document.createElement("style");
    style.textContent = css;
    document.head.appendChild(style);
    if (withLogo) { const m = document.createElement("span"); m.className = "sc-title-mark"; m.innerHTML = glyph; document.querySelector(".qs-card-head").prepend(m); }
  }, { html: d.html(), css: CSS, glyph: mark.glyph, withLogo: !!d.logo });
  if (d.draw) await page.evaluate(DRAW, d.draw);
  if (d.brackets) await page.evaluate(BRACKETS);
  if (d.wordmark) await page.evaluate(() => { const w = document.querySelector(".sc-wordmark"), b = w.getBBox(); w.setAttribute("viewBox", `${b.x} ${b.y} ${b.width} ${b.height}`); w.removeAttribute("width"); w.removeAttribute("height"); });   // fitted to its glyphs
  if (d.iconArcs) { await page.evaluate(() => document.fonts.ready); console.log(`${name}: the logo's brackets carried on`, await page.evaluate(ICON_ARCS, { brackets: LOGO.glyphs.filter((g) => g.char === ")").sort((x, y) => x.size - y.size) })); }
  await page.evaluate(() => document.fonts.ready);
  await page.waitForTimeout(300);
  // everything inside the frame: a card taller than the picture would be cut, which a check should say, not a viewer
  const over = await page.evaluate(() => [...document.querySelectorAll(".qs-card, .sc-url, .sc-side, .sy-holder, .sy-side, .home-synth")].some((e) => e.getBoundingClientRect().bottom > 630 || e.getBoundingClientRect().right > 1200));
  if (over) console.warn(`${name}: something runs past the 1200×630 frame`);
  fs.mkdirSync(path.dirname(file), { recursive: true });
  await page.screenshot({ path: file, clip: { x: 0, y: 0, width: 1200, height: 630 } });
  await ctx.close();
  console.log(`${name}: ${path.relative(process.cwd(), file)} (${Math.round(fs.statSync(file).size / 1024)} KB) — ${d.what}`);
}

const browser = await chromium.launch();
if (OPTIONS) {
  const only = arg("--only", null)?.split(",");   // just these, to look again at a few
  for (const name of Object.keys(DESIGNS).filter((n) => !only || only.includes(n))) await render(browser, name, path.join(OPTIONS, `${name}.png`));
  fs.writeFileSync(path.join(OPTIONS, "designs.json"), JSON.stringify(Object.fromEntries(Object.entries(DESIGNS).map(([k, d]) => [k, d.what]))));
} else {
  await render(browser, DESIGN, OUT);
}
await browser.close();
