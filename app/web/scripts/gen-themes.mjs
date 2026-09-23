#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// Writes web/theme/themes.json: native Sonic Pi's colour schemes, read out of
// its GUI source, so the web GUI's themes are native's and follow native when
// it changes. Also copies native's three doc stylesheets.
//
//   node scripts/gen-themes.mjs [path/to/sonic-pi]     # default: the checkout this directory is in
//
// What it reads, from app/gui/model/sonicpitheme.cpp:
//   lightTheme, darkTheme, highContrastTheme   `QString x = "..."` palettes and
//                                              `themeSettings["Key"] = x;` maps
//   neutralBaseTheme                           dark, desaturated (computed here as native does)
//   mildDarkTheme, phosphorTheme, signalTheme  `const QString a="..", b="..";` palettes,
//                                              `paintDarkGroundBackgrounds(t, bg);` and
//                                              `t["Key"]=x;` overrides, in order
// Anything it cannot read is an error, so a change in native's source is
// noticed rather than silently dropped.
import fs from "node:fs";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const SP = path.resolve(process.argv[2] ?? path.join(ROOT, "../.."));
const GUI = path.join(SP, "app/gui");
const src = fs.readFileSync(path.join(GUI, "model/sonicpitheme.cpp"), "utf8");

// The colour names native's maps use, as QColor reads them.
const NAMED = {
  white: "#ffffff", black: "#000000", red: "#ff0000", deeppink: "#ff1493", darkorange: "#ff8c00",
  transparent: "#00000000", gold: "#ffd700", dodgerblue: "#1e90ff", grey: "#808080", gray: "#808080",
};

function parseColour(v) {
  let s = v.trim().toLowerCase();
  if (NAMED[s]) s = NAMED[s];
  if (!s.startsWith("#")) throw new Error(`gen-themes: unknown colour ${JSON.stringify(v)}`);
  let h = s.slice(1);
  if (h.length === 3) h = h.split("").map((c) => c + c).join("");
  if (h.length === 6) return { a: 255, r: parseInt(h.slice(0, 2), 16), g: parseInt(h.slice(2, 4), 16), b: parseInt(h.slice(4, 6), 16) };
  if (h.length === 8) return { a: parseInt(h.slice(0, 2), 16), r: parseInt(h.slice(2, 4), 16), g: parseInt(h.slice(4, 6), 16), b: parseInt(h.slice(6, 8), 16) };
  throw new Error(`gen-themes: unreadable colour ${JSON.stringify(v)}`);
}

const hex2 = (n) => n.toString(16).padStart(2, "0");
// CSS-ready: #rrggbb when opaque, rgba() otherwise (QColor's #AARRGGBB is not CSS).
function css({ a, r, g, b }) {
  return a === 255 ? `#${hex2(r)}${hex2(g)}${hex2(b)}` : `rgba(${r}, ${g}, ${b}, ${+(a / 255).toFixed(3)})`;
}

function body(name) {
  const head = new RegExp(`SonicPiTheme::${name}\\s*\\(\\s*\\)\\s*\\{`);
  const m = head.exec(src);
  if (!m) throw new Error(`gen-themes: no ${name} in sonicpitheme.cpp`);
  let depth = 1, i = m.index + m[0].length;
  const start = i;
  for (; i < src.length && depth > 0; i++) {
    if (src[i] === "{") depth++;
    else if (src[i] === "}") depth--;
  }
  return src.slice(start, i - 1).replace(/\/\/[^\n]*/g, "");
}

// A plain scheme: palette variables, then the map.
function plainScheme(name) {
  const b = body(name);
  const vars = {};
  for (const m of b.matchAll(/QString\s+(\w+)\s*=\s*(?:"([^"]*)"|(\w+))\s*;/g)) {
    vars[m[1]] = m[2] !== undefined ? m[2] : vars[m[3]];
    if (vars[m[1]] === undefined) throw new Error(`gen-themes: ${name}: ${m[1]} = ${m[3]} is unknown`);
  }
  const map = {};
  for (const m of b.matchAll(/themeSettings\[\s*"(\w+)"\s*\]\s*=\s*(?:"([^"]*)"|(\w+))\s*;/g)) {
    const v = m[2] !== undefined ? m[2] : vars[m[3]];
    if (v === undefined) throw new Error(`gen-themes: ${name}: ${m[1]} = ${m[3]} is unknown`);
    map[m[1]] = v;
  }
  if (Object.keys(map).length < 50) throw new Error(`gen-themes: ${name}: only ${Object.keys(map).length} keys read`);
  return map;
}

// neutralBaseTheme: dark with every value desaturated, white capped at 212.
function neutralBase(dark) {
  const t = {};
  for (const [k, v] of Object.entries(dark)) {
    const c = parseColour(v);
    const y = Math.min(Math.round(0.2126 * c.r + 0.7152 * c.g + 0.0722 * c.b), 212);
    t[k] = css({ a: c.a, r: y, g: y, b: y });
  }
  return t;
}

const groundKeys = (() => {
  const m = /paintDarkGroundBackgrounds[^{]*\{[\s\S]*?keys\[\]\s*=\s*\{([\s\S]*?)\};/.exec(src);
  if (!m) throw new Error("gen-themes: no paintDarkGroundBackgrounds key list");
  return [...m[1].matchAll(/"(\w+)"/g)].map((x) => x[1]);
})();

// A derived scheme: neutral base, palette constants, then paints and overrides in order.
function derivedScheme(name, base) {
  const b = body(name);
  if (!/neutralBaseTheme\(\)/.test(b)) throw new Error(`gen-themes: ${name} is not built on neutralBaseTheme`);
  const vars = {};
  for (const decl of b.matchAll(/const\s+QString\s+([^;]+);/g)) {
    for (const m of decl[1].matchAll(/(\w+)\s*=\s*"([^"]*)"/g)) vars[m[1]] = m[2];
  }
  const t = { ...base };
  const steps = [];
  for (const m of b.matchAll(/paintDarkGroundBackgrounds\s*\(\s*t\s*,\s*(\w+)\s*\)/g)) steps.push({ at: m.index, paint: m[1] });
  for (const m of b.matchAll(/t\[\s*"(\w+)"\s*\]\s*=\s*(?:"([^"]*)"|(\w+))\s*;/g)) steps.push({ at: m.index, key: m[1], lit: m[2], ref: m[3] });
  steps.sort((x, y) => x.at - y.at);
  for (const s of steps) {
    if (s.paint) {
      const v = vars[s.paint];
      if (v === undefined) throw new Error(`gen-themes: ${name}: paint with unknown ${s.paint}`);
      for (const k of groundKeys) t[k] = v;
    } else {
      const v = s.lit !== undefined ? s.lit : vars[s.ref];
      if (v === undefined) throw new Error(`gen-themes: ${name}: ${s.key} = ${s.ref} is unknown`);
      t[s.key] = v;
    }
  }
  return t;
}

const cssMap = (m) => Object.fromEntries(Object.entries(m).map(([k, v]) => [k, css(parseColour(v))]));

const dark = plainScheme("darkTheme");
const base = neutralBase(dark);
const schemes = {
  light: { name: "Light", colours: cssMap(plainScheme("lightTheme")), doc: "light" },
  dark: { name: "Dark", colours: cssMap(dark), doc: "dark" },
  high_contrast: { name: "High Contrast", colours: cssMap(plainScheme("highContrastTheme")), doc: "high_contrast" },
  mild_dark: { name: "Mild Dark", colours: cssMap(derivedScheme("mildDarkTheme", base)), doc: "dark" },
  phosphor: { name: "Phosphor", colours: cssMap(derivedScheme("phosphorTheme", base)), doc: "dark" },
  signal: { name: "Signal", colours: cssMap(derivedScheme("signalTheme", base)), doc: "dark" },
};

const git = (...args) => { try { return execFileSync("git", ["-C", SP, ...args], { encoding: "utf8" }).trim(); } catch { return null; } };
const out = path.join(ROOT, "web/theme");
fs.mkdirSync(path.join(out, "doc"), { recursive: true });
for (const d of ["light", "dark", "high_contrast"]) {
  fs.mkdirSync(path.join(out, "doc", d), { recursive: true });
  fs.copyFileSync(path.join(GUI, "theme", d, "doc-styles.css"), path.join(out, "doc", d, "doc-styles.css"));
}
fs.writeFileSync(path.join(out, "themes.json"), JSON.stringify({
  source: { repo: path.relative(ROOT, SP) || ".", branch: git("branch", "--show-current"), commit: git("rev-parse", "--short", "HEAD"), file: "app/gui/model/sonicpitheme.cpp" },
  schemes,
}, null, 1) + "\n");
console.log(`themes.json: ${Object.entries(schemes).map(([k, s]) => `${k} (${Object.keys(s.colours).length})`).join(", ")}; doc styles copied`);
