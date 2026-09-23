// SPDX-License-Identifier: AGPL-3.0-or-later
// Themes, as native Sonic Pi does them: a port of app/gui/model/sonicpitheme.cpp.
// A scheme is a map of named colours (web/theme/themes.json, read out of the
// native source by scripts/gen-themes.mjs). Every colour the page uses goes
// through the same global transforms native applies — invert, hue spread,
// hue rotation, monochrome — and lands as a CSS custom property named after
// its native key: --Foreground, --SymbolForeground, --Scope ...

const PREFS_KEY = "sp-theme";
const VARS_KEY = "sp-theme-vars";   // the colours last applied, as CSS variables: a page puts them back before its first paint
const HUE_BUCKET = 15;
let themes = null;
const state = { scheme: "dark", hue: 0, spread: 0, monochrome: false, invert: false };
let spread = { base: -1, even: new Map() };
let derived = {};   // the roles apply() computes from the scheme (softForeground, subtleFill ...)
const listeners = new Set();

// ── Colours ───────────────────────────────────────────────────────────────

export function parseColour(s) {
  s = s.trim();
  if (s.startsWith("#")) {
    let h = s.slice(1);
    if (h.length === 3) h = h.split("").map((c) => c + c).join("");
    return { r: parseInt(h.slice(0, 2), 16), g: parseInt(h.slice(2, 4), 16), b: parseInt(h.slice(4, 6), 16), a: 1 };
  }
  const m = /rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*([\d.]+))?\s*\)/.exec(s);
  if (!m) throw new Error(`theme: unreadable colour ${s}`);
  return { r: +m[1], g: +m[2], b: +m[3], a: m[4] === undefined ? 1 : +m[4] };
}

export const cssColour = ({ r, g, b, a }) => (a >= 1 ? `#${[r, g, b].map((n) => n.toString(16).padStart(2, "0")).join("")}` : `rgba(${r}, ${g}, ${b}, ${a})`);

function toHsv({ r, g, b }) {
  const max = Math.max(r, g, b), min = Math.min(r, g, b), d = max - min;
  let h = -1;
  if (d !== 0) {
    if (max === r) h = 60 * (((g - b) / d) % 6);
    else if (max === g) h = 60 * ((b - r) / d + 2);
    else h = 60 * ((r - g) / d + 4);
    h = Math.round(h);
    if (h < 0) h += 360;
    h %= 360;
  }
  return { h, s: max === 0 ? 0 : Math.round((d * 255) / max), v: max };
}

function fromHsv(h, s, v, a) {
  const S = s / 255, V = v / 255, C = V * S, X = C * (1 - Math.abs(((h / 60) % 2) - 1)), m = V - C;
  const [r, g, b] = h < 60 ? [C, X, 0] : h < 120 ? [X, C, 0] : h < 180 ? [0, C, X] : h < 240 ? [0, X, C] : h < 300 ? [X, 0, C] : [C, 0, X];
  return { r: Math.round((r + m) * 255), g: Math.round((g + m) * 255), b: Math.round((b + m) * 255), a };
}

export function blend(a, b, t) {
  return { r: Math.round(a.r * (1 - t) + b.r * t), g: Math.round(a.g * (1 - t) + b.g * t), b: Math.round(a.b * (1 - t) + b.b * t), a: 1 };
}

export const luma601 = (c) => 0.299 * c.r + 0.587 * c.g + 0.114 * c.b;
// WCAG's contrast ratio between two colours (its relative luminance, 1.4.3)
const lum = (c) => { const f = (x) => { x /= 255; return x <= 0.03928 ? x / 12.92 : ((x + 0.055) / 1.055) ** 2.4; }; return 0.2126 * f(c.r) + 0.7152 * f(c.g) + 0.0722 * f(c.b); };
export const contrastRatio = (a, b) => { const [x, y] = [lum(a), lum(b)].sort((m, n) => n - m); return (x + 0.05) / (y + 0.05); };
// the colour moved towards `toward` (the text colour) just far enough to read at `ratio` against every ground it sits
// on: unchanged where it already does, so a scheme that passes looks as it did
function legible(c, toward, grounds, ratio) {
  for (let t = 0; t <= 1; t += 0.02) { const x = blend(c, toward, t); if (grounds.every((g) => contrastRatio(x, g) >= ratio)) return x; }
  return toward;
}
export const contrastingText = (bg) => (luma601(bg) >= 140 ? { r: 0, g: 0, b: 0, a: 1 } : { r: 255, g: 255, b: 255, a: 1 });

const bucketOf = (hue) => (Math.floor((hue + HUE_BUCKET / 2) / HUE_BUCKET) * HUE_BUCKET) % 360;
const hueDelta = (from, to) => ((to - from + 540) % 360) - 180;

function rebuildHueSpread(raw) {
  spread = { base: -1, even: new Map() };
  const accent = toHsv(parseColour(raw.HighlightedBackground));
  if (accent.h < 0 || accent.s <= 0) return;
  spread.base = accent.h;
  const buckets = new Set();
  for (const v of Object.values(raw)) {
    const c = toHsv(parseColour(v));
    if (c.h >= 0 && c.s >= 25) buckets.add(bucketOf(c.h));
  }
  if (!buckets.size) return;
  const pivot = bucketOf(spread.base);
  const ordered = [...buckets].sort((a, b) => ((a - pivot + 360) % 360) - ((b - pivot + 360) % 360));
  const n = ordered.length;
  ordered.forEach((bucket, i) => spread.even.set(bucket, i === 0 ? spread.base : (spread.base + Math.floor((i * 360) / n)) % 360));
}

function applyHueSpread(c, amount) {
  if (amount === 0 || spread.base < 0) return c;
  const { h, s, v } = toHsv(c);
  if (h < 0 || s <= 0) return c;
  let anchor = bucketOf(h), best = 360;
  for (const key of spread.even.keys()) {
    const d = Math.abs(hueDelta(h, key));
    if (d < best) { best = d; anchor = key; }
  }
  const even = spread.even.get(anchor) ?? h;
  const target = h + Math.round(hueDelta(h, even) * (amount / 100));
  return fromHsv(((target % 360) + 360) % 360, s, v, c.a);
}

/** Native's transform pipeline: invert, hue spread, hue rotation, monochrome. */
export function transform(c, st = state) {
  if (st.invert) c = { r: 255 - c.r, g: 255 - c.g, b: 255 - c.b, a: c.a };
  c = applyHueSpread(c, st.spread);
  if (st.hue % 360 !== 0) {
    const { h, s, v } = toHsv(c);
    if (h >= 0 && s > 0) c = fromHsv((h + st.hue) % 360, s, v, c.a);
  }
  if (st.monochrome) {
    const y = Math.round(0.2126 * c.r + 0.7152 * c.g + 0.0722 * c.b);
    c = { r: y, g: y, b: y, a: c.a };
  }
  return c;
}

/** A scheme's colour as it would show with the adjustments now in force (hue, spread, monochrome, invert): the
 * theme cards' previews, as native's refreshThemeCards re-previews them. The spread is the previewed scheme's own. */
export function preview(schemeId, key) {
  const raw = themes.schemes[schemeId]?.colours;
  if (!raw?.[key]) return null;
  const was = spread;
  rebuildHueSpread(raw);
  const c = cssColour(transform(parseColour(raw[key])));
  spread = was;
  return c;
}
/** Native's Reset Theme: hue, spread, monochrome and invert back to their defaults; the scheme is kept. */
export const resetMods = () => set({ hue: 0, spread: 0, monochrome: false, invert: false });

// ── The theme ─────────────────────────────────────────────────────────────

// Until a scheme is chosen in the preferences, the theme follows the OS: asking for more contrast is High Contrast,
// its dark mode Dark, its light mode Light, and a change while the page is open is followed too. A choice, once
// made, is kept and followed instead.
const osDark = typeof matchMedia === "function" ? matchMedia("(prefers-color-scheme: dark)") : null;
const osMore = typeof matchMedia === "function" ? matchMedia("(prefers-contrast: more)") : null;
let chosen = false;
const osScheme = () => (osMore?.matches ? "high_contrast" : osDark && !osDark.matches ? "light" : "dark");

export async function loadThemes(base = "./") {
  themes = await (await fetch(new URL("theme/themes.json", new URL(base, location.href)))).json();
  let saved = null;
  try { saved = JSON.parse(localStorage.getItem(PREFS_KEY) || "null"); } catch {}
  if (saved) { Object.assign(state, saved); chosen = !!saved.scheme; }
  if (!chosen) state.scheme = osScheme();
  if (!themes.schemes[state.scheme]) state.scheme = "dark";
  apply();
  for (const q of [osDark, osMore]) q?.addEventListener?.("change", () => { if (!chosen) { state.scheme = osScheme(); apply(); } });
  return themes;
}

export const schemes = () => Object.entries(themes.schemes).map(([id, s]) => ({ id, name: s.name, colours: s.colours }));
export const settings = () => ({ ...state });
export const onChange = (fn) => (listeners.add(fn), () => listeners.delete(fn));

/** A colour of the current scheme, transformed, as a {r,g,b,a}. */
export function colour(key) {
  const raw = themes.schemes[state.scheme].colours[key];
  return transform(parseColour(raw ?? "#808080"));
}

/** A colour as CSS: a scheme key, or one of the roles derived from them. */
export function css(key) {
  if (derived[key]) return cssColour(derived[key]);   // a role, or a scheme grey folded onto the ladder (apply)
  if (themes.schemes[state.scheme].colours[key] !== undefined) return cssColour(colour(key));
  return derived[key] ? cssColour(derived[key]) : "#808080";
}

export function set(changes) {
  chosen = true;   // a choice made in the preferences: kept from here on, the OS's mode no longer followed
  Object.assign(state, changes);
  state.hue = ((Math.round(state.hue) % 360) + 360) % 360;
  state.spread = Math.max(0, Math.min(100, Math.round(state.spread)));
  try { localStorage.setItem(PREFS_KEY, JSON.stringify(state)); } catch {}
  apply();
}

function apply() {
  const scheme = themes.schemes[state.scheme];
  rebuildHueSpread(scheme.colours);
  const root = document.documentElement.style;
  for (const key of Object.keys(scheme.colours)) root.setProperty(`--${key}`, css(key));
  // Native's derived roles (SonicPiTheme::softForeground … accentContrastText)
  const fg = colour("WindowForeground"), pane = colour("PaneBackground"), accent = colour("HighlightedBackground");
  const roles = {
    softForeground: blend(fg, pane, 0.1), mutedForeground: blend(fg, pane, 0.3),
    faintForeground: blend(fg, pane, 0.62), ghostForeground: blend(fg, pane, 0.82),
    subtleFill: blend(pane, fg, 0.07), accentTint: blend(pane, accent, 0.06),
    accentTintStrong: blend(pane, accent, 0.14), accentContrastText: contrastingText(accent),
    tabSelectedText: contrastingText(colour("TabSelected")),
    // a surface floating over the panels (the completion popup): a step towards the
    // foreground, a bigger one in the dark, where a shadow alone says nothing
    raisedSurface: blend(pane, fg, luma601(pane) < 128 ? 0.11 : 0.04),
  };
  // The surfaces, a ladder from the pane: every scheme the same three steps, so a column beside a band beside a
  // popup reads as three depths in the dark as in the light (a dark scheme needs bigger steps to be seen).
  //   surface1  a band of the page: every other section of a site page
  //   surface2  a column or strip beside the page: the submenu, the site's foot, bars (= raisedSurface)
  //   surface3  floating over the page: popups, tooltips, chips' fills
  const dark = luma601(pane) < 128;
  roles.surface1 = blend(pane, fg, dark ? 0.06 : 0.03);
  roles.surface2 = blend(pane, fg, dark ? 0.11 : 0.07);
  roles.surface3 = blend(pane, fg, dark ? 0.18 : 0.11);
  roles.raisedSurface = roles.surface2;
  // Text in the derived greys (faintText: faint as text, not as what is drawn — the piano roll's keys, the process
  // tree), and the line numbers. The schemes keep their own; High Contrast is the one whose every word reads at
  // WCAG's enhanced ratio (7:1) on every ground it has, stepped so faint < muted < soft stays true: the page's
  // conforming presentation, a click away in the palette (and the first one where the system asks for more contrast)
  roles.faintText = roles.faintForeground;
  roles.gutterText = colour("MarginForeground");
  if (state.scheme === "high_contrast") {
    const grounds = [pane, colour("Background"), roles.surface1];
    roles.faintText = legible(roles.faintForeground, fg, grounds, 7);
    roles.mutedForeground = legible(roles.mutedForeground, fg, grounds, 7.5);
    roles.softForeground = legible(roles.softForeground, fg, grounds, 8);
    roles.gutterText = legible(roles.gutterText, fg, [colour("MarginBackground")], 7);
  }
  // One palette of greys: the ground, the three surfaces, and the controls' outline (Button). Every other grey a
  // scheme or a rule would bring folds onto a step of the ladder, so no two near-greys ever sit side by side:
  //   a well in the page (a faceplate's group, the editor's current line, a chip's fill)   surface1
  //   a bar or a line (the drawer's divider, the pane's header, every 1px border)          surface2
  roles.subtleFill = roles.surface1;
  roles.CaretLineBackground = roles.surface1;
  roles.WindowBorder = roles.surface2;
  const sel = colour("SelectionBackground");
  root.setProperty("--selectionWash", `rgba(${sel.r}, ${sel.g}, ${sel.b}, 0.4)`);
  derived = roles;
  for (const [k, v] of Object.entries(roles)) root.setProperty(`--${k}`, cssColour(v));
  root.setProperty("--flashWash", `rgba(${accent.r}, ${accent.g}, ${accent.b}, 0.35)`);
  const bg = colour("Background");
  document.documentElement.style.colorScheme = luma601(bg) < 128 ? "dark" : "light";
  document.documentElement.dataset.scheme = state.scheme;
  updateDocCss(scheme.doc);
  // what was just applied, for the next page's head to put back before it paints (app/src/index.html)
  try { localStorage.setItem(VARS_KEY, JSON.stringify({ scheme: state.scheme, colorScheme: document.documentElement.style.colorScheme, vars: [...root].filter((k) => k.startsWith("--")).map((k) => [k, root.getPropertyValue(k)]) })); } catch {}
  for (const fn of listeners) fn(state);
}

// ── Doc styles: native's doc-styles.css with the scheme's colours swapped in ─

const docCssCache = new Map();
let docStyle = null;

async function updateDocCss(docId) {
  if (!docCssCache.has(docId)) {
    docCssCache.set(docId, fetch(`theme/doc/${docId}/doc-styles.css`).then((r) => r.text()).catch(() => ""));
  }
  const text = await docCssCache.get(docId);
  docStyle ??= Object.assign(document.createElement("style"), { id: "sp-doc-css" });
  if (!docStyle.isConnected) document.head.appendChild(docStyle);
  docStyle.textContent = scopeCss(docCss(text), ".sp-doc");
}

/** SonicPiTheme::getCss: the doc CSS's hard-coded colours become the scheme's. */
export function docCss(text) {
  const accent = css("HighlightedBackground"), number = css("NumberForeground");
  let c = text.replaceAll("deeppink", accent).replaceAll("dodgerblue", number).replaceAll("#5d99f3", number)
    .replaceAll("#FBDE2D", css("KeywordForeground")).replaceAll("darkorange", css("DemotedKeywordForeground"));
  const tn = (hex) => cssColour(transform(parseColour(hex)));
  for (const hex of ["#808080", "#9a9a9a", "#5e5e5e", "#e8e8e8", "#032c7f", "#32517f"]) c = c.replaceAll(hex, tn(hex));
  c = c.replace(/#444\b/g, tn("#444444")).replace(/#333\b/g, tn("#333333"));
  // the doc's page colour (its body's background: black in the dark doc, white in the light) is the scheme's Background,
  // and its other the Foreground — inverted too, so the docs invert with the rest of the page
  const page = /background-color:\s*(black|white)\b/.exec(text)?.[1] ?? "black";
  const to = { [page]: cssColour(colour("Background")), [page === "black" ? "white" : "black"]: cssColour(colour("Foreground")) };
  return c.replace(/(?<![\w-])(black|white)(?![\w-])/g, (w) => to[w]);   // the colours, not white-space
}

/** Prefix every rule's selectors so the doc CSS styles only the docs. */
function scopeCss(text, scope) {
  return text.replace(/\/\*[\s\S]*?\*\//g, "").split("}").map((rule) => {
    const i = rule.indexOf("{");
    if (i < 0) return "";
    const sel = rule.slice(0, i).trim();
    if (!sel || sel.startsWith("@")) return rule + "}";
    if (/^h[1-6]$/.test(sel)) return "";   // headings are the app's (shared.css): one heading rule on every page
    const scoped = sel.split(",").map((s) => (s.trim() === "body" || s.trim() === "html" ? scope : `${scope} ${s.trim()}`)).join(", ");
    return `${scoped} ${rule.slice(i)}}`;
  }).join("\n");
}
