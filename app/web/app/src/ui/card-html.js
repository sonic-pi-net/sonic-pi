// SPDX-License-Identifier: AGPL-3.0-or-later
// A card as HTML, with no document: the site's build (scripts/build-site.mjs) writes every card into its page this way,
// so a page looks as it will before its script has run — the same elements and classes ./card.js makes, the code
// highlighted a line at a time as the editor draws it — and the script's card takes its place without anything moving.
// ./card.js's transport draws the same Play and Stop from here. It is named by aria-label, not by its title's id: the
// same card can be on two pages (the home page's example is the first of Examples), and an id names one place on the site.
import { icon } from "../icons.js";
import { highlightLinesHTML } from "../highlight.js";

export const TRANSPORT = `<svg viewBox="0 0 24 24" aria-hidden="true">
  <path class="qs-disc" d="M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0" />
  <path class="qs-glyph qs-glyph-play" d="M15.5 12l-5 -4v8l5 -4" />
</svg>`;
export const TRANSPORT_STOP = `<svg viewBox="0 0 24 24" aria-hidden="true">
  <path class="qs-disc" d="M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0" />
  <path class="qs-glyph qs-glyph-stop" d="M9.5 9.5h5v5h-5z" />
</svg>`;

const ESC = { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" };
const esc = (x) => String(x).replace(/[&<>"]/g, (c) => ESC[c]);
const LABELS = { edit: "Edit", reset: "Reset", copy: "Copy", open: "Open in Sonic Pi" };
const button = (name) => `<button class="qs-card-action qs-card-${name}" type="button" title="${LABELS[name]}" aria-label="${LABELS[name]}"${name === "edit" ? ' aria-pressed="false"' : ""}>${icon(name, "")}</button>`;

/**
 * A card, as ./card.js makes it and shows it before it is played or edited.
 * @param o.title  its name, in its bar
 * @param o.code   the program
 * @param o.attrs  what the script reads it by: { key, title, blurb, wide, still, kind } as data-* (./site.js)
 * @param o.id     its element's id (#example-haunted), kept by the card the script makes
 * @param o.blurb, o.wide, o.playable, o.level: as ./card.js's createCard
 */
export function cardHTML({ title, code, attrs = {}, id = null, blurb = "", wide = false, playable = true, level = 2 }) {
  const lines = highlightLinesHTML(code.replace(/\s+$/, ""));
  const cls = `qs-card sp-card${wide || lines.length > 40 ? " qs-wide" : ""}${playable ? "" : " qs-still"}`;
  const data = Object.entries(attrs).filter(([, v]) => v != null && v !== false).map(([k, v]) => (v === true ? ` data-${k}` : ` data-${k}="${esc(v)}"`)).join("");
  return `<section class="${cls}"${id ? ` id="${esc(id)}"` : ""}${data} role="group" aria-label="${esc(title)}">` +
    `<header class="qs-card-head"><h${level} class="qs-card-title">${esc(title)}</h${level}>${["edit", "reset", "copy", "open"].map(button).join("")}</header>` +
    `<div class="qs-card-body" tabindex="0" role="group" aria-label="${esc(title)}, code"><div class="cm-editor sp-code qs-static"><div class="cm-scroller"><div class="cm-content">` +
    lines.map((l) => `<div class="cm-line">${l || "<br>"}</div>`).join("") +
    `</div></div></div></div>` +
    `<footer class="qs-card-foot"><div class="qs-foot-main"><p class="qs-blurb">${blurb ? `${esc(blurb)} ` : ""}<span class="qs-state"></span></p><div class="qs-out" role="log" aria-live="polite"></div></div>` +
    `<div class="qs-transport"${playable ? "" : " hidden"}><div class="qs-scope qs-scope-play"><button class="qs-run" type="button" title="Play">${TRANSPORT}<span class="sr-only">Play</span></button></div>` +
    `<div class="qs-scope qs-scope-stop"><button class="qs-run qs-stop" type="button" title="Stop" disabled>${TRANSPORT_STOP}<span class="sr-only">Stop</span></button></div></div></footer>` +
    `</section>`;
}
