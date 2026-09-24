// SPDX-License-Identifier: AGPL-3.0-or-later
// The site's live cards (info.js): each example's code, in the page as a <pre>,
// made a card — the code editable (and Reset), ▶ playing it through the app's
// engine on a scope slot of its own, and "Open in Sonic Pi" showing the editor
// with that code, the sound carrying on since the engine is one.
import { createCard } from "./ui/card.js";
import { createDeck } from "./ui/deck.js";

const STORE = "sp-site-card:";   // a card's edit, kept until Reset
const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };

/**
 * A page's examples as cards, each in its <pre>'s place: the Examples page's grid and the hero's one (the page has
 * them as <pre class="sp-card" data-key data-title data-blurb data-wide><code>, scripts/build-site.mjs). A card
 * keeps its block's id, so a link to it (#example-haunted) still lands on it.
 * @param pres   the <pre> elements, in page order
 * @param hooks  { play(code, {scopeSlot}) → job|null, stop(job), scopeFrame(slot, n), open(code, job) — the editor with this code (job: the card's run, if playing) }
 * @param host   the scrolling element they live in (Escape there stops a playing card)
 * @param root   the document or shadow root the cards live in
 */
// a block's code: a card the build wrote (scripts/build-site.mjs, ui/card-html.js) a line to a .cm-line, or a <pre>'s <code>
const codeOf = (block) => (block.matches("pre") ? (block.querySelector("code") ?? block).textContent : [...block.querySelectorAll(".cm-line")].map((l) => l.textContent).join("\n"));

export function mountExamples(pres, hooks, host, root = document) {
  const deck = createDeck(hooks, host);
  for (const pre of pres) {
    const d = pre.dataset;
    const card = deck.add(createCard({ title: d.title, code: codeOf(pre), blurb: d.blurb ?? "", key: d.key, actions: ["edit", "reset", "copy"], open: hooks.open, hooks, remember: STORE, root, wide: "wide" in d }));
    if (pre.id) card.el.id = pre.id;
    pre.replaceWith(card.el);
    playIfPressed(card);
  }
  return withReveal(deck);
}

/**
 * A page's code blocks as cards, each in the block's place (Learn's teaser). The block's highlighted code stays as
 * the card's body until it is clicked into. A card is named for the heading above it, numbered within its section.
 * @param pres   the <pre> elements, in page order
 */
export function mountSnippets(pres, hooks, host, root) {
  const deck = createDeck(hooks, host);
  const counts = new Map();
  pres.forEach((pre, i) => {
    let h = pre.previousElementSibling;
    while (h && !/^H[1-4]$/.test(h.tagName)) h = h.previousElementSibling;
    const section = pre.dataset.title ?? h?.textContent.trim().replace(/\s+/g, " ") ?? "Example";   // a block may name itself (a page's teaser)
    const n = (counts.get(section) ?? 0) + 1; counts.set(section, n);
    pre.querySelectorAll(".copy-button").forEach((b) => b.remove());   // the page's own copy button: the card has one
    const slot = el("div");   // the card takes the block into its body, so the block's place is held first
    pre.replaceWith(slot);
    const card = deck.add(createCard({ title: n > 1 ? `${section} · ${n}` : section, code: codeOf(pre).replace(/^\n/, "").replace(/\n$/, ""), key: pre.dataset.key ?? `snippet-${i}`, hooks, actions: ["edit", "reset", "copy"], open: hooks.open, root, playable: !("still" in pre.dataset) }));   // a fragment (data-still) to read, not a program to play   // a <pre><code> keeps a leading newline the page never shows
    slot.replaceWith(card.el);
    playIfPressed(card);
  });
  return withReveal(deck);
}

// a card whose Play was pressed before the script had made it (the page's own few lines, scripts/build-site.mjs): played
// now, as the press asked
function playIfPressed(card) {
  if (!window.spPendingPlay || window.spPendingPlay !== card.key) return;
  window.spPendingPlay = null;
  queueMicrotask(() => card.onRun?.());
}

// the deck's api as the pages know it, with the card element for a key
const withReveal = (deck) => Object.assign(deck, { reveal: (key) => deck.find(key)?.el ?? null });
