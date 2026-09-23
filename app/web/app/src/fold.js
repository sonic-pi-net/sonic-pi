// SPDX-License-Identifier: AGPL-3.0-or-later
// A page's list, folded on a phone: the list (.ic-side) lives in a strip (.ic-fold-bar) that stays at the top
// as the page scrolls, behind a chip that drops it open over the page; an item picked, or a tap elsewhere,
// folds it away. On a wide screen the strip is the list's column and the chip is not shown. The site pages,
// the tutorial and the app's docs pane fold alike, from here and shared.css.
const el = (tag, cls) => { const e = document.createElement(tag); if (cls) e.className = cls; return e; };

/** Wraps `side` (given .ic-side) in the strip, in its place if it is in a document, and returns the strip. */
export function fold(side, label) {
  const bar = el("div", "ic-fold-bar"), chip = el("button", "ic-fold");
  side.classList.add("ic-side");
  if (side.parentNode) side.replaceWith(bar);
  bar.append(chip, side);
  chip.type = "button";
  chip.innerHTML = `<span>${label}</span><svg class="tb-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M6 9l6 6l6 -6"/></svg>`;
  chip.setAttribute("aria-expanded", "false");
  return wire(bar);
}

/** A strip already in the page (a site page's, as scripts/build-site.mjs writes it: .ic-fold-bar > .ic-fold, .ic-side), made to fold. */
export const adoptFold = (bar) => wire(bar);

function wire(bar) {
  const chip = bar.querySelector(":scope > .ic-fold"), side = bar.querySelector(":scope > .ic-side");
  const label = chip.querySelector("span").textContent;
  const set = (on) => { side.classList.toggle("open", on); bar.classList.toggle("open", on); chip.classList.toggle("open", on); chip.setAttribute("aria-expanded", String(on)); bar.dispatchEvent(new CustomEvent("fold", { detail: { open: on } })); };
  bar.foldLabel = label;
  bar.toggleFold = () => { set(!side.classList.contains("open")); watch(); };
  bar.closeFold = () => set(false);
  chip.addEventListener("click", () => set(!side.classList.contains("open")));
  const overlay = () => /^(absolute|fixed)$/.test(getComputedStyle(side).position);   // folded (a phone, or a dropdown): the list drops over the page (fixed under the app's strip: site.css)
  side.addEventListener("click", (e) => { if (e.target.closest(".docs-item, a") && overlay()) set(false); });
  // open, a tap anywhere else folds it (the root the strip is in: the document, or a shadow)
  bar.addEventListener("pointerdown", () => { bar.dataset.inside = "1"; });
  const root = () => bar.getRootNode();
  let listening = null;
  const away = (e) => { if (side.classList.contains("open") && !bar.contains(e.target)) set(false); };
  const watch = () => { const r = root(); if (r !== listening) { listening?.removeEventListener?.("pointerdown", away); listening = r; r.addEventListener("pointerdown", away); } };
  chip.addEventListener("click", watch);
  return bar;
}
