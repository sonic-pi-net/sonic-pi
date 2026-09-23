// SPDX-License-Identifier: AGPL-3.0-or-later
// The page bar: where you are in a page, and the way through it — one bar for every page on a phone (Home, Learn
// and the tutorial in the strip under the tabs; the examples over their cards, ./flip.js). ‹ and › step to the
// section or card either side; the middle names the group and where you are, and a tap on it opens the page's
// whole list; under them a line, a tick an entry in groups set apart, filled as far as you have come.
//
//   const bar = createPageBar({ onPrev, onNext, onJump, onGroup: (i) => … });
//   host.append(bar.el);
//   bar.groups([{ title, n }], (k) => …);            // the ticks, n a group; a tap on one goes to entry k
//   bar.set({ eyebrow, main, prev, next, said });    // the words, and whether ‹ and › have somewhere to go
//   bar.fill(f);                                     // how far through, in items (a fraction mid-way)
//
// adopt: a bar already in the page (the site's build writes one, so it is there at the first paint rather than
// arriving with the script and moving the page down), its parts taken over rather than made.
//
// The rules are ./pagebar.css, loaded by the app and by the site pages' shadow root alike.
import { icon } from "../icons.js";

const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };

export function createPageBar({ onPrev = null, onNext = null, onJump = null, onGroup = null, label = "Contents", adopt = null } = {}) {
  const bar = adopt ?? el("div", "pb");
  const part = (cls) => bar.querySelector(`.${cls}`);
  const prev = adopt ? part("pb-prev") : el("button", "pb-nav pb-prev");
  const next = adopt ? part("pb-next") : el("button", "pb-nav pb-next");
  const now = adopt ? part("pb-now") : el("button", "pb-now");
  const eyebrow = adopt ? part("pb-eyebrow") : el("span", "pb-eyebrow"), main = adopt ? part("pb-main") : el("span", "pb-main");
  const extra = adopt ? part("pb-extra") : el("div", "pb-extra");   // a host's own line under the words (the flip's hint)
  const said = adopt ? part("pb-said") : el("span", "pb-said");
  const line = adopt ? part("pb-line") : el("div", "pb-line");
  if (!adopt) {   // the same parts as scripts/build-site.mjs writes (pageBar)
    prev.type = "button"; prev.innerHTML = icon("chevron-left", ""); prev.setAttribute("aria-label", "Previous");
    next.type = "button"; next.innerHTML = icon("chevron-right", ""); next.setAttribute("aria-label", "Next");
    now.type = "button"; now.title = label; now.setAttribute("aria-haspopup", "true");
    const words = el("span", "pb-words");
    words.append(eyebrow, main);
    now.append(words);
    now.insertAdjacentHTML("beforeend", icon("chevron-down", "pb-jump"));
    said.setAttribute("aria-live", "polite");
    const row = el("div", "pb-row");
    row.append(prev, now, next, said);
    bar.append(row, extra, line);
  }
  prev.addEventListener("click", () => onPrev?.());
  next.addEventListener("click", () => onNext?.());
  now.addEventListener("click", () => onJump?.());

  let ticks = [], groupEls = [], groupOf = [];
  const api = {
    el: bar, extra,
    /** The line: a tick an entry, in groups ([{ title, n }]) set a little further apart; a tap on a tick goes to it. */
    groups(gs, onTick = null) {
      const key = gs.map((g) => `${g.title}:${g.n}`).join("|");
      if (key === line.dataset.key) return;
      line.dataset.key = key;
      line.textContent = "";
      ticks = []; groupEls = []; groupOf = [];
      gs.forEach((g, i) => {
        const grp = el("span", "pb-grp");
        grp.style.flexGrow = String(Math.max(1, g.n));
        for (let j = 0; j < g.n; j++) {
          const k = ticks.length;
          const tick = el("button", "pb-tick");
          tick.type = "button";
          tick.tabIndex = -1;   // the list is the keyboard's way to an entry; the ticks are a finger's
          tick.setAttribute("aria-hidden", "true");
          const fill = el("i");
          tick.append(fill);
          tick.addEventListener("click", () => (onTick ? onTick(k) : onGroup?.(i)));
          grp.append(tick);
          ticks.push(fill); groupOf.push(i);
        }
        grp.title = g.title;
        line.append(grp);
        groupEls.push(grp);
      });
      line.hidden = gs.length === 0;
    },
    /** How far through: f entries in (0 the first; a fraction between two), each tick filled as far as it goes. */
    fill(f) {
      ticks.forEach((t, k) => { t.style.width = `${Math.max(0, Math.min(1, f + 1 - k)) * 100}%`; });
      const here = groupOf[Math.max(0, Math.min(groupOf.length - 1, Math.round(f)))];
      groupEls.forEach((g, i) => g.classList.toggle("here", i === here));
    },
    /** The words, and whether ‹ and › lead anywhere. */
    set({ eyebrow: e = "", main: m = "", prev: p = true, next: n = true, prevTitle = "", nextTitle = "", said: s = null, open = false } = {}) {
      eyebrow.textContent = e; eyebrow.hidden = !e;
      main.textContent = m;
      prev.disabled = !p; next.disabled = !n;
      prev.title = prevTitle; next.title = nextTitle;
      prev.setAttribute("aria-label", prevTitle ? `Previous: ${prevTitle}` : "Previous");
      next.setAttribute("aria-label", nextTitle ? `Next: ${nextTitle}` : "Next");
      now.classList.toggle("open", open); now.setAttribute("aria-expanded", String(open));
      if (s != null && s !== said.textContent) said.textContent = s;
    },
  };
  return api;
}
