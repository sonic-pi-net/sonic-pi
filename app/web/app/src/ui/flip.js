// SPDX-License-Identifier: AGPL-3.0-or-later
// A flip: cards one at a time, down a column that snaps a card to the screen — scroll down for the next, as the rest
// of a page scrolls, the next card's head showing at the foot to say there is more (a tap on it goes there). Over it
// the page bar every page has (./pagebar.js): ‹ the level and the count ›, a tap on the middle for the host's whole
// list, and the line — a tick a card, filled as the column turns, frame by frame.
// The column is the host's element holding the cards (the Examples page's levels); whether it flips is ./card.css's
// to say (a phone's width): on a wide screen it is the host's grid and the bar is not shown.
//
//   const flip = createFlip(track, { levels: [{ title, keys }], card: (key) => el, onChange: (key) => … });
//   track.before(flip.el);   flip.go(key);   flip.on → whether it flips now
import { createPageBar } from "./pagebar.js";

/**
 * @param track  the element whose cards flip (it gains .flip; card.css lays it out)
 * @param o.levels   [{ title, keys }] the cards in order, by level
 * @param o.card     (key) → the card's element
 * @param o.title    (key) → the card's name
 * @param o.onChange (key) → void: a card came to the front
 * @param o.onJump   () → void: where you are, tapped (the host opens its whole list)
 */
export function createFlip(track, { levels, card, title, onChange = null, onJump = null }) {
  track.classList.add("flip");
  const keys = levels.flatMap((l) => l.keys);
  const levelOf = new Map(levels.flatMap((l, i) => l.keys.map((k) => [k, i])));
  let at = -1;

  const pb = createPageBar({ onPrev: () => step(-1), onNext: () => step(1), onJump, onGroup: (i) => go(levels[i].keys[0]), label: "All examples" });
  const bar = pb.el;
  bar.classList.add("flip-bar");
  pb.groups(levels.map((l) => ({ title: `${l.title}: ${l.keys.length} examples`, n: l.keys.length })), (k) => go(keys[k]));

  const on = () => track.scrollHeight > track.clientHeight + 1 && getComputedStyle(track).overflowY !== "visible";
  // where a card stands against the column's top, as drawn: under a zoom (the pages' on a wide screen) the layout's
  // offsets and the scroll's position are not in the same units in every browser, the drawn ones always are
  const rel = (c) => c.getBoundingClientRect().top - track.getBoundingClientRect().top - (parseFloat(getComputedStyle(track).paddingTop) || 0);
  // the card at the front: the one whose top is nearest the column's
  function front() {
    let best = 0, dist = Infinity;
    keys.forEach((k, i) => { const c = card(k); if (!c?.offsetHeight) return; const d = Math.abs(rel(c)); if (d < dist) { dist = d; best = i; } });
    return best;
  }
  function paint(i) {
    if (i === at) return;
    at = i;
    const k = keys[i];
    pb.set({ eyebrow: levels[levelOf.get(k)].title, main: `${i + 1} of ${keys.length}`, prev: i > 0, next: i < keys.length - 1, prevTitle: i > 0 ? title(keys[i - 1]) : "", nextTitle: i < keys.length - 1 ? title(keys[i + 1]) : "", said: `${title(k)}, ${i + 1} of ${keys.length}` });
    if (!on()) pb.fill(i);
    for (const [j, key] of keys.entries()) card(key)?.classList.toggle("flip-front", j === i);
    onChange?.(k);
  }
  // once the column is still, the card it came to rest on is the one on show; where it rests is the browser's snap
  // alone (a correction of ours, run when a flick pauses mid-glide, would pull against the momentum: an overshoot and back)
  let still = 0;
  function settle() { if (on()) paint(front()); }
  // how far down the column has turned, in cards: a fraction as a scroll moves it, so the line follows the finger
  function turned() {
    const a = card(keys[0]), b = card(keys[1]);
    if (!a || !b) return at < 0 ? 0 : at;
    const ra = rel(a), rb = rel(b);
    return rb === ra ? at : Math.max(0, Math.min(keys.length - 1, -ra / (rb - ra)));
  }
  let raf = 0;
  track.addEventListener("scroll", () => {
    clearTimeout(still); still = setTimeout(settle, 180);   // no scrollend in every Safari: quiet for a moment is still
    if (!raf) raf = requestAnimationFrame(() => { raf = 0; if (on()) { pb.fill(turned()); paint(front()); } });
  }, { passive: true });
  track.addEventListener("scrollend", () => { clearTimeout(still); settle(); });

  // the column turned to this card
  let unsnap = 0;
  function place(key, smooth) {
    const c = card(key);
    if (!c) return false;
    // a snap (a touch screen's) would stop a jump of several cards at the next one: it is off while the column moves
    const glide = smooth && !matchMedia("(prefers-reduced-motion: reduce)").matches;
    if (glide) {
      track.style.scrollSnapType = "none";
      clearTimeout(unsnap);
      const back = () => { clearTimeout(unsnap); track.style.scrollSnapType = ""; track.removeEventListener("scrollend", back); };
      track.addEventListener("scrollend", back);
      unsnap = setTimeout(back, 900);
    }
    // the browser's own reckoning of where the card is: under the pages' zoom Safari and Chrome disagree about the
    // units a sum of ours would be in (in Safari it would land cards a third of a screen off). The column is the only scroller
    // with room, so nothing else moves.
    c.scrollIntoView({ block: "start", inline: "nearest", behavior: glide ? "smooth" : "auto" });
    paint(keys.indexOf(key));
    if (!smooth) pb.fill(keys.indexOf(key));   // a smooth turn fills as it scrolls
    return true;
  }
  const go = (key, { smooth = true } = {}) => place(key, smooth);
  const step = (by) => { const i = Math.max(0, Math.min(keys.length - 1, (at < 0 ? front() : at) + by)); go(keys[i]); };
  // a tap on the next card's head showing at the foot turns to it, and presses nothing on it
  for (const type of ["pointerdown", "click"]) track.addEventListener(type, (e) => {
    if (!on()) return;
    const c = e.target.closest?.(".qs-card");
    if (!c || c.classList.contains("flip-front")) return;
    e.preventDefault(); e.stopPropagation();
    if (type === "click") go(keys.find((k) => card(k) === c));
  }, true);
  // A wheel or a trackpad: one card a gesture, eased there, rather than the snap chasing a flick's momentum (a flick
  // short of half a card springs back, a long one runs on and settles late). The code inside a card scrolls first while
  // it can; the rest of a gesture (a trackpad's momentum keeps sending for a second) is spent, not a second step.
  let acc = 0, quietAt = 0, locked = false, quiet = 0;
  track.addEventListener("wheel", (e) => {
    if (!on() || e.ctrlKey) return;   // ctrl: the browser's zoom
    const dy = e.deltaMode === 1 ? e.deltaY * 16 : e.deltaY;
    const body = e.target.closest?.(".qs-card-body");
    if (body && body.closest(".flip-front")) {
      const room = dy > 0 ? body.scrollHeight - body.clientHeight - body.scrollTop : body.scrollTop;
      if (room > 1) return;   // the code's own scroll has it
    }
    e.preventDefault();
    clearTimeout(quiet); quiet = setTimeout(() => { locked = false; acc = 0; }, 180);   // the gesture over: the next one steps
    if (locked) return;
    acc += dy;
    if (Math.abs(acc) < 30) return;
    locked = true;
    step(Math.sign(acc));
  }, { passive: false });
  track.tabIndex = -1;
  track.addEventListener("keydown", (e) => {
    if (!on() || e.target.closest("input, textarea, .cm-editor")) return;
    if (["ArrowUp", "ArrowLeft", "PageUp"].includes(e.key)) { e.preventDefault(); step(-1); }
    if (["ArrowDown", "ArrowRight", "PageDown"].includes(e.key)) { e.preventDefault(); step(1); }
  });
  // a width that starts or stops the flip: the front card put back at the top. A change of height is the snap's to
  // keep: re-placing on it, mid-change, would measure the wrong card
  let width = 0;
  new ResizeObserver(() => {
    if (track.clientWidth === width) return;
    width = track.clientWidth;
    if (on()) { const i = at < 0 ? 0 : at; at = -1; place(keys[i], false); }
  }).observe(track);

  return {
    el: bar,
    go,
    step,
    /** Whether the column flips now (a phone's width). */
    get on() { return on(); },
    get key() { return keys[at] ?? null; },
  };
}
