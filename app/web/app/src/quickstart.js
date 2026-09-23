// SPDX-License-Identifier: AGPL-3.0-or-later
// Quickstart cards, as native Sonic Pi v5 has them (etc/quickstart/cards.txt):
// decks of short programs, a carousel a page at a time. Each card is native's:
// a title bar with insert, copy and a handle to drag the code into the editor
// (native's tabler glyphs), then the code, then its blurb beside the card's
// scope — which is the play/stop control, as in native's quickstart pane and
// on sonic-pi.net: the transport disc with the stereo rings drawn around it.
import { icon } from "./icons.js";
import { announce, Announcement } from "./announce.js";
import { createCard } from "./ui/card.js";
import { createDeck } from "./ui/deck.js";

const DECK_KEY = "sp-quickstart-deck";
const SHOWN_KEY = "sp-quickstart-shown";   // how many of each deck's cards have been dealt, by deck index
const REACHED_KEY = "sp-quickstart-reached";   // the furthest deck reached: the ones after it are not shown yet
const CARD_MIN = 400, GAP = 18;
const store = {
  get: (k) => { try { return localStorage.getItem(k); } catch { return null; } },
  set: (k, v) => { try { localStorage.setItem(k, v); } catch {} },
};
const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };

/**
 * @param root element to fill
 * @param data quickstart.json
 * @param hooks { play(code) → Promise<job>, stop(job), insert(code), copy(code), scope() → main Scope }
 */
export function createQuickstart(root, data, hooks) {
  let deck = Math.min(data.decks.length - 1, Math.max(0, parseInt(store.get(DECK_KEY) ?? "0", 10) || 0));
  let page = 0;
  // A deck opens on its intro card (its words, and Start), then is dealt a card at a time: Start deals the first, and
  // the tile after the last dealt card (or the pager's next, or the → key, at the end) deals the next. How far each
  // deck has got is remembered.
  const shown = (() => { try { return JSON.parse(store.get(SHOWN_KEY) ?? "{}") ?? {}; } catch { return {}; } })();
  const dealt = (i = deck) => Math.min(data.decks[i].cards.length, Math.max(0, shown[i] ?? 0));
  // The decks are reached in order: the end of one deals the next (nextDeckTile), and until then its chip is not
  // there. A deck already under way (dealt into, or the one open) counts as reached, so no one loses a deck they had.
  let reached = Math.min(data.decks.length - 1, Math.max(deck, parseInt(store.get(REACHED_KEY) ?? "0", 10) || 0,
    ...Object.keys(shown).filter((i) => shown[i] > 0).map(Number)));
  let fresh = -1;   // the card just dealt: it arrives with a flourish
  // In a deck (dealing it, or turning its pages) the deck chips give way to its carousel: the deck's name, its pages,
  // and a way back out to the chips. Out of one, the chips, and the pager under the cards.
  let inDeck = false;
  // Where focus goes when the cards are re-dealt (render replaces them, and a focused one with them): the card just
  // dealt, a card by its index, the deck's Start, or the lit dot. Without it focus falls to the page, and a keyboard or screen reader
  // user is thrown back to the start of it.
  let focusAfter = null;
  const cards = createDeck(hooks, root);   // the cards on screen, one playing at a time (ui/deck.js)

  root.textContent = "";
  root.classList.add("qs");
  const head = el("div", "qs-head");
  const tabs = el("div", "seg qs-decks");
  // the carousel, in a deck: out (to the chips and the deck's intro), the deck's name, and its pages (the pill)
  const bar = el("div", "qs-bar");
  const out = el("button", "qs-exit");
  out.type = "button";
  out.innerHTML = icon("x", "");
  out.append(el("span", "", "Decks"));
  out.title = "Back to the decks";
  const barTitle = el("span", "qs-bar-title");
  const barStart = el("span", "qs-bar-start");   // out and the name at the left; the pill in the middle of the bar
  barStart.append(out, barTitle);
  bar.append(barStart);
  head.append(tabs, bar);   // the deck's words are its intro card's (introCard), on every screen
  const carousel = el("div", "qs-carousel");
  const track = el("div", "qs-track");
  carousel.append(track);
  const pager = el("div", "qs-pager");
  const pill = el("div", "qs-pager-pill");
  const prev = el("button", "qs-nav"); prev.innerHTML = icon("chevron-left");
  prev.title = "Previous cards";
  const dots = el("div", "qs-dots");
  const next = el("button", "qs-nav"); next.innerHTML = icon("chevron-right");
  next.title = "Next cards";
  pill.append(prev, dots, next);
  pager.append(pill);
  root.append(head, carousel, pager);

  data.decks.forEach((d, i) => {
    const b = el("button", "", d.title);
    b.type = "button";
    b.addEventListener("click", () => { deck = i; page = 0; store.set(DECK_KEY, String(i)); render(); announce(`${d.title} deck, ${d.cards.length} cards`, false, Announcement.Navigation); });
    tabs.appendChild(b);
  });
  // native's "Card N of M" as the cards move, "First card." and "Last card." at the ends
  const sayPage = (moved) => {
    const d = data.decks[deck];
    if (!moved) announce(page === 0 ? "First card." : "Last card.", false, Announcement.Navigation);
    else announce(first() === 0 ? `${d.title}: the deck's introduction` : `Card ${first()} of ${d.cards.length}`, false, Announcement.Navigation);
  };
  const turn = (by) => { const was = page; page += by; if (page !== was) inDeck = true; render(); sayPage(page !== was); };
  out.addEventListener("click", () => {
    inDeck = false;
    page = 0;
    focusAfter = "start";
    render();
    announce(`Decks. ${data.decks[deck].title}: the deck's introduction`, false, Announcement.Navigation);
  });
  function deal() {
    const d = data.decks[deck], n = dealt();
    if (n >= d.cards.length) return;
    shown[deck] = n + 1;
    store.set(SHOWN_KEY, JSON.stringify(shown));
    fresh = n;
    focusAfter = "fresh";
    inDeck = true;
    page = pageCount() - 1;   // the last page: the new card, the tile after it
    render();
    announce(`Card ${n + 1} of ${d.cards.length}: ${d.cards[n].title}`, false, Announcement.Navigation);
  }
  const hasNextDeck = () => dealt() >= data.decks[deck].cards.length && deck < data.decks.length - 1;
  function nextDeck() {
    deck += 1;
    reached = Math.max(reached, deck);
    store.set(REACHED_KEY, String(reached));
    store.set(DECK_KEY, String(deck));
    page = 0;
    inDeck = false;
    focusAfter = "start";
    render();
    const d = data.decks[deck];
    announce(`New deck: ${d.title}, ${d.cards.length} cards`, false, Announcement.Navigation);
  }
  const forward = () => (page < pageCount() - 1 ? turn(1) : hasNextDeck() ? nextDeck() : deal());
  prev.addEventListener("click", () => turn(-1));
  next.addEventListener("click", forward);
  // a swipe across the cards turns them as ‹ and › do: not one begun in code being edited, or in code that scrolls
  // sideways itself, and a mostly-vertical drag is left to scroll
  let swipe = null;
  carousel.addEventListener("touchstart", (e) => {
    const t = e.touches[0], at = e.target;
    const scrolls = (n) => n && n !== carousel && (n.scrollWidth > n.clientWidth + 1 && /auto|scroll/.test(getComputedStyle(n).overflowX) || scrolls(n.parentElement));
    swipe = e.touches.length === 1 && !at.closest(".cm-editor.cm-focused, input, textarea, .dial") && !scrolls(at) ? { x: t.clientX, y: t.clientY, t: e.timeStamp } : null;
  }, { passive: true });
  carousel.addEventListener("touchend", (e) => {
    if (!swipe) return;
    const t = e.changedTouches[0], dx = t.clientX - swipe.x, dy = t.clientY - swipe.y, quick = e.timeStamp - swipe.t < 800;
    swipe = null;
    if (!quick || Math.abs(dx) < 50 || Math.abs(dx) < 1.5 * Math.abs(dy)) return;
    if (dx < 0 && !next.disabled) forward();
    else if (dx > 0 && !prev.disabled) turn(-1);
  }, { passive: true });
  carousel.addEventListener("touchcancel", () => { swipe = null; }, { passive: true });
  root.tabIndex = -1;
  root.addEventListener("keydown", (e) => {
    if (e.target.closest("input, textarea, .cm-editor")) return;
    if (e.key === "ArrowLeft") turn(-1);
    if (e.key === "ArrowRight") forward();
  });

  // the intro card, the dealt cards, and the tile while any are left once the deck has started (before, the intro's
  // Start deals); one card to a page (a phone), the tile sits under the card on the same page, not on a page alone
  // Out of a deck (the decks' overview) there is only its intro card: its words and the way in; the cards are the
  // deck's, and show once in it
  const items = () => (!inDeck ? 1 : 1 + dealt() + (((dealt() > 0 && dealt() < data.decks[deck].cards.length) || hasNextDeck()) && perPage() > 1 ? 1 : 0));
  const pageCount = () => Math.max(1, Math.ceil(items() / perPage()));
  // where a page starts: a page's worth at a time, but the last page ends at the last item, the tile when there is
  // one, however many that leaves it sharing with the page before. So the tile always has a place beside the newest
  // card, and a card dealt into a full page slides the view along by one rather than leaving the tile on a page alone.
  const first = (p = page, per = perPage()) => (p === pageCount() - 1 ? Math.max(0, items() - per) : p * per);
  const perPage = () => Math.max(1, Math.floor((track.clientWidth + GAP) / (CARD_MIN + GAP)));

  function render() {
    const d = data.decks[deck];
    [...tabs.children].forEach((b, i) => { b.hidden = i > reached; b.classList.toggle("active", i === deck); b.setAttribute("aria-pressed", String(i === deck)); });
    const per = perPage(), n = dealt();
    laidOut = per;
    const had = root.contains(document.activeElement) ? document.activeElement : null;
    const pages = pageCount();
    page = Math.min(Math.max(0, page), pages - 1);
    track.style.setProperty("--per", per);
    // every card as tall as the deck's longest program needs, dealt or not: a card dealt does not resize the others
    track.style.setProperty("--deck-lines", String(Math.max(...d.cards.map((c) => c.code.replace(/\n+$/, "").split("\n").length))));
    track.classList.toggle("qs-single", per === 1);
    cards.detach();                        // the cards on screen are about to be replaced; a playing one is adopted back by its key
    track.textContent = "";
    const from = first(page, per);
    // item 0 is the intro card; item k the deck's card k - 1
    if (from === 0) track.appendChild(introCard(d, n));
    for (let i = Math.max(0, from - 1); inDeck && i < Math.min(n, from + per - 1); i++) {
      const c = d.cards[i];
      const card = cards.add(createCard({ title: c.title, code: c.code, blurb: c.blurb, key: `${deck}/${i}`, actions: ["edit", "reset", "copy", "insert", "drag"], hooks })).el;
      if (i === fresh) card.classList.add("qs-dealt");
      track.appendChild(card);
    }
    const dealtCard = fresh >= 0 ? track.querySelector(`[data-key="${deck}/${fresh}"]`) : null;
    fresh = -1;
    if (inDeck && n > 0 && n < d.cards.length && page === pages - 1) track.appendChild(nextTile(d, n));   // the tile ends the last page
    else if (inDeck && hasNextDeck() && page === pages - 1) track.appendChild(nextDeckTile(data.decks[deck + 1]));   // or, the deck done, the next deck
    prev.disabled = page === 0;
    next.disabled = page === pages - 1 && n >= d.cards.length && !hasNextDeck();
    // in a deck the pill is its carousel's, at the top; out of one, under the cards, when there are pages to turn
    head.classList.toggle("in-deck", inDeck);
    tabs.hidden = inDeck || reached === 0;   // one deck reached: nothing to choose between (its intro names it)
    bar.hidden = !inDeck;
    barTitle.textContent = d.title;
    if (pill.parentNode !== (inDeck ? bar : pager)) (inDeck ? bar : pager).appendChild(pill);   // moved only when it must: a move drops focus
    pager.hidden = inDeck || pages === 1;
    dots.textContent = "";
    // a dot a page while they fit at a glance; past that the dots would crowd out the deck's name: where you are, as
    // words (the intro, or the cards on show "of" the deck's)
    dots.classList.toggle("qs-count", pages > 6);
    if (pages > 6) {
      const a = first(page, per), b = Math.min(n, a + per - 1);
      dots.textContent = a === 0 ? "Intro" : `${a === b ? a : `${a}–${b}`} of ${d.cards.length}`;
      return;
    }
    for (let i = 0; i < pages; i++) {
      const dot = el("button", `qs-dot${i === page ? " active" : i < page ? " seen" : ""}`);
      dot.title = `Page ${i + 1}`;
      dot.setAttribute("aria-label", `Page ${i + 1} of ${pages}`);
      dot.setAttribute("aria-pressed", String(i === page));
      dot.addEventListener("click", () => { const was = page; page = i; if (page !== was) inDeck = true; focusAfter = "dot"; render(); sayPage(page !== was); });
      dots.appendChild(dot);
    }
    placeFocus(had, dealtCard);
  }

  // after a re-deal, focus where the player's attention now is (focusAfter), or, if what had it is gone, disabled or
  // hidden, the nearest thing that does the same; nothing moves it when it was not in the cards to begin with
  function placeFocus(had, dealtCard) {
    const want = focusAfter;
    focusAfter = null;
    if (!had) return;
    const usable = (e) => e && e.isConnected && !e.disabled && e.offsetParent !== null;
    const card = (e) => { if (!e) return null; e.tabIndex = -1; return e; };   // the card as a whole: its name is read, then Tab reaches its buttons
    const target =
      want === "fresh" ? card(dealtCard) :
      typeof want === "number" ? card(track.querySelector(`[data-key="${deck}/${want}"]`)) :
      want === "start" ? track.querySelector(".qs-start") :
      want === "dot" ? dots.querySelector(".qs-dot.active") :
      usable(had) ? null :
      had === prev ? (usable(next) ? next : dots.querySelector(".qs-dot.active")) :
      had === next ? (usable(prev) ? prev : dots.querySelector(".qs-dot.active")) : again(had) ?? root;
    if (usable(target) || target === root || target?.tabIndex === -1) target.focus();
  }
  // what had focus in a card, in that card as dealt again: the same card by its key, the same control by its class
  // (Play, Edit, …), or the card itself
  function again(had) {
    const old = had.closest?.(".qs-card[data-key]");
    if (!old) return null;
    const now = track.querySelector(`.qs-card[data-key="${old.dataset.key}"]`);
    if (!now) return null;
    const same = had === old ? null : had.className && now.querySelector(had.tagName.toLowerCase() + "." + [...had.classList].join("."));
    if (same && !same.disabled) return same;
    now.tabIndex = -1;
    return now;
  }

  // The deck's first card: its words, and the call to start (or carry on). Drawn as the next-card tile is (dashed,
  // in the accent): not a program, the way in to them. A card's body and foot (ui/card.css), so it is as tall as the
  // cards beside it; the deck's name is its label, as "Next card" is the tile's.
  function introCard(d, n) {
    const art = el("section", "qs-card qs-intro");
    art.setAttribute("aria-label", `${d.title}: the deck's introduction`);
    const body = el("div", "qs-card-body");
    body.append(el("h2", "qs-intro-label", d.title));
    const desc = el("p", "qs-desc");
    paintDesc(desc, d.description);
    body.append(desc);
    const foot = el("div", "qs-card-foot");
    // the call to it, a big play: Start deals the first card; part-way, Carry on goes to the newest one (the tile
    // beside it); all dealt, Play through again goes back to the first
    const len = d.cards.length;
    const start = el("button", "qs-start");
    start.type = "button";
    start.innerHTML = `<span class="qs-start-disc" aria-hidden="true"><svg viewBox="0 0 24 24"><path d="M8 5.5v13l10.5 -6.5z"/></svg></span>`;
    const words = el("span", "qs-start-words");
    words.append(el("span", "qs-start-say", n === 0 ? "Start deck" : n < len ? "Carry on" : "Play through again"),
      el("span", "qs-start-sub", n === 0 ? `${len} cards` : n < len ? `${n} of ${len} cards dealt` : `all ${len} cards dealt`));
    start.append(words);
    start.title = n === 0 ? `Deal the first card: ${d.cards[0]?.title ?? ""}` : n < len ? `To the newest card: ${d.cards[n - 1].title}` : `Back to the first card: ${d.cards[0].title}`;
    start.addEventListener("click", () => {
      if (n === 0) return deal();
      const was = page;
      inDeck = true;
      focusAfter = n < len ? n - 1 : 0;   // the card it goes to
      page = n < len ? pageCount() - 1 : Math.floor(1 / perPage());   // the newest card's page, or the first card's
      render();
      sayPage(page !== was);
    });
    foot.append(start);
    art.append(body, foot);
    return art;
  }

  // A deck's words, laid out to be read at a glance: the first sentence the lead, the rest under it, and where they
  // name a card's control (Press play, drag it, insert at point) that control's glyph beside the word
  const GLYPHS = [[/\b(Press play)\b/, "player-play"], [/\b(drag it)\b/, "drag"], [/\b(insert at point)\b/, "insert"]];
  function paintDesc(desc, text) {
    desc.textContent = "";
    // the lead is the first sentence: to a full stop, a ! or a ?, or a smiley (Basics: "Welcome friend :-)")
    const [, lead, rest = ""] = /^(.+?(?:[.!?]|[:;]-?[)D]))(?:\s+(.*))?$/s.exec(text) ?? [null, text];
    desc.append(el("span", "qs-desc-lead", lead));
    if (!rest) return;
    const body = el("span", "qs-desc-body");
    let parts = [rest];
    for (const [re, name] of GLYPHS) parts = parts.flatMap((p) => {
      if (typeof p !== "string") return [p];
      const m = re.exec(p);
      if (!m) return [p];
      const g = el("span", "qs-desc-glyph"); g.innerHTML = icon(name, "");
      return [p.slice(0, m.index + m[1].length), g, p.slice(m.index + m[1].length)];
    });
    body.append(...parts);
    desc.append(body);
  }

  // the tile after the last dealt card: what comes next, and a tap to deal it
  function nextTile(d, n) {
    const b = el("button", "qs-next-card");
    b.type = "button";
    b.innerHTML = `<span class="qs-next-disc">${icon("chevron-right", "")}</span>`;
    b.append(el("span", "qs-next-label", "Next card"), el("span", "qs-next-title", d.cards[n].title), el("span", "qs-next-count", `${n + 1} of ${d.cards.length}`));
    b.title = `Show the next card: ${d.cards[n].title}`;
    b.addEventListener("click", deal);
    return b;
  }

  // the tile after a deck's last card: the next deck, reached by it
  function nextDeckTile(nd) {
    const b = el("button", "qs-next-card");
    b.type = "button";
    b.innerHTML = `<span class="qs-next-disc">${icon("chevron-right", "")}</span>`;
    b.append(el("span", "qs-next-label", "Next deck"), el("span", "qs-next-title", nd.title), el("span", "qs-next-count", `${nd.cards.length} cards`));
    b.title = `On to the next deck: ${nd.title}`;
    b.addEventListener("click", nextDeck);
    return b;
  }

  // re-dealt only when the cards to a page change: not for a height (the on-screen keyboard coming up), which would
  // replace the card being typed into
  let laidOut = 0;
  new ResizeObserver(() => { if (root.offsetParent && perPage() !== laidOut) render(); }).observe(track);
  render();

  return {
    render,
    groups: (live) => cards.groups(live),
    error: (r) => cards.error(r),
    flash: (job, line) => cards.flash(job, line),
    release: (job) => cards.release(job),
    stop: () => cards.stop(),
    get playing() { return cards.playing; },
    owns: (job) => cards.owns(job),
    record: (r) => cards.record(r),
    get starting() { return cards.starting; },
  };
}
