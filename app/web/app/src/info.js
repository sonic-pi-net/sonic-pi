// SPDX-License-Identifier: AGPL-3.0-or-later
// sonic-pi.net's pages, live. Each page is a whole HTML document (scripts/build-site.mjs): the app, with a page of
// the site in its Info card — the content, its list of sections and the bar's tabs all in the HTML already, so a page
// reads, links and scrolls to an anchor with no script at all. This adds what only a script can:
//
//   the list lit as the page scrolls, and the address following it      the examples and code blocks as live cards
//   a phone's list folded behind the bar's chip, stepped by its ‹ ›
//   the hero's words taking turns                                        videos that play in place
//
// and it goes from page to page without leaving the document: the next page's HTML is fetched, its content put in
// the card beside the pages already seen (hidden, kept as they were, a card playing on one still playing), and the
// address becomes the page's own. The app, its engine and whatever plays carry on; Back and Forward go between
// pages as they would between documents. Every other link is the card's: an anchor scrolls, #docs/… opens the docs
// pane, code.html is the editor, and anything off-site opens in a new tab so the music carries on.
import { mountExamples, mountSnippets } from "./site.js";
import { createSubmenu } from "./ui/submenu.js";
import { createFlip } from "./ui/flip.js";

const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };

/**
 * @param card  #info-card, the page in it (#info-body > .site-body > .ic-page)
 * @param hooks { tabs: the bar's .ic-tabs, play, stop, scopeFrame, open(code, job) (site.js's card hooks),
 *                pick(tab, id) — a page chosen, docs(section, key) — the docs pane there, leave() — the editor,
 *                shown(tab, id) — the section now being read, onTheme(fn) — fn when the app's theme changes,
 *                contents(state) — the page's list, for the bar's chip }
 */
export function createInfo(card, hooks) {
  const host = card.querySelector("#info-body"), tabsEl = hooks.tabs;
  const map = JSON.parse(document.getElementById("site-map").textContent);
  const fileOf = (key) => map.pages.find((p) => p.key === key)?.file ?? null;
  const keyOf = (file) => map.pages.find((p) => p.file === file)?.key ?? null;
  const tabOf = (key) => map.pages.find((p) => p.key === key)?.tab ?? key;   // a page under another's tab: the tutorial's chapters, under its book
  const pages = new Map();   // key → the page made live (adopt)
  let current = null;

  // A finger on the tab row: the row scrolls, and iOS drops a tap there whose finger moved at all. A touch that
  // lifts within a few pixels of where it landed is a tap, taken on the lift; the click after it is ignored.
  let down = null, tapped = -Infinity;
  tabsEl.addEventListener("pointerdown", (e) => { if (e.pointerType !== "mouse") down = { x: e.clientX, y: e.clientY, tab: e.target.closest(".ic-tab") }; });
  tabsEl.addEventListener("pointercancel", () => { down = null; });
  tabsEl.addEventListener("pointerup", (e) => {
    const d = down; down = null;
    if (!d?.tab?.dataset.tab || Math.hypot(e.clientX - d.x, e.clientY - d.y) > 12) return;   // the site's tabs only: Code is the app's (main.js)
    tapped = performance.now();
    pickTab(d.tab.dataset.tab);
  });
  tabsEl.addEventListener("click", (e) => {
    const tab = e.target.closest(".ic-tab[data-tab]");
    if (!tab || e.metaKey || e.ctrlKey || e.shiftKey) return;   // a new tab or window: the page's own document
    e.preventDefault();
    if (performance.now() - tapped < 600) return;   // a tap already took it (above)
    pickTab(tab.dataset.tab);
  });
  // Code is the editor (the engine starting in the tap); the rest are the site's pages
  const pickTab = (key) => (key === "code" ? hooks.leave() : hooks.pick(key));
  // the bar's tabs, and the editor's code icon beside the palette (index.html): the page's own lit
  const codeIcon = document.querySelector("#site-nav .sn-code");
  codeIcon?.addEventListener("click", (e) => { if (e.metaKey || e.ctrlKey || e.shiftKey) return; e.preventDefault(); hooks.leave(); });   // the editor, the engine starting in the tap
  const lightTab = (key) => { const tab = tabOf(key); codeIcon?.classList.toggle("active", tab === "code"); if (tab === "code") codeIcon?.setAttribute("aria-current", "page"); else codeIcon?.removeAttribute("aria-current"); for (const b of tabsEl.querySelectorAll("[data-tab]")) { const on = b.dataset.tab === tab; b.classList.toggle("active", on); if (on) { b.setAttribute("aria-current", "page"); b.scrollIntoView({ inline: "nearest", block: "nearest" }); } else b.removeAttribute("aria-current"); } };
  const skip = document.querySelector(".skip-link");

  // ── another page: its document fetched, its content taken into the card ──
  const fetched = new Map();   // key → the promise of its .site-body
  function fetchPage(key) {
    if (!fetched.has(key)) fetched.set(key, (async () => {
      const r = await fetch(fileOf(key));
      if (!r.ok) throw new Error(`${fileOf(key)}: ${r.status}`);
      const doc = new DOMParser().parseFromString(await r.text(), "text/html");
      // what the page's head asks for that this one's has not yet: its stylesheet, its scripts
      const loads = [];
      for (const l of doc.head.querySelectorAll('link[rel="stylesheet"]')) {
        const href = l.getAttribute("href");
        if (document.head.querySelector(`link[rel="stylesheet"][href="${href}"]`)) continue;
        const link = Object.assign(document.createElement("link"), { rel: "stylesheet", href });
        document.head.querySelector('link[href="app.css"]').before(link);   // ahead of the app's, as the page itself has it
        loads.push(new Promise((ok) => { link.onload = link.onerror = ok; }));
      }
      for (const s of doc.head.querySelectorAll("script[src]")) {
        const src = s.getAttribute("src");
        if (document.head.querySelector(`script[src="${src}"]`)) continue;
        const script = Object.assign(document.createElement("script"), { src });
        document.head.appendChild(script);
        loads.push(new Promise((ok) => { script.onload = script.onerror = ok; }));
      }
      await Promise.all(loads);
      return { body: document.adoptNode(doc.querySelector("#info-body > .site-body")), title: doc.title };
    })());
    return fetched.get(key);
  }

  /** A page by key, at an anchor on it: made live if it is new, shown, and the address its own. */
  async function go(key, id = null, { history: how = "push" } = {}) {
    if (current) current.settled = false;   // on its way: what moves under the page now is not a section being read
    let p = pages.get(key);
    if (!p) {
      const { body, title } = await fetchPage(key);
      body.hidden = true;
      host.appendChild(body);
      p = adopt(body, key, title);
    }
    const moved = current && current !== p;
    if (current !== p) {
      if (current) { current.fold?.closeFold?.(); current.body.hidden = true; current.stage?.stop(); }   // its list shut as it goes: the next page opens on itself
      p.body.hidden = false;
      current = p;
      document.title = p.title;
      if (skip) skip.href = `#${p.main.id}`;
    }
    // a page gone to is where the focus goes, so a screen reader says where it has arrived and reading starts there
    if (moved && !id) p.main.focus({ preventScroll: true });
    lightTab(key);
    const url = fileOf(key) + (id ? `#${id}` : "");
    if (how === "push" && !sameAddress(url)) history.pushState(null, "", url);
    else if (how === "replace") history.replaceState(null, "", url);
    await show(id);
  }
  const sameAddress = (url) => { const u = new URL(url, location.href); return u.pathname === location.pathname && u.hash === location.hash; };

  // ── a page made live ──
  function adopt(body, key, title = document.title) {
    const page = body.querySelector(".ic-page");
    const main = page.querySelector(".ic-main");
    const p = { key, body, page, title, main: page.querySelector("main, [role=main]"), mounts: [], looks: [], fold: null, stage: null, bare: null, settled: false, ready: null };
    let menu = null;

    // the page's list: lit as the page scrolls; picked, the page goes there
    const bar = page.querySelector(":scope > .ic-fold-bar");
    if (bar) {
      // picked: a section of this page scrolled to; one of another page's (the tutorial's other chapters) that page, there
      menu = createSubmenu({ adopt: bar, onPick: (k) => {
        const to = document.getElementById(k);
        if (to && page.contains(to)) return toSection(to);
        const [file] = (bar.querySelector(`[data-key="${CSS.escape(k)}"]`)?.getAttribute("href") ?? "").split("#");
        const other = file && keyOf(file);
        if (other) hooks.pick(other, k);
      } });
      p.fold = bar;
    }
    const logoIcon = bar?.querySelector(".logo-icon-lit");
    if (logoIcon) {   // Home's first entry: the app's icon in the theme's colours, its tile the accent; lit, its tile the page's
      const plain = logoIcon.previousElementSibling;
      const original = new Image();   // the icon's own pixels, read afresh for each theme: the page's copies are repainted
      original.src = plain.getAttribute("src");
      const paint = () => {
        if (!original.naturalWidth) return;
        recolourIcon(original, plain, "--HighlightedBackground", "--accentContrastText");
        recolourIcon(original, logoIcon, "--PaneBackground", "--HighlightedBackground");
      };
      original.decode().then(paint, () => {});
      hooks.onTheme?.(paint);
    }
    // the section being read, to the address once the page has reached where it was opened (before then a section
    // passed on the way would replace the anchor asked for)
    const read = (id) => { if (p.settled && current === p) hooks.shown?.(key, id); };
    if (menu && key !== "examples") {
      const sections = menu.keys().map((k) => document.getElementById(k)).filter(Boolean);
      if (sections.length) p.looks.push(spy(main, sections, (on) => { menu.active(on.id); read(on === sections[0] ? null : on.id); }));
    }

    // its images arrive after it: an anchor scrolled to before they do lands short, so it is scrolled to again once they have
    p.ready = Promise.race([Promise.all([...page.querySelectorAll("img")].map((i) => i.decode().catch(() => {}))), new Promise((r) => setTimeout(r, 2000))]);
    for (const a of page.querySelectorAll('a[href^="http"]')) { a.target = "_blank"; a.rel = "noopener"; }   // off-site: a new tab, the music carrying on
    for (const a of page.querySelectorAll(".yt-lite")) facade(a);

    // a page's own playable blocks (Learn's tutorial teaser): cards
    const teaser = [...page.querySelectorAll(".tut-teaser pre.sp-card, .tut-page pre.sp-card")];   // and the tutorial's, every code block a card
    if (teaser.length) p.mounts.push(mountSnippets(teaser, hooks, host, document));

    // the hero's words: they take turns while the hero is on show; a word pressed takes its turn now and the turns go
    // on from it. They hold still while the keyboard's focus is on them (only a visible focus: a click leaves one
    // behind in some browsers, and the pointer does not stop them)
    const facets = page.querySelector(".facets");
    if (facets) {
      const words = [...facets.querySelectorAll(".facet-word")], lines = [...facets.querySelectorAll(".facet-line")];
      const TURN = 5000, LEAVE = 700, still = window.matchMedia("(prefers-reduced-motion: reduce)").matches;   // a turn; how long a line takes to leave (site.css .facet-line)
      let i = 0, leaving = 0, next = 0, running = false, paused = false;
      facets.style.setProperty("--facet-turn", `${TURN}ms`);
      // a word's line arrives as the word lights, stays its turn, and leaves in the turn's last moments, gone as the
      // bar fills: the next word lights with nothing left to wait for
      const turn = (k) => {
        i = k;
        words.forEach((w, j) => { const on = j === k; w.classList.toggle("on", on); w.setAttribute("aria-pressed", String(on)); if (on) { const b = w.querySelector(".facet-bar"); b.replaceWith(b.cloneNode(true)); } });   // a fresh bar: the fill starts again
        lines.forEach((l, j) => l.classList.toggle("on", j === k));
        clearTimeout(leaving); clearTimeout(next);
        if (!running || still || paused) return;
        leaving = setTimeout(() => lines[k]?.classList.remove("on"), TURN - LEAVE);
        next = setTimeout(() => turn((i + 1) % words.length), TURN);
      };
      words.forEach((w, k) => w.addEventListener("click", () => turn(k)));
      const pause = (on) => { if (paused === on) return; paused = on; facets.classList.toggle("paused", on); if (!on) turn(i); else { clearTimeout(leaving); clearTimeout(next); lines[i]?.classList.add("on"); } };
      facets.addEventListener("focusin", (e) => { if (e.target.matches(":focus-visible")) pause(true); });
      facets.addEventListener("focusout", (e) => { if (!facets.contains(e.relatedTarget)) pause(false); });
      p.stage = { start() { running = true; turn(i); }, stop() { running = false; clearTimeout(leaving); clearTimeout(next); } };
    }

    // the examples: the hero's one, and the Examples page's, by level
    // a live synth (the home page's, under Code. Music. Live.): the docs pane's instrument, in place of its static words
    for (const s of page.querySelectorAll(".home-synth[data-synth]")) { const m = hooks.instrument?.(s, s.dataset.synth, host); if (m) p.mounts.push(m); }
    const heroPre = page.querySelector("#sp-hero-card pre.sp-card");
    if (heroPre) p.mounts.push(mountExamples([heroPre], hooks, host, document));
    const grid = page.querySelector("#sp-live-cards");
    if (grid) {
      const levels = [...grid.querySelectorAll(":scope > h3.qs-level")].map((h) => ({ title: h.textContent, keys: [...h.nextElementSibling.querySelectorAll("pre.sp-card")].map((e) => e.dataset.key) }));
      const examples = mountExamples([...grid.querySelectorAll("pre.sp-card")], hooks, host, document);
      p.mounts.push(examples);
      // a phone's width: one card at a time, swiped or stepped through (ui/flip.js), the list lit with the card at the front
      const flip = createFlip(grid, {
        levels, card: (k) => examples.reveal(k), title: (k) => examples.find(k)?.title ?? k,
        onChange: (k) => { if (flip.on) { menu?.active(`example-${k}`, { scroll: false }); read(k === levels[0]?.keys[0] ? null : `example-${k}`); } },   // the list lit (tellFold hears it), the address at the card: the first, the page's own (as a page's first section is)
        onJump: () => menu?.toggleFold(),   // the bar's place, tapped: the whole list, to go anywhere
      });
      p.bare = () => flip.on;   // flipping, the card and the bar name where you are: the strip's chip would only say it again
      grid.before(flip.el);   // over the cards, under the tabs: a phone's foot is its browser's own ‹ › and the cards' Play
      let look;   // the list's items pin their card in it
      for (const k of menu?.keys() ?? []) {
        const ex = k.replace(/^example-/, "");
        menu.onPick(k, () => { if (flip.on) return flip.go(ex); const c = examples.reveal(ex); if (c) { look.pin = c; c.scrollIntoView({ block: "start", behavior: "smooth" }); look(); } });
      }
      look = spy(main, () => (flip.on ? [] : [...main.querySelectorAll(".qs-card[data-key]")].filter((c) => c.offsetParent)), (on) => { menu?.active(on?.id ?? null, { scroll: false }); read(on?.id ?? null); });   // flipping, the flip says which card is on show
      p.looks.push(look);
    }

    // the page's links: an anchor on it scrolls; another page of the site is gone to here; code.html, #docs/… are the app's
    page.addEventListener("click", (e) => {
      const a = e.target.closest?.("a[href]");
      if (!a || e.defaultPrevented || e.metaKey || e.ctrlKey || e.shiftKey || a.target === "_blank") return;
      const href = a.getAttribute("href");
      if (href === "#app" || href === map.code.file) { e.preventDefault(); return hooks.leave(); }
      const d = /^(?:code\.html)?#docs\/(\w+)(?:\/([\w.-]+))?$/.exec(href);
      if (d) { e.preventDefault(); return hooks.docs(d[1], d[2] ?? null); }
      const [file, id = null] = href.split("#");
      const to = file ? keyOf(file) : key;
      if (!to) return;   // not a page of the site: an ordinary link
      e.preventDefault();
      if (to !== key) return hooks.pick(to, id || null);
      const target = id && document.getElementById(id);
      if (target) toSection(target);
    });

    pages.set(key, p);
    return p;
  }

  /** The current page, at its top or at an anchor on it. The address follows the page's reading once it is there. */
  async function show(id = null) {
    const p = current;
    p.stage?.start();
    p.settled = false;
    const target = id ? document.getElementById(id) : null;
    tellFold(p);
    if (target) {
      await toSection(target, { instant: true });
      await p.ready;   // its images in: laid out as it will stay
      if (current === p && !card.hidden) await toSection(target, { instant: true });
    } else {
      // at once: the column scrolls smoothly (site.css), and WebKit drops a smooth scroll on a column just unhidden,
      // leaving it where it was, which Safari then does not paint (a blank page)
      host.scrollTo({ top: 0, behavior: "instant" });
      p.page.querySelector(".ic-main")?.scrollTo({ top: 0, behavior: "instant" });
    }
    await new Promise(requestAnimationFrame);
    if (current !== p) return;
    p.settled = true;
    p.looks.forEach((look) => look.again());
  }

  // the page's fold, to the host's chip: the section the page is at (its list's lit entry, as the page scrolls), or
  // the fold's own name until one is lit; open or shut; and every change after
  function tellFold(p) {
    const bar = p.fold;
    if (!bar) return hooks.contents?.(null);
    const side = bar.querySelector(".ic-side");
    const report = () => {
      if (current !== p) return;
      const { items, groups } = contentsOf(side);
      const at = items.findIndex((it) => it.node.matches(".active"));
      const it = items[at];
      hooks.contents?.({
        // the group over where you are, the same on every page (a page's heading, an example's level); a group's own
        // first entry is its Introduction, not its name twice
        group: it ? groups[it.group]?.title ?? "" : "", label: it ? (it.title === groups[it.group]?.title ? "Introduction" : it.title) : bar.foldLabel,
        at: Math.max(0, at), count: items.length,
        groups: groups.map((g) => ({ title: g.title, n: g.n })), prevTitle: items[at - 1]?.title ?? "", nextTitle: at >= 0 ? items[at + 1]?.title ?? "" : items[0]?.title ?? "",
        open: bar.classList.contains("open"), bare: !!p.bare?.(),
        sideRight: side?.offsetParent ? side.getBoundingClientRect().right : 0,   // a wide screen's list beside the page: the bar starts at its edge
      });
    };
    report();
    if (!bar.told) {
      bar.told = true;
      bar.addEventListener("fold", report);
      if (side) new MutationObserver(report).observe(side, { attributes: true, subtree: true, attributeFilter: ["class"] });   // the lit entry moves as the page scrolls
      if (side) new ResizeObserver(report).observe(side);   // a wide screen's list settling or zoomed: the bar keeps to its edge
    }
  }

  // A page's list as the page bar walks it: its entries in order, each in a group (a heading, then its sections)
  function contentsOf(side) {
    const items = [], groups = [];
    if (!side) return { items, groups };
    const name = (n) => (n.querySelector(":scope > span:last-of-type") ?? n).textContent.trim().replace(/\s+/g, " ");
    for (const n of side.querySelectorAll(".docs-group, .docs-item")) {
      if (n.classList.contains("docs-group")) { groups.push({ title: n.textContent.trim(), n: 0 }); continue; }
      if (!groups.length) groups.push({ title: "", n: 0 });
      groups.at(-1).n++;
      items.push({ node: n, title: name(n), group: groups.length - 1 });
    }
    return { items, groups };
  }
  // the bar's ‹ ›: the entry either side of the lit one, picked as a tap on it would pick it
  // the entry a step went to, while the page scrolls there (a long way can take a while): a second tap goes on from it,
  // until the page's lit entry has caught up
  let heading = null;
  const side = () => current?.fold?.querySelector(".ic-side");
  function stepContents(by) {
    const { items } = contentsOf(side());
    let at = items.findIndex((it) => it.node.matches(".active"));
    if (heading && heading.page === current && heading.i !== at && performance.now() - heading.t < 4000) at = heading.i;
    contentsItem(at + by, items);
  }
  function contentsItem(k, items = contentsOf(side()).items) {
    const i = Math.max(0, Math.min(items.length - 1, k));
    heading = { page: current, i, t: performance.now() };
    items[i]?.node.click();
  }
  function contentsGroup(g) {
    const { items } = contentsOf(side());
    contentsItem(items.findIndex((it) => it.group === g), items);
  }

  // To a section: the first is the page's top, so what scrolls goes right up and the room above it stays; any other
  // comes to the top. What scrolls is the page's column on a wide screen, and on a phone, where the column just
  // flows, the card's body (host): scrolling the column there moved nothing.
  // Resolves once the section is where it stays. instant: straight there, as a page opened at an anchor goes.
  function toSection(to, { instant = false } = {}) {
    let scroller = host;
    for (let e = to.parentElement; e; e = e.parentElement) { if (e.scrollHeight > e.clientHeight + 1 && /auto|scroll/.test(getComputedStyle(e).overflowY)) { scroller = e; break; } }
    const top = to.getBoundingClientRect().top - scroller.getBoundingClientRect().top + scroller.scrollTop;
    if (top < 160) { scroller.scrollTo({ top: 0, behavior: instant ? "instant" : "smooth" }); return Promise.resolve(); }
    to.scrollIntoView({ behavior: instant ? "instant" : "smooth", block: "start" });
    // the page can move under the scroll: cards on the way are laid out at a guessed height until they near the
    // view (card.css content-visibility), and take their own as they pass, so the section lands off its mark.
    // Once the scroll is still, the section is put where it belongs, again until it stays there.
    const aim = ++aiming;
    return new Promise((done) => {
      let tries = 0, quiet = 0;
      const check = () => {
        if (aim !== aiming) return done();
        const pad = parseFloat(getComputedStyle(scroller).scrollPaddingTop) || 0;
        const off = to.getBoundingClientRect().top - scroller.getBoundingClientRect().top - pad;
        const bottom = scroller.scrollTop >= scroller.scrollHeight - scroller.clientHeight - 1;
        if (Math.abs(off) <= 3 || (off > 0 && bottom) || ++tries > 4) return done();
        to.scrollIntoView({ behavior: "instant", block: "start" });
        requestAnimationFrame(() => requestAnimationFrame(check));   // and again, once what just came into view has its height
      };
      const settle = () => { clearTimeout(quiet); quiet = setTimeout(() => { scroller.removeEventListener("scroll", settle); check(); }, instant ? 30 : 160); };
      scroller.addEventListener("scroll", settle, { passive: true });
      settle();
    });
  }
  let aiming = 0;   // the latest section gone to: an earlier one's correction stands down

  // the app's icon in two of the theme's colours, its tile's and its glyph's: the icon's own pixels, each read as a mix
  // of tile and glyph by its whiteness, repainted in them
  function recolourIcon(src, into, tileVar, inkVar) {
    const rgb = (v) => { const m = v.match(/\d+/g); return m ? m.slice(0, 3).map(Number) : [0, 0, 0]; };
    const probe = el("span"); document.body.appendChild(probe);
    probe.style.color = `var(${tileVar})`; const tile = rgb(getComputedStyle(probe).color);
    probe.style.color = `var(${inkVar})`; const ink = rgb(getComputedStyle(probe).color); probe.remove();
    const c = document.createElement("canvas"); c.width = src.naturalWidth; c.height = src.naturalHeight;
    const ctx = c.getContext("2d"); ctx.drawImage(src, 0, 0);
    const img = ctx.getImageData(0, 0, c.width, c.height), d = img.data;
    for (let i = 0; i < d.length; i += 4) { const w = Math.max(0, Math.min(d[i], d[i + 1], d[i + 2]) - 60) / (255 - 60); for (let k = 0; k < 3; k++) d[i + k] = tile[k] * (1 - w) + ink[k] * w; }   // the least channel is the whiteness: the pink tile has little green (the little it has is tile), the white glyph all
    ctx.putImageData(img, 0, 0);
    into.src = c.toDataURL();
  }

  // the section being read: the last whose top has passed a line a quarter of the way down the scrolling column
  function spy(scroller, targets, markOn) {
    let raf = 0, last;
    // the column scrolls on a wide screen; on a phone it flows and the card's body scrolls the page: the line is the one that scrolls
    const box = () => (scroller.scrollHeight > scroller.clientHeight + 1 ? scroller : host);
    const look = () => {
      raf = 0;
      if (card.hidden || !scroller.offsetParent) return;   // a page not on show has nothing in view
      const list = typeof targets === "function" ? targets() : targets;
      const sc = box();
      const top = sc.getBoundingClientRect().top + (parseFloat(getComputedStyle(sc).paddingTop) || 0), line = Math.min(sc.clientHeight * 0.25, 96);   // from below whatever floats over its top (a phone's contents strip pads the body); near the top a short card scrolled to is the one lit, not the next
      if (!list.length) return;
      let on = list[0], i = 0;
      for (let k = 0; k < list.length; k++) { if (list[k].getBoundingClientRect().top - top <= line) { on = list[k]; i = k; } else break; }
      // scrolled to the end: the last section is the one being read, though it is too short to reach the line (a page
      // zoomed up: Learn's Community, under its Talks)
      if (sc.scrollTop > 0 && sc.scrollTop + sc.clientHeight >= sc.scrollHeight - 2) {
        const end = list[list.length - 1];
        if (end.getBoundingClientRect().top < top + sc.clientHeight) { on = end; i = list.length - 1; }
      }
      // cards stand in rows: of a row's cards the first is lit, or the one picked from the list if it is in this row
      const rowTop = on.getBoundingClientRect().top;
      const sameRow = (t) => Math.abs(t.getBoundingClientRect().top - rowTop) < 2;
      while (i > 0 && sameRow(list[i - 1])) i--;
      on = look.pin && sameRow(look.pin) ? look.pin : list[i];
      if (on === last) return;
      last = on;
      markOn(on);
    };
    const onScroll = () => { if (!raf) raf = requestAnimationFrame(look); };
    scroller.addEventListener("scroll", onScroll, { passive: true });
    host.addEventListener("scroll", onScroll, { passive: true });
    look.again = () => { last = undefined; look(); };
    return look;
  }

  // a video's facade plays in place where a credentialless frame is allowed, else on YouTube
  function facade(a) {
    a.addEventListener("click", (e) => {
      if (!("credentialless" in HTMLIFrameElement.prototype)) return;
      e.preventDefault();
      const f = document.createElement("iframe");
      f.credentialless = true;
      f.src = `https://www.youtube.com/embed/${a.dataset.yt}?autoplay=1`;
      f.title = a.title;
      f.allow = "autoplay; encrypted-media; picture-in-picture";
      f.allowFullscreen = true;
      a.replaceWith(f);
    });
  }

  // the page this document was loaded as; the scroll is the page's own to set (an anchor, or its top), not the browser's
  // memory of where a reload left it, which lands before the cards are in and jumps once they are
  history.scrollRestoration = "manual";
  current = adopt(card.querySelector("#info-body > .site-body"), map.page);

  const all = () => [...pages.values()].flatMap((p) => p.mounts);
  return {
    /** The page on show (about, examples, …); the page holding an id; a page's file; a file's page. */
    get page() { return current.key; },
    pageOf: (id) => (current.page.querySelector(`#${CSS.escape(id)}`) ? current.key : map.pages.find((p) => p.ids.includes(id))?.key ?? null),
    fileOf, keyOf,
    /** The editor's own document (code.html) and its title. */
    code: map.code,
    tabs: map.pages.map((p) => p.key),
    go,
    /** The page's contents, open or shut (a phone's chip in the bar). */
    toggleContents() { current.fold?.toggleFold(); },
    /** A wide screen's list, always open: where you are in it brought into view. */
    revealContents() { const a = side()?.querySelector(".active"); a?.scrollIntoView({ block: "center", behavior: "smooth" }); a?.focus({ preventScroll: true }); },
    /** The page's next or previous entry (by 1 or -1), or a group's first: the page bar's ‹ › and line. */
    stepContents, contentsGroup, contentsItem: (k) => contentsItem(k),
    /** The card put away (the editor showing): the stage rests, the address is the editor's, and Code is the tab lit */
    blur() { current.stage?.stop(); current.settled = false; lightTab("code"); },
    /** The page is showing again. */
    focus() { current.stage?.start(); lightTab(current.key); },
    get current() { return current.key; },
    // every page's live cards: a card playing on a page left behind still hears the session
    groups: (live) => all().forEach((m) => m.groups(live)),
    error: (r) => all().forEach((m) => m.error(r)),
    release: (job) => all().forEach((m) => m.release(job)),
    flash: (job, line) => all().forEach((m) => m.flash(job, line)),
    stop: () => all().forEach((m) => m.stop()),
    get playing() { return all().map((m) => m.playing).find((j) => j != null) ?? null; },
    owns: (job) => all().some((m) => m.owns(job)),
    record: (r) => all().forEach((m) => m.record(r)),
    get starting() { return all().some((m) => m.starting); },
  };
}
