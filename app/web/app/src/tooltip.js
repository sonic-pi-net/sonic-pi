// SPDX-License-Identifier: AGPL-3.0-or-later
// Tooltips as native Sonic Pi draws them (widgets/sonicpitooltip.cpp): one
// anchored, caret-pointing bubble for the whole page in place of the
// browser's. A neutral bubble in the window-divider colour, the body a little
// muted, an optional bold title above it, and the control's shortcut as a
// chip — the one spot of theme colour. Placed wholly below or above the
// control with the caret at its centre, a short fade in, and, as native's
// manager does for WCAG 1.4.13, shown on keyboard focus after a moment too;
// gone on a click, a key, a scroll or a blur.
//
// A control says what its tip is the way it always has: title="…". The first
// time it is pointed at, the title moves to data-tip so the browser's own
// bubble never shows; what it said to a screen reader moves to aria-label
// (a control with no words of its own) or aria-description, and an
// aria-label already there is untouched. A trailing "(⌘R)" is the
// shortcut chip; data-tip-title adds a heading.
import { settings } from "./theme.js";

const HOVER_DELAY = 500, FOCUS_DELAY = 650, WARM_WINDOW = 300, GAP = 4, MARGIN = 6, CARET_W = 14, CARET_H = 7, RADIUS = 8;
const CHORD = /^(?:[⌃⌥⇧⌘↩⌫⌦⎋⇥↑↓←→⇞⇟↖↘]+\S*|(?:Ctrl|Alt|Shift|Meta|Cmd|Win)(?:\+\S+)+|F\d{1,2}|Esc|Enter|Space|Tab)$/;

export function installTooltips(root = document.body) {
  const tip = document.createElement("div");
  tip.className = "sp-tip";
  tip.setAttribute("role", "tooltip");
  tip.hidden = true;
  const title = el("div", "sp-tip-title"), body = el("div", "sp-tip-body"), chip = el("span", "sp-tip-chip");
  // a tip may carry a link on (data-tip-link: where, data-tip-link-text: its words, "Docs" by default): the tip then
  // waits for the pointer to come onto it, and a click on the link goes there (the instrument page's dials: an opt's
  // tip, and its detail below)
  const link = el("a", "sp-tip-link");
  link.hidden = true;
  tip.append(title, body, chip, link);
  root.appendChild(tip);

  let pointerX = null;   // where the pointer is along a wide control, which the tip points at rather than its middle (place)
  let anchor = null, pending = null, timer = 0, hiddenAt = -Infinity, down = false;   // down: a control is being worked (a dial turned), and no tip is to open over it   // pending: the control a tip is on its way to   // when the last tip went: a page can boot in under the warm window

  // the tip a control carries: its title, moved out of the browser's reach the first time it is asked for
  function tipOf(node) {
    const t = node.closest?.("[title], [data-tip]");
    if (!t || t.closest(".cm-tooltip, .roll-tip, .sp-tip")) return null;
    if (t.hasAttribute("title")) {
      const title = t.getAttribute("title");
      t.dataset.tip = title;
      t.removeAttribute("title");
      // the title was a name, or a description, to a screen reader: it keeps being one (an icon's button has no other)
      // (as native's mirrorToolTipsToAccessibleDescriptions: a tip that says more than the name is its description)
      const name = t.getAttribute("aria-label") ?? (t.hasAttribute("aria-labelledby") ? null : t.textContent.trim());
      if (!name && !t.hasAttribute("aria-labelledby")) t.setAttribute("aria-label", title);
      else if (!t.hasAttribute("aria-description") && title !== name) t.setAttribute("aria-description", title);
    }
    const text = t.dataset.tip?.trim();
    if (!text) return null;
    let bodyText = text, shortcut = "";
    const m = /^(.*?)\s*\(([^()]+)\)$/.exec(text);
    // its key, or a pair of them (a size's smaller and larger: "⌃- / ⌃="), in the chip
    if (m && m[2].split(" / ").every((k) => CHORD.test(k.trim()))) { bodyText = m[1]; shortcut = m[2].trim(); }
    return { target: t, title: t.dataset.tipTitle ?? "", body: bodyText, shortcut, link: t.dataset.tipLink ?? "", linkText: t.dataset.tipLinkText ?? "Docs" };
  }

  function show(target, parts) {
    anchor = target;
    title.textContent = parts.title;
    title.hidden = !parts.title;
    body.textContent = parts.body;
    chip.textContent = parts.shortcut;
    chip.hidden = !parts.shortcut;
    link.hidden = !parts.link;
    link.textContent = parts.link ? `${parts.linkText} ↓` : "";
    link.href = parts.link || "#";
    tip.classList.toggle("interactive", !!parts.link);
    tip.classList.toggle("high-contrast", settings().scheme === "high_contrast");
    tip.hidden = false;
    place(target.getBoundingClientRect(), pointerX);
    tip.classList.remove("in");
    void tip.offsetWidth;
    tip.classList.add("in");
  }

  // wholly below the anchor when it fits, else above; centred on it, kept on
  // screen; the caret at the anchor's centre, within the bubble's straight edge. An anchor much wider than the tip (a
  // preferences row across the panel, its label at one end and its slider at the other) has it where the pointer is
  // along it instead: its middle can be far from what was pointed at. By the keyboard, there is no pointer: its middle.
  function place(r, px = null) {
    const vw = document.documentElement.clientWidth, vh = document.documentElement.clientHeight;
    const w = tip.offsetWidth, h = tip.offsetHeight;
    const wide = r.width > w * 1.5 && px != null && px >= r.left && px <= r.right;
    const cx = wide ? px : r.left + r.width / 2;
    let left = Math.round(Math.max(MARGIN, Math.min(cx - w / 2, vw - w - MARGIN)));
    const below = r.bottom + GAP + CARET_H + h <= vh - MARGIN || r.top - GAP - CARET_H - h < MARGIN;
    const top = Math.round(below ? r.bottom + GAP + CARET_H : r.top - GAP - CARET_H - h);
    const caret = Math.round(Math.max(RADIUS + CARET_W / 2, Math.min(cx - left, w - RADIUS - CARET_W / 2)));
    tip.style.left = `${left}px`;
    tip.style.top = `${top}px`;
    tip.style.setProperty("--caret-x", `${caret}px`);
    tip.classList.toggle("above", !below);
  }

  let leaving = 0;   // an interactive tip's grace while the pointer crosses the gap from its control to it
  function hide() {
    clearTimeout(timer);
    clearTimeout(leaving);
    leaving = 0;
    timer = 0;
    pending = null;
    if (!tip.hidden) hiddenAt = performance.now();
    tip.hidden = true;
    tip.classList.remove("in");
    anchor = null;
  }

  function arm(node, delay) {
    if (down) return;                 // mid-drag: a dial focuses itself as it is turned, and its tip would cover the panel
    if (tip.contains(node)) return;   // over the tip itself (an interactive one): it stays as it is
    const parts = tipOf(node);
    if (!parts) {
      if (anchor && !anchor.contains(node)) {
        if (tip.classList.contains("interactive")) { if (!leaving) leaving = setTimeout(hide, 250); }   // on its way to the tip: a moment to get there
        else hide();
      }
      return;
    }
    clearTimeout(leaving); leaving = 0;
    if (parts.target === anchor) return;
    clearTimeout(timer);
    // a tip that has just gone (the pointer moved to the next control) comes back at once, as native's does
    const warm = performance.now() - hiddenAt < WARM_WINDOW;
    // a tip with a link up: another control crossed on the way to it waits as long as the gap's grace, and reaching
    // the tip cancels it — so the pointer can get to the link past the controls between
    const crossing = !!anchor && !tip.hidden && tip.classList.contains("interactive");
    pending = parts.target;
    timer = setTimeout(() => { pending = null; show(parts.target, parts); }, crossing ? Math.max(250, warm ? 0 : delay) : warm ? 0 : delay);
  }

  // A root watched: the document's body, and each shadow root (the editor's and the panes': shadow.js, main.js). An event
  // leaving a shadow root is retargeted to its host, so a listener on the body never sees the control inside, nor
  // where the pointer went within it: each root's own listeners see those, and each watcher takes only the events of
  // its own tree (where the target is where the event began), or the body's would put away a tip the root's just armed
  const own = (e) => e.composedPath()[0] === e.target;
  function watch(r) {
    // Nothing is armed while a button is down: turning a dial drags the pointer over its parts and focuses it, and a
    // tip that opened mid-turn would cover the very controls being turned (and the code under them)
    r.addEventListener("pointerover", (e) => { if (own(e) && e.pointerType !== "touch" && !e.buttons) { pointerX = e.clientX; arm(e.target, HOVER_DELAY); } });
    r.addEventListener("pointermove", (e) => { if (own(e) && e.pointerType !== "touch" && !anchor) pointerX = e.clientX; }, { passive: true });   // until the tip shows: it doesn't chase the pointer after
    r.addEventListener("pointerout", (e) => {
      if (!own(e)) return;
      const to = e.relatedTarget;
      const t = e.target.closest?.("[data-tip], [title]");
      if (to && tip.contains(to)) return;   // onto the tip itself (one with a link on)
      if (t && !(to && t.contains(to))) {
        if (anchor === t && tip.classList.contains("interactive")) { clearTimeout(leaving); leaving = setTimeout(hide, 250); }   // time to reach it (arm below keeps to it, too)
        else if (anchor === t) hide();
        else if (timer) { clearTimeout(timer); timer = 0; }
      }
    });
    r.addEventListener("focusin", (e) => { if (own(e) && e.target.matches?.(":focus-visible")) { pointerX = null; arm(e.target, FOCUS_DELAY); } });
    r.addEventListener("focusout", (e) => { if (own(e) && !(e.relatedTarget && tip.contains(e.relatedTarget))) hide(); });   // focus into the tip (its link, pressed) is not away
    for (const type of ["pointerdown", "keydown", "wheel"]) r.addEventListener(type, (e) => { if (!tip.contains(e.target)) { hide(); hiddenAt = 0; } }, { capture: true, passive: true });   // hiddenAt: a tip put away to get on with something waits the full delay to come back, rather than the warm window's none
    r.addEventListener("pointerdown", (e) => { if (!tip.contains(e.target)) down = true; }, { capture: true, passive: true });   // down: a control is being worked, and no tip opens over it
  }
  watch(root);
  // the drag is over wherever it ends, pointer capture or not
  for (const type of ["pointerup", "pointercancel"]) window.addEventListener(type, () => { down = false; }, { capture: true, passive: true });
  // an interactive tip: kept while the pointer is on it, gone when it leaves (unless back to its control)
  tip.addEventListener("pointerenter", () => { clearTimeout(leaving); leaving = 0; clearTimeout(timer); timer = 0; pending = null; });
  tip.addEventListener("pointerleave", (e) => { if (!(e.relatedTarget && anchor?.contains(e.relatedTarget))) hide(); });
  link.addEventListener("mousedown", (e) => e.preventDefault());   // pressed, it takes no focus: a focus leaving would put the tip away before the click
  link.addEventListener("click", (e) => {
    const to = link.getAttribute("href");
    const target = to?.startsWith("#") ? document.getElementById(decodeURIComponent(to.slice(1))) : null;
    if (!target) return;
    e.preventDefault();
    hide();
    const still = matchMedia("(prefers-reduced-motion: reduce)").matches || document.body.classList.contains("reduce-motion");
    target.scrollIntoView({ behavior: still ? "auto" : "smooth", block: "start" });
    if (!target.hasAttribute("tabindex")) target.tabIndex = -1;
    target.focus({ preventScroll: true });
    target.classList.remove("tip-landed"); void target.offsetWidth; target.classList.add("tip-landed");   // where it went, lit a moment
  });
  // A scroll puts the tip away only when it moves what the tip is for: the page, or a box the control is in. Every
  // other box scrolling is nothing to it — the log and the cues scroll themselves to each new line while a program
  // plays, and a tip that went (or never came) with each of those was a tip never seen while anything ran.
  const holds = (box, node) => { for (let n = node; n; n = n.parentNode ?? n.host) if (n === box) return true; return false; };
  window.addEventListener("scroll", (e) => {
    const t = anchor ?? pending;
    if (!t) return;
    const box = e.target === document ? document.documentElement : e.target;
    if (holds(box, t)) hide();
  }, { capture: true, passive: true });
  window.addEventListener("resize", hide);
  window.addEventListener("blur", hide);

  return { hide, watch };
}

function el(tag, cls) { const e = document.createElement(tag); e.className = cls; return e; }
