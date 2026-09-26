// SPDX-License-Identifier: AGPL-3.0-or-later
// Shadow roots for the parts of the app that change as it plays. The page is to stay still while a program plays and
// nobody touches it (the browser check listens: "the page stays still …"), and what would change it goes, in order:
//
//   1. in the page, when it changes only as the player does something (a drawer, a dialog, Run, a card's Play): one
//      change is one scan, once;
//   2. in the page, when it can change without changing the page: drawn on a canvas (the scope, the stop's loading
//      comet), animated (a card's line flash, ui/card.js), hidden by its own animation (an announcement gone quiet,
//      announce.js), or written only as what it says changes (a scope's data-painted);
//   3. in a shadow root, when it must change the page as it plays: text to be read, selected and copied as it
//      arrives, and what a library draws as elements (CodeMirror's flashes). The smallest part that holds it, whole.
//
// The parts in shadow roots, each for the third reason; a new one is added here, with its reason, when 1 and 2 cannot do:
//
//   #editor-mount                 the buffer's editor: CodeMirror's line flashes, loop scopes and marks (editor.js)
//   #log, #cues                   their lines, as they arrive (main.js drawPane)
//   #insight-pane                 the Threads pane: its tree and timeline (insight.js)
//   #logs-pane, .debug-logs       the Logs, and the OSC to and from SuperSonic (logs.js)
//
// Why: a page-wide watcher neither hears a change inside a shadow root nor searches one. AdBlock's element hiding
// is such a watcher (eyeo's tracer, ewe-content.js): a MutationObserver on the whole document, and a second after
// any change every one of its hiding rules tried against the whole page with document.querySelector. A page that
// changes as often as a playing program's does had that scan running back to back, 0.36 s of every 1.36 s, the
// page's thread frozen through it (the sound, off that thread, carried on). Behind a shadow root the changes go
// unheard and the parts go unsearched, and the scan runs once, over the page's frame, and is done.
//
// A part keeps its host where it was, with its id, its role and its name: the page's landmarks, and a screen
// reader's map of them, are as they were. Its insides are in the shadow root, styled by the app's own stylesheet
// (one parsed copy, shared: appSheet). The rules that reach a part from outside it say so with :host — its host by
// id, :host(#editor-mount), and the page's states, which the root cannot see on <body>, said on the host
// (data-page, below): :host([data-page~="reduce-motion"]). An id reference (aria-labelledby, aria-controls, a
// label's for) cannot cross the boundary, so a part moves whole, with everything it names.
//
//   const root = shadowFor(host);   // the root, with the app's styles and the page's states
//   const pane = shadowPane(host);  // an element in it, laid out as the host's children were, to build into
//   eachShadowRoot((root) => …)     // every root, made and to come (the tooltips watch each)
//   origin(e)                       // the element an event began at, inside a root (e.target, from outside, is the host)
//   deepActive()                    // the focused element, inside a root (document.activeElement is the host)

const hosts = new Set();
let mirroring = null;

// The app's stylesheet (app.css), for every shadow root to adopt: one parsed copy, shared, made from the page's own
// (its rules read back, at once: no second load, and no root drawn unstyled while one arrived). A rule read back
// keeps all it says but one thing: a shorthand with a var() in it, then a longhand of it in the same rule, comes back
// with the longhands blank (`font: 600 var(--t-ui) …; line-height: normal` as `font-size: ; …`). So app.css writes
// none (a longhand folded into the shorthand, or in a rule of its own), the browser check fails on any, and any here
// is named in the console.
const own = [...document.styleSheets].find((s) => /\/app\.css(\?|$)/.test(s.href ?? ""));
const shared = new CSSStyleSheet({ baseURL: own?.href ?? document.baseURI });   // its url()s, as app.css's own
const text = own ? [...own.cssRules].map((r) => r.cssText).join("\n") : "";
shared.replaceSync(text);
const lost = [...text.matchAll(/([^{}]*)\{[^{}]*?([\w-]+):\s*;/g)].map((m) => `${m[1].trim()} (${m[2]})`);
if (lost.length) console.warn(`app.css rules read back without some of what they say, so the shadow roots miss it (shadow.js): ${lost.join(", ")}`);
export const appSheet = () => shared;

// <body>'s classes, the page's states (reduce-motion, info-open, dialog-open, …), said on every host as data-page:
// a part's rules read them as :host([data-page~="state"]). Said as the classes change, not asked each frame.
const paint = (host) => { host.dataset.page = document.body.className; };
function mirror() {
  if (mirroring) return;
  mirroring = new MutationObserver(() => { for (const h of hosts) paint(h); });
  mirroring.observe(document.body, { attributes: true, attributeFilter: ["class"] });
}

/** A shadow root on host, styled as the page is and told the page's states. */
export function shadowFor(host) {
  if (hosts.has(host)) return host.shadowRoot;
  const root = host.attachShadow({ mode: "open" });
  root.adoptedStyleSheets = [appSheet()];
  hosts.add(host);
  paint(host);
  mirror();
  for (const fn of told) fn(root);
  return root;
}

const told = new Set();
/** fn told of every shadow root, those made already and those to come: what listens on the page for its controls
 *  (the tooltips, tooltip.js) listens in each root as well. */
export function eachShadowRoot(fn) {
  told.add(fn);
  for (const h of hosts) fn(h.shadowRoot);
}

const panes = new WeakMap();   // host → its shadowPane
/** A part's insides, for a module that builds into an element (createLogs, createInsight, the log's window): an
 *  element in the host's shadow root that lays out as the host's own children did (display: contents, style.css
 *  .shadow-pane). The host keeps its layout and its scrolling, its id, its role and its name. An element laid out so
 *  has no box of its own: a module that watches or measures the element it is given (ui/shown.js's
 *  IntersectionObserver, a ResizeObserver) gives it one in style.css, as the Threads pane does (.shadow-pane.insight). */
export function shadowPane(host) {
  let pane = panes.get(host);
  if (!pane) {
    pane = document.createElement("div");
    pane.className = "shadow-pane";
    shadowFor(host).appendChild(pane);
    panes.set(host, pane);
  }
  return pane;
}

/** The element an event began at: inside a shadow root, where e.target (seen from outside it) is the host. */
export const origin = (e) => {
  const t = e.composedPath?.()[0];
  return t instanceof Element ? t : e.target;
};

/** The focused element, looked for inside shadow roots: document.activeElement, from outside one, is its host. */
export function deepActive() {
  let a = document.activeElement;
  while (a?.shadowRoot?.activeElement) a = a.shadowRoot.activeElement;
  return a;
}
