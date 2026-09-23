// SPDX-License-Identifier: AGPL-3.0-or-later
// The submenu: a page's or a pane's list of what is in it, down the left on a wide screen and folded behind a
// chip on a phone — the docs pane's list of topics (with its filter), a site page's sections, the examples by
// level, the web tutorial's contents. Group labels in the accent, items with an icon from the app's set, the
// current one lit; picked, an item calls back and the fold closes.
//
//   const menu = createSubmenu({ label: "Contents", filter: "Filter…", onPick: (key) => … });
//   menu.set([{ group: "About" }, { key: "intro", title: "Sonic Pi", icon: svg }, …]);
//   menu.active("intro");
//   page.append(menu.el, main);   // menu.el is the fold strip holding the list (./fold.js)
//   createSubmenu({ adopt: bar, onPick })   // a list the page already has (a site page's), made live
//
// The rules are ./submenu.css: the list (.docs-list, .docs-group, .docs-item), the filter (.docs-search-wrap),
// the column (.ic-side) and the fold (.ic-fold-bar, .ic-fold). On a wide screen the column scrolls on its own;
// under 820px of container the list drops open over the page from the strip, or from a chip the host puts
// elsewhere (the site bar's: fold.js's toggleFold).
import { fold, adoptFold } from "../fold.js";

const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };
const SEARCH = '<svg viewBox="0 0 24 24" aria-hidden="true"><circle cx="10.5" cy="10.5" r="6.5"/><path d="m15.5 15.5 5 5"/></svg>';

/**
 * @param o.label    the fold chip's word ("Contents", "All examples")
 * @param o.filter   a placeholder: the list gets a filter box that narrows it as you type (null: none)
 * @param o.onPick   (key, item, event) => void: an item chosen. An item with an href and no onPick is a plain link.
 * @param o.onFilter (count) => void: the filter typed, this many items left
 * @param o.className extra classes for the column
 */
export function createSubmenu({ label = "Contents", filter = null, onPick = null, onFilter = null, className = "", adopt = null } = {}) {
  if (adopt) return adopted(adopt, onPick);
  const side = el("nav", `ic-side ${className}`.trim());
  const list = el("div", "docs-list");
  let search = null;
  if (filter) {
    const wrap = el("label", "docs-search-wrap");
    wrap.innerHTML = SEARCH;
    search = el("input", "docs-search");
    search.type = "search";
    search.placeholder = filter;
    search.setAttribute("aria-label", filter.replace(/…$/, ""));
    search.addEventListener("input", () => render());
    wrap.appendChild(search);
    side.appendChild(wrap);
  }
  side.appendChild(list);
  const bar = fold(side, label);

  let items = [], activeKey = null;
  const nodes = new Map();   // key → the item's element

  function render() {
    const q = search?.value.trim().toLowerCase() ?? "";
    list.textContent = "";
    nodes.clear();
    for (const it of items) {
      if (it.group != null) { if (!q) list.appendChild(el("div", "docs-group", it.group)); continue; }
      if (q && !it.title.toLowerCase().includes(q) && !String(it.key).toLowerCase().includes(q)) continue;
      const node = el(it.href && !onPick ? "a" : it.href ? "a" : "button", `docs-item${it.sub ? " sub" : ""}${it.absent ? " absent" : ""}${it.className ? ` ${it.className}` : ""}`);
      if (node.tagName === "BUTTON") node.type = "button";
      if (it.href) node.href = it.href;
      if (it.icon) node.insertAdjacentHTML("beforeend", it.icon);
      if (it.iconEl) node.prepend(...[].concat(it.iconEl));
      node.appendChild(el("span", "", it.title));
      if (it.detail != null) node.appendChild(el("span", "docs-item-detail", it.detail));
      if (it.hint) node.title = it.hint;
      node.dataset.key = it.key;
      if (onPick || it.onPick) node.addEventListener("click", (ev) => { if (!it.href || it.onPick || onPick) ev.preventDefault(); (it.onPick ?? onPick)?.(it.key, it, ev); });
      list.appendChild(node);
      nodes.set(it.key, node);
    }
    if (activeKey != null) { nodes.get(activeKey)?.classList.add("active"); nodes.get(activeKey)?.setAttribute("aria-current", "page"); }
    if (q) onFilter?.(nodes.size);
  }

  const api = {
    el: bar, side, list, search,
    /** The items, in order: { group } labels among { key, title, detail, icon (svg markup), iconEl, href, sub, absent, hint, className, onPick }. */
    set(next) { items = next; render(); },
    /** The item lit, by key (null for none); scrolled into view. */
    active(key, { scroll = true } = {}) {
      const was = activeKey != null ? nodes.get(activeKey) : null;
      was?.classList.remove("active"); was?.removeAttribute("aria-current");
      activeKey = key;
      const node = key != null ? nodes.get(key) : null;
      node?.classList.add("active"); node?.setAttribute("aria-current", "page");   // the page shown, said as such
      const it = key != null ? items.find((i) => i.key === key) : null;
      api.setLabel(it?.title ?? label);   // the chip names where you are, not the list: "Contents" only before anything is lit
      if (node && scroll) node.scrollIntoView({ block: "nearest" });
    },
    get activeKey() { return activeKey; },
    /** The element for a key. */
    node(key) { return nodes.get(key) ?? null; },
    /** The fold (a phone): open or shut it, or shut it. */
    toggleFold: () => bar.toggleFold(), closeFold: () => bar.closeFold(),
    get label() { return bar.foldLabel; },
    /** The chip's word, changed (the buffer picker's names the buffer). */
    setLabel(text) { bar.foldLabel = text; bar.querySelector(".ic-fold > span").textContent = text; },
    /** Whether the fold is open. */
    get open() { return side.classList.contains("open"); },
  };
  return api;
}

/**
 * A list already in the page (a site page's, as scripts/build-site.mjs writes it): its strip made to fold, its items
 * picked through onPick, the same api as a made one's for lighting and folding.
 * @param bar the page's .ic-fold-bar
 */
function adopted(bar, onPick) {
  adoptFold(bar);
  const side = bar.querySelector(".ic-side"), list = side.querySelector(".docs-list");
  const nodes = new Map([...list.querySelectorAll(".docs-item[data-key]")].map((n) => [n.dataset.key, n]));
  const label = bar.foldLabel;
  const picks = new Map();   // key → its own onPick, where an item has one
  for (const [key, node] of nodes) node.addEventListener("click", (ev) => { const pick = picks.get(key) ?? onPick; if (!pick) return; ev.preventDefault(); pick(key, { key, href: node.getAttribute("href") }, ev); });
  let activeKey = null;
  const api = {
    el: bar, side, list, search: null,
    /** An item's own pick, in place of the list's (the examples: a card brought out). */
    onPick(key, fn) { picks.set(key, fn); },
    active(key, { scroll = true } = {}) {
      const was = activeKey != null ? nodes.get(activeKey) : null;
      was?.classList.remove("active"); was?.removeAttribute("aria-current");
      activeKey = key;
      const node = key != null ? nodes.get(key) : null;
      node?.classList.add("active"); node?.setAttribute("aria-current", "location");   // the section being read, said as such
      api.setLabel(node?.querySelector("span:last-of-type")?.textContent ?? label);
      if (node && scroll) node.scrollIntoView({ block: "nearest" });
    },
    get activeKey() { return activeKey; },
    node: (key) => nodes.get(key) ?? null,
    keys: () => [...nodes.keys()],
    toggleFold: () => bar.toggleFold(), closeFold: () => bar.closeFold(),
    get label() { return bar.foldLabel; },
    setLabel(text) { bar.foldLabel = text; bar.querySelector(".ic-fold > span").textContent = text; },
    get open() { return side.classList.contains("open"); },
  };
  return api;
}
