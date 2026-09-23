// SPDX-License-Identifier: AGPL-3.0-or-later
// The buffer tabs, drawn from the workspace (workspace.js): as many pads as the set showing has buffers, the one
// showing filled, one with code in it brighter than an empty one. Before them a slot the sets' chip goes in
// (sets-view.js), after them JAM. With jam on the pads grow to drum pads, fire on the press rather than the lift, and
// a tap does what the mode picker above them says: run (switch and run, layering, as Run does), show (switch only),
// exec (switch, stop what the buffers are playing, and run: the drum pad's replace). JAM steps through its layouts:
// off, a row of pads, a grid of big ones in two rows (style.css). A set of another size has its pads made again.
import { icon } from "./icons.js";

export const JAM_LAYOUTS = ["off", "row", "grid"];
const JAM_MODES = [["run", "run", "Tapping a pad will play its buffer on top of whatever is already playing"], ["show", "show", "Tapping a pad will just switch to its buffer"], ["exec", "exec", "Tapping a pad will stop the other buffers and play just this one"]];

/**
 * root: the element the tabs are drawn in; workspace; title(i): a pad's title (its shortcut with it); onPick(i): a pad
 * tapped or its shortcut pressed; store: { get(key, fallback), set(key, value) } for the jam's layout and mode.
 */
export function createBufferTabs({ root, workspace, title, onPick, store }) {
  const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };
  const saved = store.get("sp-jam", "off");   // an older true was the row
  const jam = { layout: saved === true ? "row" : JAM_LAYOUTS.includes(saved) ? saved : "off", mode: store.get("sp-jam-mode", "run"), get on() { return this.layout !== "off"; } };

  // made once: the row, and in it the chip's slot, the modes, and JAM; the pads made again whenever the size changes
  const modes = el("div", "jam-modes");
  modes.setAttribute("role", "radiogroup");
  modes.setAttribute("aria-label", "What tapping a pad will do");
  for (const [id, label, tip] of JAM_MODES) {
    const b = el("button", "jam-mode", label);
    b.type = "button"; b.dataset.mode = id; b.title = tip; b.setAttribute("role", "radio");
    b.addEventListener("click", () => { jam.mode = id; store.set("sp-jam-mode", id); paint(); });
    modes.append(b);
  }
  const row = el("div", "buffer-tabs-row"), slot = el("span", "set-slot"), list = el("div", "buffer-tabs-list");
  list.setAttribute("role", "tablist");
  list.setAttribute("aria-label", "Buffers");
  const toggle = el("button", "jam-toggle");
  toggle.type = "button";
  toggle.addEventListener("click", () => { jam.layout = JAM_LAYOUTS[(JAM_LAYOUTS.indexOf(jam.layout) + 1) % JAM_LAYOUTS.length]; store.set("sp-jam", jam.layout); paint(); });
  // the tablist holds tabs alone; the chip, the modes (shown with jam on) and JAM beside it. One row, so jam's layouts
  // put the modes right against the pads (style.css): over them, or on the line of the chip and JAM
  row.append(slot, modes, list, toggle);
  root.replaceChildren(row);
  root.setAttribute("aria-label", "Buffers");

  let made = 0;
  function makePads(n) {
    list.textContent = "";
    root.style.setProperty("--pads", String(n));
    root.style.setProperty("--pad-cols", String(Math.ceil(n / 2)));   // the grid: two rows
    for (let i = 0; i < n; i++) {
      if (i) { const sep = el("span", "buffer-tab-sep", "|"); sep.setAttribute("aria-hidden", "true"); list.append(sep); }
      const b = el("button", "buffer-tab", String(i));
      b.type = "button";
      b.dataset.idx = i;
      b.title = title(i);
      b.setAttribute("role", "tab");
      b.setAttribute("aria-label", `Buffer ${i}`);
      // a pad: with jam on it fires as the finger lands (a drum pad's feel); otherwise on the lift, unless it was a drag
      let x0 = 0, y0 = 0, dragged = false, firedOnDown = false;
      const fire = () => {
        onPick(i);
        b.classList.remove("tab-tapped"); void b.offsetWidth; b.classList.add("tab-tapped");   // the flash, restarted each time
      };
      b.addEventListener("pointerdown", (e) => { x0 = e.clientX; y0 = e.clientY; dragged = false; firedOnDown = jam.on; if (jam.on) { e.preventDefault(); fire(); } });
      b.addEventListener("pointermove", (e) => { if (Math.abs(e.clientX - x0) > 8 || Math.abs(e.clientY - y0) > 8) dragged = true; });
      b.addEventListener("pointerup", (e) => { if (firedOnDown) { firedOnDown = false; return; } if (dragged) return; e.preventDefault(); fire(); });
      b.addEventListener("click", (e) => { if (e.detail === 0) fire(); });   // the keyboard's Enter or Space
      list.append(b);
    }
    made = n;
  }

  function paint() {
    if (workspace.size !== made) makePads(workspace.size);
    root.classList.toggle("jam-on", jam.on);
    root.classList.toggle("jam-grid", jam.layout === "grid");
    toggle.classList.toggle("on", jam.on);
    toggle.setAttribute("aria-pressed", String(jam.on));
    toggle.innerHTML = jam.layout === "grid" ? `JAM ${icon("layout-grid", "jam-grid-glyph")}` : "JAM";
    toggle.title = { off: "Jam mode: the buffer pads will play the moment you tap them", row: "Jam mode is on, a row of pads: tap for big pads", grid: "Jam mode is on, big pads: tap to turn jam off" }[jam.layout];
    toggle.setAttribute("aria-label", { off: "Jam mode off", row: "Jam mode on, a row of pads", grid: "Jam mode on, big pads" }[jam.layout]);
    for (const b of modes.children) { const on = b.dataset.mode === jam.mode; b.classList.toggle("on", on); b.setAttribute("aria-checked", String(on)); }
    const active = workspace.active;
    for (const b of list.querySelectorAll(".buffer-tab")) {
      const i = Number(b.dataset.idx), on = i === active;
      b.classList.toggle("active", on);
      b.setAttribute("aria-selected", String(on));
      b.classList.toggle("has-code", !!workspace.text(i).trim());
    }
  }
  workspace.subscribe(paint);
  paint();

  return {
    jam,
    /** where the sets' chip goes (sets-view.js) */
    slot,
    paint,
    /** the pads' titles again, as the keys change */
    retitle() { for (const b of list.querySelectorAll(".buffer-tab")) b.title = title(Number(b.dataset.idx)); },
  };
}
