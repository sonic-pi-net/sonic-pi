// SPDX-License-Identifier: AGPL-3.0-or-later
// The sets, drawn from the workspace (workspace.js): a chip in the buffer tabs' slot, SET as JAM is, and the popover it
// opens. The popover has views, as a menu that goes deeper does: the list, and a set's details. The list: the set
// showing, as a card of its own with its Edit; the others to switch to, a row each, most recently used first, each with
// its details a tap away; New set at the foot. The details, a view in the list's place with the way back at its head:
// a new set's name and description, or a set's own, and for a set not showing, Delete. The list draws again whenever
// the workspace says something changed; a set's details are left alone while they are being typed.
import { icon } from "./icons.js";
import { NAME_MAX, DESCRIPTION_MAX } from "./workspace.js";

const ago = (t) => {
  const s = Math.round((Date.now() - t) / 1000);
  return s < 60 ? "just now" : s < 3600 ? `${Math.round(s / 60)} min ago` : s < 86400 ? `${Math.round(s / 3600)} h ago` : new Date(t).toLocaleDateString();
};

/** slot: where the chip goes (buffer-tabs.js); menu: the popover's element (#sets-menu); workspace; toast(text). */
export function createSetsView({ slot, menu, workspace, toast }) {
  const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };
  const iconEl = (tag, cls, name) => { const e = el(tag, cls); e.innerHTML = icon(name); return e; };
  // which view: the list, or a set's details (detailsOf: its id, or null for a new one)
  let view = "list", detailsOf = null;

  const chip = el("button", "set-chip");
  chip.type = "button";
  chip.setAttribute("aria-haspopup", "dialog");
  chip.setAttribute("aria-expanded", "false");
  chip.setAttribute("aria-controls", menu.id);
  chip.addEventListener("click", () => (menu.hidden ? open() : close()));
  slot.replaceChildren(chip);

  function paintChip() {
    const { name, description } = workspace.set();
    chip.textContent = "Set";   // a word, as JAM beside it is (style.css): the set's name is in its tip and at the popover's head
    chip.title = `${name}${description ? `: ${description}` : ""}. Switch sets`;
    chip.setAttribute("aria-label", `Set: ${name}. Switch sets`);
  }

  function render() {
    if (view === "details") return renderDetails();
    menu.textContent = "";
    menu.setAttribute("aria-label", "Switch sets");
    const all = workspace.sets(), here = all.find((s) => s.current), others = all.filter((s) => !s.current);

    // the set showing: a card, what it's called, what it's for and how much of it there is, and its Edit
    menu.append(el("div", "sv-kicker", "Showing"));
    const card = el("div", "sv-current");
    const words = el("div", "sv-words");
    words.append(el("div", "sv-name", here.name));
    if (here.description) words.append(el("div", "sv-about", here.description));
    words.append(el("div", "sv-meta", `${here.filled} of ${here.size} buffers`));
    const edit = el("button", "sv-edit", "Edit");
    edit.type = "button";
    edit.setAttribute("aria-label", `Edit "${here.name}": its name and description`);
    edit.addEventListener("click", () => details(here.id));
    card.append(iconEl("span", "sv-mark", "stack-2"), words, edit);
    menu.append(card);

    // the others: a tap switches to one; its ⋯ opens its details
    if (others.length) {
      menu.append(el("div", "sv-kicker", "Switch to"));
      const list = el("ul", "sv-list");
      for (const s of others) {
        const li = el("li", "sv-row");
        const pick = el("button", "sv-pick");
        pick.type = "button";
        const w = el("span", "sv-words"), top = el("span", "sv-top");
        top.append(el("span", "sv-name", s.name), el("span", "sv-when", ago(s.used)));
        w.append(top);
        // under its name, what it's for; with no description, how it starts
        if (s.description) w.append(el("span", "sv-about", s.description));
        else if (s.first) w.append(el("code", "sv-first", s.first));
        else w.append(el("span", "sv-about sv-empty", "empty"));
        pick.append(iconEl("span", "sv-mark", "stack-2"), w);
        pick.addEventListener("click", () => { workspace.switchSet(s.id); toast(`showing "${s.name}"`); close(); });
        const more = iconEl("button", "sv-more", "dots");
        more.type = "button";
        more.title = more.ariaLabel = `"${s.name}": name, description, delete`;
        more.addEventListener("click", () => details(s.id));
        li.append(pick, more);
        list.append(li);
      }
      menu.append(list);
    }

    const add = el("button", "sv-new");
    add.type = "button";
    add.append(iconEl("span", "sv-plus", "plus"), el("span", null, "New set"));
    add.addEventListener("click", () => details(null));
    menu.append(add);
  }

  // A set's details, in the list's place: a new set's (detailsOf null) or a set's own. Its name and description; for a
  // set not showing, Delete, which asks once more in its own place before it goes. Enter (in the name) or the button
  // keeps them; the way back, Cancel or Escape leaves things as they were.
  function renderDetails() {
    const s = detailsOf == null ? null : workspace.sets().find((x) => x.id === detailsOf);
    if (detailsOf != null && !s) { view = "list"; return render(); }   // gone meanwhile
    menu.textContent = "";
    menu.setAttribute("aria-label", s ? `Edit "${s.name}"` : "New set");
    const head = el("div", "sv-head");
    const back = iconEl("button", "sv-back", "chevron-left");
    back.type = "button";
    back.title = back.ariaLabel = "Back to your sets";
    back.addEventListener("click", toList);
    head.append(back, el("h2", "sv-title", s ? "Edit set" : "New set"));
    menu.append(head);

    const f = el("form", "sv-form");
    const field = (label, input, note) => {
      input.id = `sv-${label.toLowerCase()}`;
      const l = el("label", "sv-label", label);
      l.htmlFor = input.id;
      if (note) l.append(el("span", "sv-note", note));
      f.append(l, input);
    };
    const name = el("input", "sv-input");
    name.value = s ? s.name : ""; name.placeholder = "New Set"; name.maxLength = NAME_MAX; name.autocomplete = "off";
    const about = el("textarea", "sv-input sv-textarea");
    about.value = s ? s.description : ""; about.placeholder = "Describe this set..."; about.maxLength = DESCRIPTION_MAX; about.rows = 3;
    field("Name", name);
    field("Description", about, "optional");

    const foot = el("div", "sv-foot");
    if (s && !s.current) {
      // delete: asked twice, in its own place, the second time in the danger colour
      const del = el("button", "sv-delete", "Delete");
      del.type = "button";
      let armed = false;
      del.addEventListener("click", () => {
        if (!armed) { armed = true; del.textContent = "Delete for good"; del.classList.add("armed"); return; }
        workspace.removeSet(s.id);
        toast(`deleted "${s.name}"`);
        toList();
      });
      foot.append(del);
    }
    const cancel = el("button", "sv-cancel", "Cancel"), save = el("button", "sv-save", s ? "Save" : "Create");
    cancel.type = "button"; save.type = "submit";
    cancel.addEventListener("click", toList);
    foot.append(cancel, save);
    f.append(foot);
    f.addEventListener("submit", (e) => {
      e.preventDefault();
      if (s) workspace.editSet(s.id, { name: name.value, description: about.value });
      else { const r = workspace.newSet({ name: name.value, description: about.value }); toast(`new set "${r.name}"`); }
      toList();
    });
    menu.append(f);
    name.focus();
    if (s) name.setSelectionRange(name.value.length, name.value.length);   // the caret after its name, nothing selected
  }

  function details(id) { view = "details"; detailsOf = id; render(); }
  function toList() { view = "list"; detailsOf = null; render(); menu.querySelector(".sv-pick, .sv-edit")?.focus(); }

  function open() {
    view = "list"; detailsOf = null;
    render();
    // under the chip where there is the room, over it where there is not (the tabs sit at the editor's foot)
    const r = chip.getBoundingClientRect(), up = r.top > window.innerHeight - r.bottom;
    menu.classList.toggle("up", up);
    menu.style.setProperty("--picker-top", `${Math.round(r.bottom) + 6}px`);
    menu.style.setProperty("--picker-bottom", `${Math.round(window.innerHeight - r.top) + 6}px`);
    menu.style.setProperty("--menu-left", `${Math.round(Math.max(12, Math.min(r.left, window.innerWidth - 352)))}px`);
    menu.hidden = false;
    chip.setAttribute("aria-expanded", "true");
    menu.querySelector(".sv-pick, .sv-edit")?.focus();
  }
  function close(refocus = true) {
    if (menu.hidden) return;
    menu.hidden = true;
    view = "list"; detailsOf = null;
    chip.setAttribute("aria-expanded", "false");
    if (refocus) chip.focus();
  }
  document.addEventListener("pointerdown", (e) => { if (!menu.hidden && !menu.contains(e.target) && !chip.contains(e.target)) close(false); }, true);
  // Escape: from a set's details back to the list; from the list, shut
  document.addEventListener("keydown", (e) => {
    if (e.key !== "Escape" || menu.hidden) return;
    e.stopPropagation(); e.preventDefault();
    if (view === "details") toList(); else close();
  }, true);

  workspace.subscribe((e) => {
    if (e.kind !== "edit" && e.kind !== "buffer") paintChip();
    if (!menu.hidden && view === "list") render();
  });
  paintChip();
  return { open, close };
}
