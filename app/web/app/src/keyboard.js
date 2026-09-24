// SPDX-License-Identifier: AGPL-3.0-or-later
// The code keyboard. On a touch screen the system keyboard stays away (inputmode="none") and a keyboard
// shaped for code docks at the bottom instead: letters, the symbols Sonic Pi
// leans on (: , . _ [ ] ( ) ", the most used in its examples and tutorial),
// arrows, and on its 123 layer the digits and the rest of the symbols, each where
// its partner is on the letters (: =, " ', _ -, [ ] ( ) { } < >, and , . as they
// are), and in the space bar's place the editor's verbs (undo, redo, cut, copy,
// paste, select all, comment): every row the same keys in the same places on
// both layers, the space bar's seven columns a key each. It opens while the finger is still down on the editor
// (after the delay the player chose), or when the iOS magnifier drags the
// caret; ⌄, in the corner, puts it away. A preference switches back to the
// system keyboard.
import { undo, redo, cursorCharLeft, cursorCharRight, cursorLineUp, cursorLineDown, deleteCharBackward } from "@codemirror/commands";
import { acceptCompletion, completionStatus, startCompletion, moveCompletionSelection } from "@codemirror/autocomplete";
import { EditorView } from "@codemirror/view";
import { newlineAndIndent, toggleComment } from "./editor.js";

// simple-keyboard, fetched the first time the keyboard opens (a page never typed on by touch never loads it). Its
// main build is CommonJS with its class on `default`, which a bundler wraps once more: take the constructor wherever
// it lands.
let keyboardClass = null;
const loadKeyboard = () => (keyboardClass ??= import("simple-keyboard").then((m) => m.default?.default ?? m.SimpleKeyboard ?? m.default));

// the 123 pillar bottom-left, as a phone's is, Tab above it where a keyboard has it; ← and → side by side (under
// ↑ ↓, which walk a completion list), nothing between them; ⌄ in the corner, under return
const BOTTOM = "{shift2} {space} {left} {right} {hide}";
const LAYOUT = {
  default: [
    "q w e r t y u i o p",
    "{caps} a s d f g h j k l \"",
    "{tab} z x c v b n m _ {bksp}",
    "{shift} : , . [ ] ( ) {up} {down} {enter}",
    BOTTOM,
  ],
  caps: [
    "Q W E R T Y U I O P",
    "{caps} A S D F G H J K L \"",
    "{tab} Z X C V B N M _ {bksp}",
    "{shift} : , . [ ] ( ) {up} {down} {enter}",
    BOTTOM,
  ],
  shift: [
    "1 2 3 4 5 6 7 8 9 0",
    "@ ^ + * / % ! ? & | '",
    "{tab} $ ; \\ ` # ~ \" - {bksp}",
    "{shift} = , . { } < > {up} {down} {enter}",   // , and . where they are on the letters: a number is followed by one, or has one
    "{shift2} {undo} {redo} {cut} {copy} {paste} {selectall} {comment} {left} {right} {hide}",
  ],
};
const DISPLAY = {
  "{bksp}": "⌫", "{enter}": "↵", "{shift}": "123", "{shift2}": "123", "{caps}": "⇪︎",
  "{space}": " ", "{hide}": "⌄", "{undo}": "↶", "{redo}": "↷", "{comment}": "⌗", "{selectall}": "⬚", "{cut}": "✂︎", "{copy}": "⧉", "{paste}": "⎘",
  "{left}": "←", "{up}": "↑", "{down}": "↓", "{right}": "→", "{tab}": "⇥",
};
const LETTERS = "q w e r t y u i o p a s d f g h j k l z x c v b n m Q W E R T Y U I O P A S D F G H J K L Z X C V B N M";

const coarse = () => window.matchMedia("(pointer: coarse)").matches;
const important = (el, prop, value) => el.style.setProperty(prop, value, "important");

/**
 * @param editor the editor (editor.js createEditor): its view
 * @param dock the element the keyboard docks in, with a #code-keyboard inside
 * @param mount the editor's mount, whose touches open the keyboard
 * @param store the page's preference store (get(key, fallback), set(key, value))
 */
export function createCodeKeyboard({ editor, dock, mount, store }) {
  // the editor the keys go to: the buffer's, or a card's while its code is being edited (aim, below)
  let view = editor.view;
  let content = view.contentDOM;
  // no QuickType bar, no autocorrect, whichever keyboard types
  const settle = (c) => { for (const [k, v] of [["autocorrect", "off"], ["autocapitalize", "none"], ["autocomplete", "off"], ["spellcheck", "false"]]) c.setAttribute(k, v); };
  settle(content);

  let virtual = store.get("sp-virtual-kbd", coarse());
  let delay = store.get("sp-kbd-delay", 0);
  let capsLock = false, shiftMode = false;
  const kbdEl = dock.querySelector("#code-keyboard");
  kbdEl.classList.add("code-kbd");

  const selecting = () => { const { from, to } = view.state.selection.main; return from !== to; };
  const insert = (text) => view.dispatch({ ...view.state.replaceSelection(text), userEvent: "input.type", scrollIntoView: true });

  // built the first time it opens (setOpen): its keys are hundreds of elements no one may ever touch
  let kbd = null;
  const build = (Keyboard) => (kbd ??= new Keyboard(".code-kbd", {
    layout: LAYOUT,
    display: DISPLAY,
    buttonTheme: [{ class: "kbd-letter", buttons: LETTERS }],
    layoutName: "default",
    preventMouseDownDefault: true,
    // The shapes, by row: half-width keys at the caps row's ends; Tab and ⌫ wide; the 123 pillar
    // bottom-left (two real keys, abutted in CSS). The last two rows share 11
    // columns exactly, so ↑ ↓ stand over ← → and ⌄ under return.
    onRender: () => {
      const [, capsRow, tabRow, altRow, bottomRow] = kbdEl.querySelectorAll(".hg-row");
      kbdEl.querySelector('.hg-button[data-skbtn="{caps}"]')?.classList.toggle("caps-locked", capsLock);
      for (const btn of [capsRow?.firstElementChild, capsRow?.lastElementChild]) if (btn) { important(btn, "flex", "0.5 1 0"); important(btn, "min-width", "0"); }   // the row's half-width ends
      for (const btn of [tabRow?.firstElementChild, tabRow?.lastElementChild]) if (btn) { important(btn, "flex", "1.5 1 0"); important(btn, "min-width", "0"); important(btn, "font-size", "18px"); }
      // a key n columns wide is n columns and the n - 1 gaps inside it (flex-grow would share out what the gaps
      // leave, and the rows' different numbers of gaps would drift them apart)
      const cols = (btn, n) => { important(btn, "flex", `0 0 calc(${n} * (100% - 10 * var(--kbd-gap)) / 11 + ${n - 1} * var(--kbd-gap))`); important(btn, "min-width", "0"); };
      for (const btn of altRow?.querySelectorAll(".hg-button") ?? []) {
        const key = btn.getAttribute("data-skbtn");
        cols(btn, 1);
        if (key === "{shift}") { important(btn, "font-size", "14px"); important(btn, "border-bottom-left-radius", "0"); important(btn, "border-bottom-right-radius", "0"); }
        else if (key === "{up}" || key === "{down}") important(btn, "font-size", "20px");
        else if (key === "{enter}") important(btn, "font-size", "18px");
      }
      for (const btn of bottomRow?.querySelectorAll(".hg-button") ?? []) {
        const key = btn.getAttribute("data-skbtn");
        cols(btn, key === "{space}" ? 7 : 1);   // the 123 layer's seven verbs, a column each, in the space bar's place
        if (key === "{shift2}") { important(btn, "border-top-left-radius", "0"); important(btn, "border-top-right-radius", "0"); }
        else if (key === "{left}" || key === "{right}") important(btn, "font-size", "20px");
      }
      kbdEl.classList.toggle("caps-locked", capsLock);
    },
    onKeyPress: (button) => {
      if (button === "{hide}") return setOpen(false);
      switch (button) {
        // return accepts a completion, else a new line indented as the editor's own Enter indents it
        case "{enter}": if (completionStatus(view.state) === "active") acceptCompletion(view); else newlineAndIndent(view); break;
        case "{bksp}": deleteCharBackward(view); break;
        case "{shift}": case "{shift2}":
          shiftMode = !shiftMode;
          kbd.setOptions({ layoutName: shiftMode ? "shift" : capsLock ? "caps" : "default" });
          break;
        case "{caps}":
          capsLock = !capsLock;
          if (!shiftMode) kbd.setOptions({ layoutName: capsLock ? "caps" : "default" });
          break;
        case "{space}":
          if (spaceHeld) break;                          // held: the trackpad (or just a long press), never a run of spaces
          insert(" ");
          lastSpace = { pos: view.state.selection.main.head, at: performance.now() };
          break;
        case "{undo}": undo(view); break;
        case "{redo}": redo(view); break;
        case "{comment}": toggleComment(view); break;
        case "{selectall}": {
          const { from, to, head } = view.state.selection.main;
          view.dispatch({ selection: from === to ? { anchor: 0, head: view.state.doc.length } : { anchor: head } });
          break;
        }
        case "{cut}": {
          const { from, to } = view.state.selection.main;
          if (from !== to) {
            navigator.clipboard?.writeText(view.state.sliceDoc(from, to));
            view.dispatch({ changes: { from, to }, userEvent: "delete.cut" });
          }
          break;
        }
        case "{copy}": {
          const { from, to } = view.state.selection.main;
          if (from !== to) navigator.clipboard?.writeText(view.state.sliceDoc(from, to));
          break;
        }
        case "{paste}":
          navigator.clipboard?.readText().then((text) => { if (text) { insert(text); view.focus(); } }).catch(() => {});
          break;
        case "{left}": cursorCharLeft(view); break;
        case "{right}": cursorCharRight(view); break;
        // while the completion popup is up, the arrows walk it (editor.js Up and Down)
        case "{up}": if (completionStatus(view.state) === "active") moveCompletionSelection(false)(view); else cursorLineUp(view); break;
        case "{down}": if (completionStatus(view.state) === "active") moveCompletionSelection(true)(view); else cursorLineDown(view); break;
        case "{tab}": startCompletion(view); break;
        default: insert(button);
      }
      view.focus();
    },
  }));

  // ── Hold space to move the caret, as iOS's keyboard does ──
  // A press on space held still for a moment turns the keyboard into a trackpad:
  // the keys fade, and dragging walks the caret a character per few pixels across
  // and a line per row of pixels up or down. Letting go ends it. simple-keyboard
  // types the press on pointerdown, before the hold can be known: that space is
  // taken back as the trackpad arms, and its hold-repeats are ignored meanwhile.
  const TRACKPAD_HOLD = 320, TRACKPAD_SLOP = 6, CHAR_PX = 9, LINE_PX = 20;
  let trackpad = null, lastSpace = null;
  // true from the press until the release: simple-keyboard's hold-repeat of the space is ignored meanwhile
  let spaceHeld = false;
  kbdEl.addEventListener("pointerdown", (e) => {
    const btn = e.target.closest?.('.hg-button[data-skbtn="{space}"]');
    if (!btn) return;
    spaceHeld = true;
    if (trackpad) return;
    // the press's own space, typed a moment ago by the button's handler
    const typed = lastSpace && performance.now() - lastSpace.at < 80 ? lastSpace.pos : null;
    const t = (trackpad = { id: e.pointerId, btn, x: e.clientX, y: e.clientY, ax: 0, ay: 0, on: false, timer: 0, typed });
    t.timer = setTimeout(() => {
      t.on = true;
      kbdEl.classList.add("kbd-trackpad");
      const head = view.state.selection.main.head;
      if (t.typed != null && head === t.typed && view.state.sliceDoc(head - 1, head) === " ") view.dispatch({ changes: { from: head - 1, to: head }, userEvent: "delete" });
      try { btn.setPointerCapture(t.id); } catch {}
    }, TRACKPAD_HOLD);
  });
  kbdEl.addEventListener("pointermove", (e) => {
    const t = trackpad;
    if (!t || e.pointerId !== t.id) return;
    const dx = e.clientX - t.x, dy = e.clientY - t.y;
    if (!t.on) { if (Math.hypot(dx, dy) > TRACKPAD_SLOP) { clearTimeout(t.timer); trackpad = null; } return; }   // a drag before the hold is not this
    t.ax += dx; t.ay += dy; t.x = e.clientX; t.y = e.clientY;
    while (t.ax >= CHAR_PX) { cursorCharRight(view); t.ax -= CHAR_PX; }
    while (t.ax <= -CHAR_PX) { cursorCharLeft(view); t.ax += CHAR_PX; }
    while (t.ay >= LINE_PX) { cursorLineDown(view); t.ay -= LINE_PX; }
    while (t.ay <= -LINE_PX) { cursorLineUp(view); t.ay += LINE_PX; }
  });
  // any release, anywhere: a finger that slid off the keyboard still lets go
  const endTrackpad = (e) => {
    const t = trackpad;
    if (t && e.pointerId !== t.id) return;
    spaceHeld = false;
    if (!t) return;
    clearTimeout(t.timer);
    trackpad = null;
    if (!t.on) return;
    kbdEl.classList.remove("kbd-trackpad");
    try { t.btn.releasePointerCapture(t.id); } catch {}
  };
  window.addEventListener("pointerup", endTrackpad, true);
  window.addEventListener("pointercancel", endTrackpad, true);
  window.addEventListener("blur", () => endTrackpad({ pointerId: trackpad?.id }));

  // tapping keys keeps focus (and the caret) in the editor
  kbdEl.addEventListener("pointerdown", (e) => e.stopPropagation());
  kbdEl.addEventListener("mousedown", (e) => e.preventDefault());
  kbdEl.addEventListener("touchstart", (e) => e.preventDefault(), { passive: false });

  // ── The dock: its height drives the layout above it (--kbd-h, style.css) ──
  const isOpen = () => document.body.classList.contains("kbd-open");
  const measure = () => { const h = dock.offsetHeight; if (h > 0) document.documentElement.style.setProperty("--kbd-h", `${h}px`); };
  new ResizeObserver(() => requestAnimationFrame(measure)).observe(dock);
  function setOpen(on) {
    if (on && !virtual) return;
    if (isOpen() === on) return;
    if (on && !kbd) { loadKeyboard().then((Keyboard) => { build(Keyboard); setOpen(true); }); return; }
    dock.hidden = !on;
    document.body.classList.toggle("kbd-open", on);
    if (on) requestAnimationFrame(() => { measure(); view.dispatch({ effects: [], scrollIntoView: true }); });
  }

  // ── Opening it: the touch trigger ──
  // During the hold, once the delay is reached (so a finger holding to place the
  // caret with the magnifier gets it without having to let go); on release after
  // at least the delay; never while a selection is being dragged. A mouse opens
  // it on click when the preference forces it on a desktop.
  let tapX = 0, tapY = 0, tapPointer = -1, tapStart = 0, timer = null;
  const TAP_THRESHOLD = 10;
  const clearTimer = () => { if (timer) { clearTimeout(timer); timer = null; } };
  mount.addEventListener("pointerdown", (e) => {
    tapX = e.clientX; tapY = e.clientY; tapPointer = e.pointerId; tapStart = Date.now();
    clearTimer();
    if (coarse() && delay > 0) {
      const started = e.pointerId;
      timer = setTimeout(() => {
        timer = null;
        if (tapPointer === started && !selecting()) setOpen(true);
      }, delay);
    }
  });
  mount.addEventListener("pointermove", (e) => {
    if (e.pointerId !== tapPointer) return;
    if (Math.abs(e.clientX - tapX) > TAP_THRESHOLD || Math.abs(e.clientY - tapY) > TAP_THRESHOLD) { tapPointer = -1; clearTimer(); }
  });
  mount.addEventListener("pointerup", (e) => {
    if (e.pointerId !== tapPointer) return;
    const held = Date.now() - tapStart;
    tapPointer = -1;
    clearTimer();
    if (coarse() && held < delay) return;
    setTimeout(() => { if (!selecting()) setOpen(true); }, 0);
  });
  mount.addEventListener("pointercancel", () => { tapPointer = -1; clearTimer(); });

  // The iOS magnifier: a press-and-hold that drags the caret fires a second
  // selectionchange during the touch (the first is the tap placing it); that
  // alone opens the keyboard, however long the hold.
  let touchStart = 0, selChanges = 0, openedThisTouch = false;
  mount.addEventListener("pointerdown", () => {
    if (!coarse()) return;
    touchStart = Date.now(); selChanges = 0; openedThisTouch = false;
  }, true);
  document.addEventListener("selectionchange", () => {
    if (!touchStart || openedThisTouch) return;
    if (++selChanges < 2 || Date.now() - touchStart < 80) return;
    openedThisTouch = true;
    setOpen(true);
  });
  mount.addEventListener("pointerup", () => { touchStart = 0; }, true);
  mount.addEventListener("pointercancel", () => { touchStart = 0; }, true);

  // With a mouse, a click away puts it away; on touch only ⌄ does
  document.addEventListener("pointerdown", (e) => {
    if (!isOpen() || coarse()) return;
    if (dock.contains(e.target) || mount.contains(e.target) || e.composedPath().some((n) => n.classList?.contains?.("qs-card")) || e.target.closest?.("#buffer-tabs, #drawer-divider")) return;
    setOpen(false);
  });

  // ── Aim: a card's editor (ui/card.js) takes the keys while its code has focus ──
  // Focus is a composed event, so a card in the docs pane and one in a site page's shadow root alike are seen;
  // the editor is found from its content element. On a touch screen the keyboard comes up for it as it does
  // for the buffer's; the buffer's editor takes the keys back when it is focused again.
  function aim(v) {
    if (v === view) return;
    if (virtual && content !== editor.view.contentDOM) content.removeAttribute("inputmode");
    view = v;
    content = v.contentDOM;
    settle(content);
    if (virtual) content.setAttribute("inputmode", "none");
  }
  document.addEventListener("focusin", (e) => {
    const el = e.composedPath()[0];
    if (!(el instanceof Element) || !el.classList.contains("cm-content")) return;
    if (el === editor.view.contentDOM) return aim(editor.view);
    if (!el.closest(".qs-card")) return;
    const v = EditorView.findFromDOM(el);
    if (!v) return;
    aim(v);
    if (coarse()) setTimeout(() => { if (view === v && !selecting()) setOpen(true); }, 0);
  });

  // ── Virtual or system ──
  function applyMode(on, refocus) {
    virtual = on;
    document.body.classList.toggle("force-kbd", on);
    for (const c of new Set([content, editor.view.contentDOM])) { if (on) c.setAttribute("inputmode", "none"); else c.removeAttribute("inputmode"); }
    if (!on) {
      setOpen(false);
      // the system keyboard comes up for an editor that is focused again
      if (refocus && document.activeElement === content) setTimeout(() => { content.blur(); content.focus(); }, 100);
    }
  }
  applyMode(virtual, false);

  return {
    get virtual() { return virtual; },
    setVirtual(on) { applyMode(on, true); store.set("sp-virtual-kbd", on); },
    get delay() { return delay; },
    setDelay(ms) { delay = Math.max(0, Math.min(500, Number(ms) || 0)); store.set("sp-kbd-delay", delay); },
    get isOpen() { return isOpen(); },
    open: () => setOpen(true),
    close: () => setOpen(false),
  };
}
