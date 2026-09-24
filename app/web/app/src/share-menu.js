// SPDX-License-Identifier: AGPL-3.0-or-later
// The Share button's panel: what to share, then how. What: the buffer showing, or the whole set (sets.js), the
// buffer by default. How: its link (share.js) copied to the clipboard, as a QR code to scan or save, or shown to
// copy by hand where the clipboard is out of reach; or saved as a file, a buffer as .txt and a set as .sonicpi,
// which native Sonic Pi opens too. A buffer's link is in the address bar as well. The panel stays open: what was
// done is said in place (the button a tick, and a line under it), and what each kind of thing is, under the choice.
//
// A QR code carries the link's digits form (share.js encodeDigits) in the code's numeric mode, a third more than
// the text link would hold, and stops at a size a phone reads off a screen (QR_MAX): past it, it says so and points
// at the link or the file, rather than show a code too dense to scan.
import { icon } from "./icons.js";
import { renderCode } from "./highlight.js";

const QR_MAX = 25;   // the largest QR version offered: 117×117 modules, still read off a laptop's screen at arm's length

/**
 * scopes: { buffer: {...}, set: {...} }, each { icon, kind (its tile's words, e.g. "This buffer"), summary (its name
 * and what it holds, e.g. "drums · 12 lines"), preview (a buffer's first lines), title (the words for it, e.g. "buffer 3"), link() (its text link), qrLink() (its link for
 * a QR code), save() (saves it as a file: its name, or null when put off), fileKind (".txt", ".sonicpi"), about (what
 * it is and what its file opens in), fields ({ name, nameMax, set(name,
 * description) }, and a set's description and descriptionMax: given in the panel above the actions; the name is its
 * file's) }.
 */
const ACTS = [
  ["copy", "copy", "Copy link", "Copy its link to the clipboard, ready to paste"],
  ["qr", "qrcode", "QR code", "Show a QR code, for a phone to scan or to save as an image"],
  ["show", "link", "Show link", "Show the link, to copy by hand"],
  ["file", "download", "Save file", (s) => `Save it as a ${s.fileKind} file`],
];

export function createShareMenu({ button, menu, scopes, clipboard, ready = null }) {
  // what the links need (share.js loadShareCodec), fetched as a pointer or the focus reaches the button: in long
  // before a tap lands, and the menu opens once it is (its link is written as it opens)
  for (const type of ["pointerover", "pointerdown", "focus"]) button.addEventListener(type, () => { ready?.()?.catch?.(() => {}); }, { passive: true });
  let scope = "buffer", body = null, view = null;
  // a phone's: the panel as About is there, a dialog over the page dimmed and blurred behind it (style.css); a tap on
  // the page behind closes it, as a click outside the menu does
  const backdrop = document.createElement("div");
  backdrop.className = "sm-backdrop";
  backdrop.hidden = true;
  menu.before(backdrop);
  const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };
  const glyph = (name) => { const s = el("span", "sm-glyph"); s.innerHTML = icon(name); return s; };

  function open() {
    scopes.buffer.link();   // the buffer's link in the address bar as the panel opens, to copy from there too
    render();
    const r = button.getBoundingClientRect();
    menu.style.setProperty("--picker-top", `${Math.round(r.bottom) + 6}px`);
    menu.style.setProperty("--menu-left", `${Math.round(Math.max(12, Math.min(r.left, window.innerWidth - 352)))}px`);
    menu.hidden = false;
    backdrop.hidden = false;
    button.setAttribute("aria-expanded", "true");
    menu.querySelector(".sm-what [aria-checked=true]").focus();
  }
  // what to share (the buffer or the set, two tabs), its panel (what it is, what it's called), then the four ways
  function render() {
    const s = scopes[scope];
    menu.innerHTML = "";
    // its head: the word, and a close as About's (shown on a phone, where the panel is a dialog)
    const top = el("div", "sm-top"), shut = el("button", "zoom-btn sm-close");
    shut.type = "button"; shut.title = "Close"; shut.setAttribute("aria-label", "Close");
    shut.innerHTML = '<svg viewBox="0 0 24 24" aria-hidden="true"><circle cx="12" cy="12" r="9"/><path d="M10 10l4 4m0 -4l-4 4"/></svg>';
    shut.addEventListener("click", close);
    top.append(el("div", "sm-step", "Share"), shut);
    menu.append(top);
    const what = el("div", "sm-what");
    what.setAttribute("role", "radiogroup");
    what.setAttribute("aria-label", "What to share");
    const keys = Object.keys(scopes);
    const choose = (key) => { if (scope === key) return; scope = key; render(); menu.querySelector(".sm-what [aria-checked=true]").focus(); };
    for (const key of keys) {
      const sc = scopes[key], b = el("button", "sm-what-opt");
      b.dataset.scope = key;
      b.type = "button";
      b.setAttribute("role", "radio");
      b.setAttribute("aria-checked", String(key === scope));
      b.tabIndex = key === scope ? 0 : -1;
      const words = el("span", "sm-what-words");
      words.append(el("span", "sm-what-name", sc.kind), el("span", "sm-what-sum", sc.summary));
      b.append(glyph(sc.icon), words);
      b.addEventListener("click", () => choose(key));
      b.addEventListener("keydown", (e) => {
        if (!["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown"].includes(e.key)) return;
        e.preventDefault();
        choose(keys[(keys.indexOf(scope) + (e.key === "ArrowLeft" || e.key === "ArrowUp" ? keys.length - 1 : 1)) % keys.length]);
      });
      what.append(b);
    }
    menu.append(what);
    // the chosen tab's panel, joined to it: what it is, what it's called, and a set's description or a buffer's
    // code. One height either way, so the actions under it stay where they are
    // a card as the quickstart's (ui/card.css), its bar the chosen tab: the body what it is and what it's called, the
    // foot the ways to share it
    const card = el("div", "sm-card" + (keys.indexOf(scope) === 0 ? " first" : " last"));
    const panel = el("div", "sm-panel");
    // the panel's body (what it is, its name, its code or description), and in its place, the same size, a QR code or
    // the link shown: in the card, never below it where a phone's screen would cut it off
    body = el("div", "sm-body");
    const about = el("p", "sm-about", s.about);
    about.setAttribute("aria-live", "polite");   // what was just done is said here, in the card, in the about's place (done)
    body.append(about);
    if (s.fields) body.append(fields(s.fields, what.querySelector("[aria-checked=true] .sm-what-sum"), s));
    view = el("div", "sm-view");
    view.hidden = true;
    view.setAttribute("aria-live", "polite");
    panel.append(body, view);
    card.append(panel);
    menu.append(card);
    const how = el("div", "sm-how");
    for (const [act, ic, label, tip] of ACTS) {
      const b = el("button", "sm-act");
      b.type = "button";
      b.dataset.act = act;
      b.title = typeof tip === "function" ? tip(s) : tip;
      b.setAttribute("aria-label", b.title);
      b.append(glyph(ic), el("span", "sm-act-label", label));
      b.addEventListener("click", () => act === "copy" ? copy() : act === "file" ? file() : show(act));
      how.append(b);
    }
    card.append(how);
  }
  // what it's called, and a set's description or a buffer's first lines, in a box of their own: each labelled, with
  // what it's for beside the label. Saved as they are typed, the tile's line following the name. The second slot
  // is the same height either way, so the actions under it stay put.
  function fields(f, tileSum, s) {
    const box = el("div", "sm-fields");
    const row = (label, hint, id) => { const r = el("div", "sm-field-head"), l = el("label", "sm-field-label", label); l.htmlFor = id; r.append(l); if (hint) r.append(el("span", "sm-field-hint", hint)); return r; };
    const name = el("input", "sm-field");
    name.value = f.name; name.maxLength = f.nameMax; name.id = "sm-name"; name.spellcheck = false;
    box.append(row("Name", "", name.id), name);
    let about = null;
    if (f.descriptionMax) {
      about = el("textarea", "sm-field sm-field-about");
      about.rows = 3; about.id = "sm-about";
      about.value = f.description; about.maxLength = f.descriptionMax;
      about.placeholder = "Describe this set...";
      box.append(row("Description", "optional", about.id), about);
    } else {
      // a buffer: its code, highlighted as the editor has it, from its first line of code (the comments heading it
      // skipped), scrolling for the rest: what is being shared, at a glance
      const pre = renderCode(s.preview, el("pre", "sm-field sm-preview"));
      pre.tabIndex = 0;
      pre.setAttribute("role", "region");
      pre.setAttribute("aria-label", "Preview of the buffer's code");
      const head = el("div", "sm-field-head");
      head.append(el("span", "sm-field-label", "Preview"));
      box.append(head, pre);
    }
    let timer = 0;
    const keep = () => { clearTimeout(timer); timer = setTimeout(() => { f.set(name.value, about?.value); tileSum.textContent = s.summary; }, 300); };
    name.addEventListener("input", keep);
    about?.addEventListener("input", keep);
    name.addEventListener("blur", () => { if (!name.value.trim()) name.value = f.name; });   // a name can't be emptied: it keeps the one it had
    return box;
  }
  // an action done: its button a tick with the word for it a moment, and what it did said under the row
  function done(act, word, said) {
    const b = menu.querySelector(`.sm-act[data-act=${act}]`);
    for (const r of menu.querySelectorAll(".sm-act")) r.setAttribute("aria-pressed", "false");
    if (b) {
      b.classList.add("done");
      b.querySelector(".sm-glyph").innerHTML = icon("check");
      b.querySelector(".sm-act-label").textContent = word;
      clearTimeout(b.doneTimer);
      b.doneTimer = setTimeout(() => { if (!b.isConnected) return; const [, ic, label] = ACTS.find((a) => a[0] === act); b.classList.remove("done"); b.querySelector(".sm-glyph").innerHTML = icon(ic); b.querySelector(".sm-act-label").textContent = label; }, 2500);
    }
    view.hidden = true; body.hidden = false;   // the body back, what was done said in the card's own line
    const about = body.querySelector(".sm-about");
    about.textContent = said;
    about.classList.add("said");
  }
  function close() {
    if (menu.hidden) return;
    menu.hidden = true;
    backdrop.hidden = true;
    button.setAttribute("aria-expanded", "false");
    button.focus();
  }
  async function copy() {
    const s = scopes[scope], url = s.link();
    try { await clipboard(url); } catch { show("show"); return; }   // no clipboard to be had: the link, to copy by hand
    done("copy", "Copied", "Link copied");
  }
  async function file() {
    const s = scopes[scope], name = await s.save();
    if (name) done("file", "Saved", `Saved ${name}`);
  }
  // a QR code or the link, shown in the panel in place of its body; the same one pressed again puts the body back
  function show(act) {
    qrTurn++;   // a QR on its way is for the view it was asked in only
    const s = scopes[scope];
    const again = menu.querySelector(`.sm-act[data-act=${act}]`)?.getAttribute("aria-pressed") === "true";
    for (const r of menu.querySelectorAll(".sm-act")) r.setAttribute("aria-pressed", String(!again && r.dataset.act === act));
    if (again) { view.hidden = true; body.hidden = false; return; }
    const room = body.hidden ? view.offsetHeight : body.offsetHeight;   // the body's own height: the view takes just that
    view.style.height = `${room}px`;
    view.innerHTML = "";
    body.hidden = true;
    view.hidden = false;
    const detailOf = view;
    if (act === "show") {
      const url = s.link();
      const field = el("textarea", "sm-url");
      field.readOnly = true;
      field.value = url;
      field.rows = 3;
      field.setAttribute("aria-label", "The link");
      field.addEventListener("focus", () => field.select());
      detailOf.append(field, el("div", "sm-note", `${url.length} characters${scope === "buffer" ? " · the address bar has it too" : ""}`));
      field.focus();
      return;
    }
    const url = s.qrLink();
    // the QR library, fetched the first time a code is asked for; a view changed before it arrives is left alone
    const turn = ++qrTurn;
    loadQR().then(() => { if (turn === qrTurn && !view.hidden) showQR(url, s, room); });
  }
  let qrTurn = 0;
  function showQR(url, s, room) {
    const detailOf = view;
    const qr = makeQR(url);
    if (!qr) {
      detailOf.append(el("div", "sm-note", `${scope === "set" ? "this set is" : "this program is"} too big for a QR code a phone can read: copy the link or save it as a file instead`));
      return;
    }
    const canvas = drawQR(qr);
    const save = el("button", "sm-save", "Save the image");
    save.addEventListener("click", () => canvas.toBlob((blob) => {
      const a = document.createElement("a");
      a.href = URL.createObjectURL(blob);
      a.download = `sonic-pi-${s.title.replace(/[^\w-]+/g, "-")}.png`;
      a.click();
      setTimeout(() => URL.revokeObjectURL(a.href), 1000);
    }));
    // as large as the panel's room allows, beside its note and its button
    canvas.style.setProperty("--qr-size", `${Math.max(120, Math.min(240, room - 72))}px`);
    detailOf.append(canvas, el("div", "sm-note", `QR version ${qr.typeNumber}, ${qr.getModuleCount()}×${qr.getModuleCount()}`), save);
  }

  button.addEventListener("click", () => {
    if (!menu.hidden) return close();
    Promise.resolve(ready?.()).then(open, (e) => console.error("the share links could not load:", e));
  });
  document.addEventListener("pointerdown", (e) => { if (!menu.hidden && !menu.contains(e.target) && !button.contains(e.target)) close(); }, true);
  document.addEventListener("keydown", (e) => { if ((e.key === "Escape" || (e.key === "g" && e.ctrlKey)) && !menu.hidden) { e.stopPropagation(); e.preventDefault(); close(); } }, true);   // Escape, or Emacs's Ctrl-G
  return { open, close };
}

let qrcode = null;
const loadQR = () => (qrcode ? Promise.resolve() : import("qrcode-generator").then((m) => { qrcode = m.default ?? m; }));

// A QR of a link: the address up to the program in the byte mode, the program's digits (share.js encodeDigits,
// after "#code=N<format>") in the numeric mode. Medium error correction while it fits (a phone reads a marked
// screen or a creased page), low when only that will hold it; none past QR_MAX.
export function makeQR(url) {
  const m = /^(.*#code=N.)(\d+)$/.exec(url);
  for (const level of ["M", "L"]) {
    try {
      const qr = qrcode(0, level);
      if (m) { qr.addData(m[1], "Byte"); qr.addData(m[2], "Numeric"); } else qr.addData(url, "Byte");
      qr.make();
      qr.typeNumber ??= Math.round((qr.getModuleCount() - 17) / 4);
      if (qr.typeNumber <= QR_MAX) return qr;
    } catch { /* too long at this level */ }
  }
  return null;
}
function drawQR(qr) {
  const n = qr.getModuleCount(), quiet = 4, size = 240;
  const scale = Math.max(1, Math.floor((size * devicePixelRatio) / (n + quiet * 2)));
  const px = scale * (n + quiet * 2);
  const canvas = document.createElement("canvas");
  canvas.className = "sm-qr";
  canvas.width = canvas.height = px;
  canvas.setAttribute("role", "img");
  canvas.setAttribute("aria-label", "QR code of the link");
  const ctx = canvas.getContext("2d");
  ctx.fillStyle = "#fff";
  ctx.fillRect(0, 0, px, px);
  ctx.fillStyle = "#000";
  for (let r = 0; r < n; r++) for (let c = 0; c < n; c++) if (qr.isDark(r, c)) ctx.fillRect((c + quiet) * scale, (r + quiet) * scale, scale, scale);
  return canvas;
}
