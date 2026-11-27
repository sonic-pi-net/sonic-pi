// SPDX-License-Identifier: AGPL-3.0-or-later
// The site's live cards (info.js): each example's code, in the page as a <pre>,
// made a card — the code editable (and Reset), ▶ playing it through the app's
// engine on a scope slot of its own, and "Open in Sonic Pi" showing the editor
// with that code, the sound carrying on since the engine is one.
import { KEPT_KEYS as THEME_KEYS, onChange as onThemeChange } from "./theme.js";
import { heldPress } from "./ui/held-press.js";
import { createCard } from "./ui/card.js";
import { createDeck } from "./ui/deck.js";
import { piano, fitKeys, QWERTY_KEYS } from "./ui/piano.js";

const STORE = "sp-site-card:";   // a card's edit, kept until Reset
const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; };

/**
 * A page's examples as cards, each in its <pre>'s place: the Examples page's grid and the hero's one (the page has
 * them as <pre class="sp-card" data-key data-title data-blurb data-wide><code>, scripts/build-site.mjs). A card
 * keeps its block's id, so a link to it (#example-haunted) still lands on it.
 * @param pres   the <pre> elements, in page order
 * @param hooks  { play(code, {scopeSlot}) → job|null, stop(job), scopeFrame(slot, n), open(code, job) — the editor with this code (job: the card's run, if playing) }
 * @param host   the scrolling element they live in (Escape there stops a playing card)
 * @param root   the document or shadow root the cards live in
 */
// a block's code: a card the build wrote (scripts/build-site.mjs, ui/card-html.js) a line to a .cm-line, or a <pre>'s <code>
const codeOf = (block) => (block.matches("pre") ? (block.querySelector("code") ?? block).textContent : [...block.querySelectorAll(".cm-line")].map((l) => l.textContent).join("\n"));
// a block's heading level: the build's card's (a level under the heading it sits under), a <pre>'s the card's own
const levelOf = (block) => Number(block.querySelector(".qs-card-title")?.tagName.slice(1)) || 2;

// How tall a card may be: the part of the page on show (its scroller's height, less a little air), which card.css
// reads as --card-most. Kept as the window changes size.
const fitted = new WeakSet();
function fitCardsTo(scroller) {
  const box = scroller instanceof Element ? scroller : document.scrollingElement;
  if (!box || fitted.has(box)) return;
  fitted.add(box);
  const fit = () => { if (box.clientHeight) document.documentElement.style.setProperty("--card-most", `${Math.max(240, box.clientHeight - 24)}px`); };
  fit();
  new ResizeObserver(fit).observe(box);
}

// A card's Open in Sonic Pi, which shows the editor in the page's place: not for a card of an article its reader is to
// stay on (data-stay: the plugin's sales page, where leaving is a sale lost).
const opens = (at, hooks) => (at.closest?.("[data-stay]") ? undefined : hooks.open);

export function mountExamples(pres, hooks, host, root = document) {
  fitCardsTo(host);
  const deck = createDeck(hooks, host);
  for (const pre of pres) {
    const d = pre.dataset;
    const card = deck.add(createCard({ title: d.title, code: codeOf(pre), blurb: d.blurb ?? "", key: d.key, actions: ["edit", "reset", "copy"], open: opens(pre, hooks), hooks, remember: STORE, root, wide: "wide" in d, level: levelOf(pre) }));
    if (pre.id) card.el.id = pre.id;
    pre.replaceWith(card.el);
    playIfPressed(card);
  }
  return withReveal(deck);
}

/**
 * A page's code blocks as cards, each in the block's place (Learn's teaser). The block's highlighted code stays as
 * the card's body until it is clicked into. A card is named for the heading above it, numbered within its section.
 * @param pres   the <pre> elements, in page order
 */
export function mountSnippets(pres, hooks, host, root) {
  fitCardsTo(host);
  const deck = createDeck(hooks, host);
  const counts = new Map();
  pres.forEach((pre, i) => {
    let h = pre.previousElementSibling;
    while (h && !/^H[1-6]$/.test(h.tagName)) h = h.previousElementSibling;
    const section = pre.dataset.title ?? h?.textContent.trim().replace(/\s+/g, " ") ?? "Example";   // a block may name itself (a page's teaser)
    const n = (counts.get(section) ?? 0) + 1; counts.set(section, n);
    pre.querySelectorAll(".copy-button").forEach((b) => b.remove());   // the page's own copy button: the card has one
    const open = opens(pre, hooks);   // asked while the block is still in its article
    const slot = el("div");   // the card takes the block into its body, so the block's place is held first
    pre.replaceWith(slot);
    const card = deck.add(createCard({ title: n > 1 ? `${section} · ${n}` : section, code: codeOf(pre).replace(/^\n/, "").replace(/\n$/, ""), key: pre.dataset.key ?? `snippet-${i}`, hooks, sizes: true, actions: ["edit", "reset", "copy"], open, root, playable: !("still" in pre.dataset), level: levelOf(pre) }));   // a fragment (data-still) to read, not a program to play   // a <pre><code> keeps a leading newline the page never shows
    slot.replaceWith(card.el);
    playIfPressed(card);
  });
  return withReveal(deck);
}

/**
 * Several programs in one card, one chosen at a time from a row over it (the plugin page's). The host's code blocks
 * (<pre class="sp-card" data-title>, or the cards the build made of them) are the programs, each followed by a
 * <p class="sp-ways-about"> of words shown under the row while it is the one chosen. Chosen while the card plays, a
 * program plays in the other's place.
 * @param panel  an element for the card, between its bar and its code (a keyboard)
 * @returns { deck, card, playing() }
 */
export function mountWays(host, hooks, scroller, root = document, panel = null) {
  const ways = [...host.querySelectorAll(".sp-card")].map((block) => ({
    title: block.dataset.title ?? block.querySelector(".qs-card-title")?.textContent ?? "",
    code: codeOf(block).replace(/^\n/, "").replace(/\n$/, ""),
    about: block.nextElementSibling?.matches(".sp-ways-about") ? block.nextElementSibling.innerHTML : "",
  }));
  fitCardsTo(scroller);
  const deck = createDeck(hooks, scroller);
  if (!ways.length) return { deck: withReveal(deck), card: null, playing: () => false };
  const card = deck.add(createCard({ title: host.dataset.title ?? ways[0].title, code: ways[0].code, key: host.dataset.key ?? "ways", hooks, sizes: true, actions: ["edit", "reset", "copy"], open: opens(host, hooks), root, wide: true, panel, level: Number(host.closest("article")?.querySelector(".sec-title")?.tagName.slice(1) || 1) + 1 }));
  const playing = () => card.el.classList.contains("playing");

  const row = el("div", "sp-ways-row");
  row.setAttribute("role", "group");
  row.setAttribute("aria-label", host.dataset.label ?? "Which program");
  const about = el("p", "sp-ways-about");
  about.setAttribute("aria-live", "polite");
  about.innerHTML = ways[0].about;
  const buttons = ways.map((w, i) => {
    const b = el("button", "sp-way", w.title);
    b.type = "button";
    b.setAttribute("aria-pressed", String(i === 0));
    b.addEventListener("click", () => {
      buttons.forEach((x, k) => x.setAttribute("aria-pressed", String(k === i)));
      about.innerHTML = w.about;
      const was = playing();
      if (was) card.onStop?.();
      card.setCode(w.code);
      if (was) card.onRun?.();
    });
    row.appendChild(b);
    return b;
  });
  host.replaceChildren(row, about, card.el);
  host.classList.add("live");
  return { deck: withReveal(deck), card, playing };
}

/**
 * A keyboard that plays a program as a MIDI keyboard plays one in the plugin (the plugin page's): each key pressed
 * reaches the running code as the cue a note from the DAW is, "/midi:daw:1/note_on" with its note and a velocity, so
 * the code under it is code for the plugin as it stands. The host's programs are ways of answering the keys
 * (mountWays). A key pressed while the card is not playing plays it first; the computer's own keys play too, as they
 * play the instrument page's keyboard, while the focus is here and not in the code.
 * @param hooks  the examples' (mountExamples), and cue(address, args): a cue into the running program; waiting(job):
 *               whether a thread of that run waits on a sync
 */
const MIDI_NOTE_ON = "/midi:daw:1/note_on", MIDI_VELOCITY = 100;
export function mountMidiKeys(host, hooks, scroller, root = document) {
  const keysBox = el("div", "midi-keys-piano"), keysRoom = el("div", "midi-keys-room");   // the room pads, the box in it is measured
  keysRoom.appendChild(keysBox);
  const { deck, card, playing } = mountWays(host, hooks, scroller, root, keysRoom);
  if (!card) return deck;

  // the card playing, and a thread of its run waiting on a sync, before a key's cue is sent: a cue with no program
  // waiting on it is heard by nobody. The run starts when the engine is up and its synths are in, which on a first
  // press takes a second or more; a program that never waits (an edit took its sync out) is given up on.
  // Presses made meanwhile are held, the last one playing when it is up (ui/held-press.js): never all of them at once.
  const WAIT_MOST = 6000;
  // While it starts the card says so, where it says what is wrong with a program: keys pressed into silence look broken.
  const STARTING = "Starting…", said = card.el.querySelector(".qs-state");
  const start = () => {
    const run = card.onRun?.();   // the run clears what the card last said: the word goes up after it
    if (said && !said.textContent) said.textContent = STARTING;
    return Promise.resolve(run).then(async () => {
      if (!hooks.waiting) return new Promise((r) => setTimeout(r, 200));
      for (const from = performance.now(); performance.now() - from < WAIT_MOST && playing() && !hooks.waiting(card.job);) await new Promise((r) => setTimeout(r, 25));
    }).finally(() => { if (said?.textContent === STARTING) said.textContent = ""; });
  };
  const press = heldPress({ ready: playing, start, send: (note) => hooks.cue?.(MIDI_NOTE_ON, [note, MIDI_VELOCITY]) });

  // the instrument page's keyboard (docs.js): the same keys in the same room, its letters on them, z and x its octaves
  let base = 48, drawnFor = 0;
  const draw = () => {
    drawnFor = keysBox.clientWidth;
    const { octaves, keyWidth } = fitKeys(drawnFor || 600);
    const labels = {};
    [...QWERTY_KEYS].forEach((k, i) => (labels[base + i] = k));
    keysBox.replaceChildren(piano({ from: base, to: base + octaves * 12, labels, onKey: press, keyWidth }));
  };
  draw();
  new ResizeObserver(() => { if (keysBox.clientWidth && Math.abs(keysBox.clientWidth - drawnFor) > 4) draw(); }).observe(keysBox);
  const shiftOctave = (by) => { base = Math.min(96, Math.max(24, base + by)); draw(); };

  host.tabIndex = -1;
  // a press anywhere on it but in the code brings the computer's keys here (the piano's keys take no focus of their own)
  host.addEventListener("pointerdown", (e) => { if (!e.target.closest?.(".cm-editor")) host.focus({ preventScroll: true }); });
  host.addEventListener("keydown", (e) => {
    if (e.metaKey || e.ctrlKey || e.altKey || e.target.closest?.(".cm-editor, input, textarea")) return;
    const key = e.key.toLowerCase(), i = QWERTY_KEYS.indexOf(key);
    if (i >= 0) {
      e.preventDefault();
      if (e.repeat) return;   // held: one note, not the keyboard's repeats
      const k = keysBox.querySelector(`.sp-key[data-note="${base + i}"]`);
      if (k) { k.classList.add("down"); setTimeout(() => k.classList.remove("down"), 150); }
      press(base + i);
    } else if (key === "z") shiftOctave(-12);
    else if (key === "x") shiftOctave(12);
  });
  return deck;
}

/**
 * A showcase: the app in a frame, playing a piece to be looked at (showcase.js has the frame's side). The host holds
 * the piece (<script type="application/json" class="showcase-code">: { code }) and a button, .showcase-sound, over
 * the frame; beside the host, in its figure, buttons with data-view say what the frame's panel shows.
 * Silent as it opens. The button starts the piece again out loud, and again silently: a browser lets sound start
 * only in a press, so the audio context is made here, in the press, and handed to a frame loaded anew. Out loud it
 * gives way to the page's own sound, and stops when it is no longer on show.
 */
export function mountShowcase(host, root = document) {
  let code = "";
  try { code = JSON.parse(host.querySelector("script.showcase-code")?.textContent ?? "{}").code ?? ""; } catch {}
  if (!code) return;
  const scope = host.closest("figure") ?? host.parentElement;
  const views = [...scope.querySelectorAll("[data-view]")];
  const button = host.querySelector(".showcase-sound"), label = button?.querySelector("span") ?? button;
  let view = host.dataset.view ?? "processes", audio = null, frame = null;

  // out loud, the page is told (main.js): its bar's stop shows, moves with the frame's sound (the frame's engine's
  // scope of what it sent out, read as the page reads its own), and stops it
  let told = false;
  const tell = () => {
    if (!!audio === told) return;
    told = !!audio;
    root.dispatchEvent(new CustomEvent("sp-showcase-sound", { detail: { on: told, read: () => { try { return frame?.contentWindow?.sonicPi?.engine?.getDeviceScope?.(0, 1024) ?? null; } catch { return null; } } } }));
  };
  root.addEventListener("sp-stop", () => sound(false));
  const paint = () => {
    for (const b of views) b.setAttribute("aria-pressed", String(b.dataset.view === view));
    if (button) { button.setAttribute("aria-pressed", String(!!audio)); if (label?.dataset.on) label.textContent = audio ? label.dataset.on : label.dataset.off; }
    host.classList.toggle("sounding", !!audio);
    tell();
  };
  function load() {
    frame?.remove();
    frame = el("iframe");
    frame.title = host.dataset.title ?? "";
    frame.tabIndex = -1;
    // the log's and cues' text at its smallest beside the code, and no metronome strip (the room goes to the log and
    // the cues); and this page's colours, so the frame opens in them (a change made here afterwards it is told: below)
    const storage = { "sp-log-smaller": "8", "sp-metro-shown": "false" };
    for (const key of THEME_KEYS) { try { const kept = localStorage.getItem(key); if (kept != null) storage[key] = kept; } catch {} }
    frame.showcase = { code, view, audio, line: Number(host.dataset.line) || 1, plugin: host.dataset.plugin, storage, follows: THEME_KEYS };
    frame.src = host.dataset.src ?? "code.html";
    host.prepend(frame);
    paint();
  }
  // this page's colours changed: the frame takes them up
  onThemeChange(() => { for (const key of THEME_KEYS) { try { frame?.contentWindow?.spShowcase?.keep?.(key, localStorage.getItem(key)); } catch {} } });
  // The loud frame's sound goes out through a level of this page's own, which the frame's engine takes for the
  // context's output (it connects to context.destination): so the sound can be faded to nothing before its context is
  // closed. A context closed while it sounds leaves its last moment in the browser's output, and WebKit plays that
  // when the next context starts: a snatch of the old sound at every Sound on.
  const FADE = 0.04, levels = new WeakMap();
  function quieten(context) {
    const level = levels.get(context);
    if (level) { level.gain.cancelScheduledValues(context.currentTime); level.gain.setValueAtTime(level.gain.value, context.currentTime); level.gain.linearRampToValueAtTime(0, context.currentTime + FADE); }
    setTimeout(() => context.close().catch(() => {}), 300);   // closed once silence has gone all the way out
  }
  function sound(on) {
    if (on === !!audio) return;
    if (audio) {
      quieten(audio);
      audio = null;
      paint();
      return void setTimeout(load, FADE * 1000 + 20);   // the frame goes once its sound has faded
    }
    // made and started here, in the press; the frame is loaded once it runs. A context is not running the moment it
    // is resumed (WebKit takes a while), and a frame handed one that is not yet running played on in silence.
    const made = (audio = new AudioContext({ latencyHint: "interactive", sampleRate: 48000 }));
    const out = made.destination, level = made.createGain();
    level.connect(out);
    levels.set(made, level);
    Object.defineProperty(made, "destination", { configurable: true, get: () => level });
    paint();
    made.resume().catch(() => {}).then(() => { if (audio === made) load(); });
  }
  // the page going (a reload, a link): nothing left sounding in the output for whatever starts next
  addEventListener("pagehide", () => { const level = audio && levels.get(audio); if (level) level.gain.value = 0; });
  for (const b of views) b.addEventListener("click", () => {
    view = b.dataset.view;
    frame.showcase.view = view;   // and where a frame loaded anew begins
    frame.contentWindow?.spShowcase?.show(view);
    paint();
  });
  button?.addEventListener("click", () => sound(!audio));
  // out loud, it gives way: to the page's own sound (its synth, its cards), and when it is no longer on show
  new MutationObserver(() => { if (audio && root.body.classList.contains("sounding")) sound(false); }).observe(root.body, { attributes: true, attributeFilter: ["class"] });
  setInterval(() => { if (audio && !host.offsetParent) sound(false); }, 500);
  load();
}

// a card whose Play was pressed before the script had made it (the page's own few lines, scripts/build-site.mjs): played
// now, as the press asked
function playIfPressed(card) {
  if (!window.spPendingPlay || window.spPendingPlay !== card.key) return;
  window.spPendingPlay = null;
  queueMicrotask(() => card.onRun?.());
}

// the deck's api as the pages know it, with the card element for a key
const withReveal = (deck) => Object.assign(deck, { reveal: (key) => deck.find(key)?.el ?? null });
