// SPDX-License-Identifier: AGPL-3.0-or-later
// The one piano keyboard: the completion's (a note, a chord or scale lit on it, a synth or an FX to play), the note
// list's under its rows, and the synth and FX pages' instrument. A key pressed plays; held down, the pointer swept
// across the keys plays each one it passes, as a glissando does — a finger's as well as a mouse's.

const NOTE_NAMES = ["C", "C♯", "D", "E♭", "E", "F", "F♯", "G", "A♭", "A", "B♭", "B"];
export const BLACK = new Set([1, 3, 6, 8, 10]);
export const noteName = (n) => `${NOTE_NAMES[((n % 12) + 12) % 12]}${Math.floor(n / 12) - 1}`;

const el = (tag, cls, text) => {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text != null) e.textContent = text;
  return e;
};

/** A piano from `from` to `to`; `lit` notes lit, `root` marked; a key pressed, or swept over held down, plays
 * (onKey). A wider `keyWidth` draws the instrument page's keyboard. */
export function piano({ from, to, lit = new Set(), root = null, onKey, labels = null, keyWidth = 14 }) {
  const W = keyWidth, B = Math.round(keyWidth * 0.64);
  const large = keyWidth !== 14;
  const keys = el("div", `sp-piano${large ? " sp-piano-large" : ""}`);
  let white = 0;
  for (let n = from; n <= to; n++) {
    const black = BLACK.has(((n % 12) + 12) % 12);
    const k = el("button", `sp-key ${black ? "sp-key-black" : "sp-key-white"}${lit.has(n) ? " lit" : ""}${n === root ? " root" : ""}`);
    k.type = "button";
    k.tabIndex = -1;   // played, not focused: the editor keeps its caret, the instrument page its QWERTY keys
    k.dataset.note = n;
    k.title = `${noteName(n)} (${n})`;
    if (labels?.[n]) k.appendChild(el("span", "sp-key-label", labels[n]));
    if (large) k.style.width = `${black ? B : W}px`;
    if (black) {
      k.style.left = `${white * W - B / 2}px`;
    } else {
      k.style.left = `${white * W}px`;
      white++;
    }
    keys.appendChild(k);
  }
  keys.style.width = `${white * W}px`;
  sweep(keys, (n) => onKey?.(n));
  return keys;
}

// Pressed, and swept held down: each key the pointer comes onto plays once, until it is let go. The key is found
// under the pointer (not by the key's own events), so a touch — which the browser keeps on the key it began on — sweeps
// as a mouse does.
function sweep(keys, play) {
  let held = null, last = null;
  const keyAt = (x, y) => {
    const k = document.elementFromPoint(x, y)?.closest?.(".sp-key");
    return k && keys.contains(k) ? k : null;
  };
  const hit = (k) => {
    if (!k || k === last) return;
    last = k;
    k.classList.add("down");
    setTimeout(() => k.classList.remove("down"), 150);
    play(Number(k.dataset.note));
  };
  const move = (e) => { if (e.pointerId === held) hit(keyAt(e.clientX, e.clientY)); };
  const end = (e) => {
    if (e.pointerId !== held) return;
    held = last = null;
    window.removeEventListener("pointermove", move);
    window.removeEventListener("pointerup", end);
    window.removeEventListener("pointercancel", end);
  };
  keys.addEventListener("pointerdown", (e) => {
    const k = e.target.closest?.(".sp-key");
    if (!k || e.button > 0 || held != null) return;
    e.preventDefault();
    k.releasePointerCapture?.(e.pointerId);   // a touch is captured by the key it began on: let it go, so the others hear it
    held = e.pointerId;
    last = null;
    window.addEventListener("pointermove", move);
    window.addEventListener("pointerup", end);
    window.addEventListener("pointercancel", end);
    hit(k);
  });
  keys.addEventListener("mousedown", (e) => e.preventDefault());   // no focus taken: the editor keeps its caret
}
