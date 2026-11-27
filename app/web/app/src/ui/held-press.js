// SPDX-License-Identifier: AGPL-3.0-or-later
// Presses made while what they play is still starting (the engine coming up, a program reaching the point where it
// listens). They are held, not queued: each takes the place of the last, and when the start is over the last one
// plays, alone. Queued, every press made while waiting would sound in the same instant, as loud as all of them
// together.

/**
 * @param ready  () => bool: up, so a press plays at once
 * @param start  () => Promise: bring it up; settled when it is up, or is not going to be
 * @param send   (what) => void: play a press
 * @returns press(what)
 */
export function heldPress({ ready, start, send }) {
  let starting = null, held = null;
  return (what) => {
    if (!starting && ready()) return send(what);
    held = { what };
    starting ??= new Promise((r) => r(start())).catch(() => {}).then(() => {
      const last = held;
      starting = held = null;
      if (last && ready()) send(last.what);
    });
  };
}
