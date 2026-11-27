// SPDX-License-Identifier: AGPL-3.0-or-later
// The plugin's resize grip: a corner of the page to drag, for a host that gives a plugin's window no handles of its
// own (Ableton Live). The plugin resizes itself and the host's window follows: an Audio Unit sizes its own view, a
// VST3 asks its host for the size (app/plugin: au/shell.mm, vst3/shell.cpp).
//
// The page does not work the size out. How far the pointer has gone is said in screen units, which stay put while
// the page under the pointer changes size and are the same whatever the page's zoom; the plugin adds that to the
// size the drag began at, and keeps the result within what it allows.
//
// A screen gives the pointer's place in fractions of a unit; what is said is whole.
//
//   page → plugin   { t: "resizeStart" }           a drag begins: note the size now
//                   { t: "resizeBy", dx, dy }      the pointer is this far from where it began

/**
 * @param plugin     window.__sonicPiPlugin: post(message)
 * @param document   where the grip goes (its body)
 * @param nextFrame  requestAnimationFrame: a resize is asked for at most once a frame
 */
export function createResizeGrip({ plugin, document, nextFrame = (fn) => requestAnimationFrame(fn) }) {
  const grip = document.createElement("div");
  grip.setAttribute("id", "plugin-grip");
  grip.setAttribute("role", "separator");
  grip.setAttribute("aria-label", "Drag to change the plugin's size");
  grip.setAttribute("title", "Drag to resize");
  let from = null, at = null, waiting = false;
  const say = () => { waiting = false; if (from && at) plugin.post({ t: "resizeBy", dx: Math.round(at.x - from.x), dy: Math.round(at.y - from.y) }); };
  const end = (e) => { if (!from) return; from = at = null; grip.releasePointerCapture?.(e.pointerId); grip.classList.remove("dragging"); };
  grip.addEventListener("pointerdown", (e) => {
    if (e.button !== 0) return;
    e.preventDefault();
    from = { x: e.screenX, y: e.screenY };
    at = null;
    grip.setPointerCapture?.(e.pointerId);
    grip.classList.add("dragging");
    plugin.post({ t: "resizeStart" });
  });
  grip.addEventListener("pointermove", (e) => {
    if (!from) return;
    at = { x: e.screenX, y: e.screenY };
    if (!waiting) { waiting = true; nextFrame(say); }
  });
  grip.addEventListener("pointerup", end);
  grip.addEventListener("pointercancel", end);
  document.body.appendChild(grip);
  return grip;
}
