// SPDX-License-Identifier: AGPL-3.0-or-later
// The plugin's resize grip (app/src/plugin-grip.js): a drag from the corner asks the plugin for a size.
import test from "node:test";
import assert from "node:assert/strict";
import { createResizeGrip } from "../src/plugin-grip.js";

function fakes() {
  const listeners = {};
  const grip = { attrs: {}, classes: new Set(), captured: null, setAttribute(k, v) { this.attrs[k] = v; }, addEventListener: (type, fn) => (listeners[type] = fn),
    setPointerCapture(id) { this.captured = id; }, releasePointerCapture() { this.captured = null; }, classList: null };
  grip.classList = { add: (c) => grip.classes.add(c), remove: (c) => grip.classes.delete(c) };
  const document = { createElement: () => grip, body: { appendChild(e) { this.child = e; } } };
  const posted = [], frames = [];
  const plugin = { post: (m) => posted.push(m) };
  const fire = (type, e = {}) => listeners[type]?.({ pointerId: 1, button: 0, preventDefault() {}, ...e });
  const frame = () => { while (frames.length) frames.shift()(); };
  return { grip, document, plugin, posted, fire, frame, nextFrame: (fn) => frames.push(fn) };
}

test("the grip is a control in the page's corner, named for what it does", () => {
  const f = fakes();
  createResizeGrip({ plugin: f.plugin, document: f.document, nextFrame: f.nextFrame });
  assert.equal(f.document.body.child, f.grip);
  assert.equal(f.grip.attrs.role, "separator");
  assert.match(f.grip.attrs["aria-label"], /size/i);
});

test("a drag says where it began, then how far it has gone, once a frame, and stops at the release", () => {
  const f = fakes();
  createResizeGrip({ plugin: f.plugin, document: f.document, nextFrame: f.nextFrame });
  f.fire("pointerdown", { screenX: 1000, screenY: 700 });
  assert.deepEqual(f.posted, [{ t: "resizeStart" }], "the plugin notes the size the drag starts from");
  assert.equal(f.grip.captured, 1, "the pointer is the grip's until it is let go, wherever it goes");
  f.fire("pointermove", { screenX: 1040, screenY: 720 });
  f.fire("pointermove", { screenX: 1100, screenY: 760 });
  f.fire("pointermove", { screenX: 1150, screenY: 790 });
  assert.equal(f.posted.length, 1, "nothing more until the frame");
  f.frame();
  assert.deepEqual(f.posted[1], { t: "resizeBy", dx: 150, dy: 90 }, "one message a frame, with where the pointer has got to");
  f.fire("pointermove", { screenX: 900, screenY: 600 });
  f.frame();
  assert.deepEqual(f.posted[2], { t: "resizeBy", dx: -100, dy: -100 }, "smaller as well as larger");
  f.fire("pointerup", { screenX: 900, screenY: 600 });
  assert.equal(f.grip.captured, null);
  f.fire("pointermove", { screenX: 2000, screenY: 2000 });
  f.frame();
  assert.equal(f.posted.length, 3, "let go: the pointer moves nothing");
});

test("only the main button drags, and a drag that is cancelled ends", () => {
  const f = fakes();
  createResizeGrip({ plugin: f.plugin, document: f.document, nextFrame: f.nextFrame });
  f.fire("pointerdown", { button: 2, screenX: 0, screenY: 0 });
  assert.equal(f.posted.length, 0);
  f.fire("pointerdown", { screenX: 0, screenY: 0 });
  f.fire("pointercancel");
  f.fire("pointermove", { screenX: 50, screenY: 50 });
  f.frame();
  assert.deepEqual(f.posted, [{ t: "resizeStart" }]);
});

test("how far the pointer has gone is said in whole units, whatever fractions the screen gives its position in", () => {
  const f = fakes();
  createResizeGrip({ plugin: f.plugin, document: f.document, nextFrame: f.nextFrame });
  f.fire("pointerdown", { screenX: 1000.5, screenY: 700.25 });
  f.fire("pointermove", { screenX: 1040.75, screenY: 719.5 });
  f.frame();
  assert.deepEqual(f.posted[1], { t: "resizeBy", dx: 40, dy: 19 }, "the plugin reads whole numbers: a fraction would be read as no move at all");
});
