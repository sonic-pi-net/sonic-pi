// SPDX-License-Identifier: AGPL-3.0-or-later
// Presses made while what they play is still starting (app/src/ui/held-press.js).
import test from "node:test";
import assert from "node:assert/strict";
import { heldPress } from "../src/ui/held-press.js";

function rig() {
  let up = false, ready = null;
  const sent = [];
  const press = heldPress({ ready: () => up, start: () => new Promise((r) => (ready = () => { up = true; r(); })), send: (n) => sent.push(n) });
  return { press, sent, started: () => ready(), settle: () => new Promise((r) => setTimeout(r, 0)), stop: () => (up = false) };
}

test("presses made while it starts do not pile up: when it is ready the last one plays, alone", async () => {
  const r = rig();
  for (const n of [60, 62, 64, 65, 67, 69]) r.press(n);
  assert.deepEqual(r.sent, [], "nothing is sent to a program that is not there yet");
  r.started();
  await r.settle();
  assert.deepEqual(r.sent, [69], "one note, the last pressed: not six at once");
});

test("once it is up every press plays at once, and it is started again after it stops", async () => {
  const r = rig();
  r.press(60);
  r.started();
  await r.settle();
  r.press(62);
  r.press(64);
  assert.deepEqual(r.sent, [60, 62, 64]);
  r.stop();
  r.press(65);
  r.press(67);
  assert.deepEqual(r.sent, [60, 62, 64], "stopped: held again");
  r.started();
  await r.settle();
  assert.deepEqual(r.sent, [60, 62, 64, 67]);
});

test("a start that does not come up plays nothing", async () => {
  const sent = [];
  const press = heldPress({ ready: () => false, start: () => Promise.resolve(), send: (n) => sent.push(n) });
  press(60);
  await new Promise((r) => setTimeout(r, 0));
  assert.deepEqual(sent, []);
});
