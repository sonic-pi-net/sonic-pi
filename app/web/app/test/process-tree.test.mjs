// SPDX-License-Identifier: AGPL-3.0-or-later
// The process tree's layout is native Sonic Pi's node tree layout
// (nodetreegraph.cpp computeTargets): leaves in sequential slots across,
// parents centred over their children, depth down.
import { test } from "node:test";
import assert from "node:assert/strict";
import { layoutTargets, processLabel, KIND } from "../src/process-tree.js";

test("leaves take sequential slots and parents centre over them", () => {
  const t = layoutTargets([
    { id: 0, parent: null },
    { id: 1, parent: 0 },
    { id: 2, parent: 1 },
    { id: 3, parent: 1 },
    { id: 4, parent: 1 },
  ]);
  assert.deepEqual([t.get(2).tx, t.get(3).tx, t.get(4).tx], [0, 0.5, 1]);
  assert.equal(t.get(1).tx, 0.5);
  assert.equal(t.get(0).tx, 0.5);
  assert.deepEqual([t.get(0).ty, t.get(1).ty, t.get(2).ty], [0, 0.5, 1]);
});

test("siblings keep their given order: a thread spawned first sits left", () => {
  const t = layoutTargets([
    { id: 0, parent: null },
    { id: 5, parent: 0 },
    { id: 9, parent: 0 },
  ]);
  assert.ok(t.get(5).tx < t.get(9).tx);
});

test("an uneven tree centres each parent over its own children", () => {
  const t = layoutTargets([
    { id: 0, parent: null },
    { id: 1, parent: 0 },
    { id: 2, parent: 0 },
    { id: 3, parent: 1 },
    { id: 4, parent: 1 },
    { id: 5, parent: 2 },
  ]);
  // leaves 3, 4, 5 at 0, 0.5, 1; parent 1 over 3 and 4; parent 2 over 5
  assert.equal(t.get(1).tx, 0.25);
  assert.equal(t.get(2).tx, 1);
  assert.equal(t.get(0).tx, 0.625);
});

test("a lone node sits in the middle; an orphan becomes a root", () => {
  assert.equal(layoutTargets([{ id: 0, parent: null }]).get(0).tx, 0.5);
  const t = layoutTargets([{ id: 1, parent: 42 }, { id: 2, parent: 42 }]);
  assert.deepEqual([t.get(1).ty, t.get(2).ty], [0, 0]);
});

test("an fx and a sound are named as a program says them", () => {
  assert.equal(processLabel({ kind: KIND.fx, name: "sonic-pi-fx_reverb" }), "with_fx :reverb");
  assert.equal(processLabel({ kind: KIND.synth, name: "sonic-pi-beep" }), "synth :beep");
  assert.equal(processLabel({ kind: KIND.sample, name: "sonic-pi-basic_stereo_player", sample: "bd_haus.flac" }), "sample :bd_haus");
});

test("a live loop moved into an fx hangs from it, under the thread that moved it", () => {
  // run 1 → main 2 → with_fx 7 → loop 3; the loop's old parent was main 5 of an earlier run
  const t = layoutTargets([
    { id: 0, parent: null },
    { id: 1, parent: 0 },
    { id: 2, parent: 1 },
    { id: 7, parent: 2 },
    { id: 3, parent: 7 },
  ]);
  assert.deepEqual([t.get(2).ty, t.get(7).ty, t.get(3).ty], [0.5, 0.75, 1]);
});
