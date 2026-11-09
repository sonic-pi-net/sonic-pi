// SPDX-License-Identifier: AGPL-3.0-or-later
// The Debug pane's OSC logs (logs.js oscText): a message as SuperSonic writes one, and a timed bundle marked with how
// far ahead of the engine's now it was when it arrived — "+0.045s" ahead, "-0.010s" behind, never "+-".
import { test } from "node:test";
import assert from "node:assert/strict";
import { oscText } from "../src/logs.js";

const bundle = (timeTag, ...packets) => ({ timeTag, packets });

test("a bundle ahead of now is marked +", () => {
  assert.equal(oscText(bundle(100.045, ["/s_new", "sonic-pi-beep", 1001]), 100), '+0.045s /s_new "sonic-pi-beep", 1001');
});

test("a bundle behind now is marked - once, never +-", () => {
  const line = oscText(bundle(99.99, ["/n_free", 1001]), 100);
  assert.equal(line, "-0.010s /n_free 1001");
  assert.ok(!line.includes("+-"));
});

test("a hair either side of now reads +0.000s, not -0.000s", () => {
  assert.equal(oscText(bundle(100 - 0.0001, ["/n_free", 1]), 100), "+0.000s /n_free 1");
  assert.equal(oscText(bundle(100 + 0.0001, ["/n_free", 1]), 100), "+0.000s /n_free 1");
});

test("a bundle's other messages line up under the first", () => {
  assert.equal(oscText(bundle(100.5, ["/n_free", 1], ["/n_free", 2]), 100), "+0.500s /n_free 1\n        /n_free 2");
});

test("a message, an immediate bundle, or no time to measure from carries no mark", () => {
  assert.equal(oscText(["/notify", 1]), "/notify 1");
  assert.equal(oscText(bundle(1, ["/n_free", 1]), 100), "/n_free 1");
  assert.equal(oscText(bundle(100.5, ["/n_free", 1])), "/n_free 1");
});
