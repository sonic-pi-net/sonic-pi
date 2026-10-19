// SPDX-License-Identifier: AGPL-3.0-or-later
// A live loop's thread, as the runtime names it, read back as the loop's name
// as written (lang.rb live_loop).
import { test } from "node:test";
import assert from "node:assert/strict";
import { loopName } from "../src/loop-name.js";

test("a live loop's thread is its loop's name", () => {
  assert.equal(loopName("live_loop_groove"), "groove");
});

test("any other thread is no loop's", () => {
  assert.equal(loopName("drums"), null);
  assert.equal(loopName(""), null);
  assert.equal(loopName(undefined), null);
});
