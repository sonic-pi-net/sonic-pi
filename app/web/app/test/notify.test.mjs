// SPDX-License-Identifier: AGPL-3.0-or-later
import { test } from "node:test";
import assert from "node:assert/strict";
import { ask, SAID } from "../src/notify.js";

const answers = (status) => async () => ({ status });

test("an address the server takes is thanked, sent trimmed with the trap field as it was", async () => {
  const sent = [];
  const said = await ask({ email: " ada@example.com ", website: "" }, async (fields) => { sent.push(fields); return { status: 200 }; });
  assert.equal(said, "ok");
  assert.deepEqual(sent, [{ email: "ada@example.com", website: "" }]);
});

test("what is not an address is said to be none, and is not sent at all", async () => {
  let sent = 0;
  const get = async () => { sent++; return { status: 200 }; };
  for (const not of ["", "   ", "ada", "ada@", "@example.com", "ada@example", "a b@example.com", "ada@example.com\nmallory@example.com", "a".repeat(250) + "@example.com"]) assert.equal(await ask({ email: not }, get), "invalid", JSON.stringify(not));
  assert.equal(sent, 0);
});

test("too many from one sender is said as that", async () => {
  assert.equal(await ask({ email: "ada@example.com" }, answers(429)), "slow");
});

test("a server that is not there, or answers anything else, is a failure to try again", async () => {
  assert.equal(await ask({ email: "ada@example.com" }, async () => { throw new Error("offline"); }), "fail");
  assert.equal(await ask({ email: "ada@example.com" }, answers(404)), "fail");
  assert.equal(await ask({ email: "ada@example.com" }, answers(403)), "fail");
});

test("there are words for every outcome", () => {
  for (const key of ["sending", "ok", "invalid", "slow", "fail"]) assert.ok(SAID[key], key);
});
