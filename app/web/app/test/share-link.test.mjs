// SPDX-License-Identifier: AGPL-3.0-or-later
// Links (src/share.js): a program or a whole set, as text (base64url) or as a QR code's digits, back as it went.
import { test } from "node:test";
import assert from "node:assert/strict";
import { encodeCode, encodeDigits, decodeCode, toDigits, fromDigits } from "../src/share.js";
import { serialise, deserialise, isSet } from "../src/set-bundle.js";

test("any bytes go to digits and back, every length", () => {
  for (let n = 0; n < 60; n++) {
    const bytes = Uint8Array.from({ length: n }, (_, i) => (i * 97 + n * 31) % 256);
    assert.deepEqual(fromDigits(toDigits(bytes)), bytes);
  }
  assert.deepEqual(fromDigits(toDigits(new Uint8Array(23).fill(255))), new Uint8Array(23).fill(255));
});

test("digits that are cut short or too big are refused", () => {
  assert.throws(() => fromDigits("1234"));                // no length of bytes is four digits
  assert.throws(() => fromDigits("9".repeat(17)));        // more than seven bytes hold
});

test("a program and a set both go by link, as text or digits", () => {
  const code = "use_synth :prophet\nplay 40, release: 8, cutoff: 95\n";
  const set = serialise(["play 1\n", "", "#-- tricky\nplay 3\n"], 2, [5], { names: ["x"] });
  for (const text of [code, set]) {
    assert.equal(decodeCode(encodeCode(text)), text);
    assert.equal(decodeCode(encodeDigits(text)), text);
    assert.match(encodeDigits(text), /^N1\d+$/);
  }
  assert.ok(isSet(decodeCode(encodeCode(set))) && !isSet(decodeCode(encodeCode(code))));
  assert.equal(deserialise(decodeCode(encodeDigits(set))).buffers[2], "#-- tricky\nplay 3\n");
});

test("a link from a newer format is refused, naming it", () => {
  assert.throws(() => decodeCode("9abc"), /newer Sonic Pi/);
  assert.throws(() => decodeCode("N9123"), /newer Sonic Pi/);
});
