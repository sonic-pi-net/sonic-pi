// SPDX-License-Identifier: AGPL-3.0-or-later
// Links (src/share.js): a program or a whole set, as text (base64url) or as a QR code's digits, back as it went.
import { test } from "node:test";
import assert from "node:assert/strict";
import { encodeCode, encodeDigits, decodeCode, toDigits, fromDigits, loadShareCodec } from "../src/share.js";
await loadShareCodec();
import { serialise, deserialise, isSet } from "../src/set-bundle.js";
import { createHash } from "node:crypto";
import table from "../src/share-table.js";

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

// Format 1, for good: links made with it are out in the world, so these must read back as they are, whatever changes
// in the codec (docs/share-link-format.md has them as its test vectors). Making a link may one day write other bytes
// (another deflate can choose other matches, and any of them reads back); reading one never changes.
const GOLDEN = [
  ["play 60", "1A_VpAA", "N10066414848"],
  ["live_loop :drums do\n  sample :bd_haus, amp: 2\n  sleep 0.5\nend\n", "1QypHIFdtQYsQAA", "N11890530841325907309113600"],
  ["# héllo — ünïcode, 🎹 and a tab\there\nplay :e3\r\nsleep 1", "1m5oBOj4HOLYPGqNpYJ6iAOTuyQMS60F1ho4C8wfm-cx9zDuB2Q-4gzMxiRPUrgLNh6ca84KGagA", null],
  ["\u0001\u0002\u0003 x \u007f", "1Y2ZkZmJmBi7AqwcA", "N1279786040948423740200800929536"],
  ["#-- Sonic Pi Set v1\n#-- meta {\"current\":2,\"names\":[\"x\"],\"zooms\":[5,2,2,2,2,2,2,2,2,2]}\n#-- buffer 0\nplay 1\n\n#-- buffer 2\n#-- ~#-- tricky\nplay 3\n\n", "1I9YdwBwOHusBWgbcHRGLsNoUr9UdoCEeZHvrQATojJvsSqCk8YMHAA", null],
];

test("format 1's links read back, byte for byte, as they always have", () => {
  for (const [program, link, digits] of GOLDEN) {
    assert.equal(decodeCode(link), program);
    if (digits) assert.equal(decodeCode(digits), program);
  }
});

test("format 1's links are still written the same (a new deflate may change this: see the comment above)", () => {
  for (const [program, link, digits] of GOLDEN) {
    assert.equal(encodeCode(program), link);
    if (digits) assert.equal(encodeDigits(program), digits);
  }
});

test("format 1's table is the one it shipped with (a new table is a new format: scripts/build-share-table.mjs)", () => {
  const hash = createHash("sha256").update(JSON.stringify({ version: table.version, vocab: table.vocab, dictionary: table.dictionary })).digest("hex");
  assert.equal(table.version, "1");
  assert.equal(hash, "6dff3b3f31e0aaad2772c70c33883d2dd23e2ff1897eb3d3162d543f9af5e8e3");
});
