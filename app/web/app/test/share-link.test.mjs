// SPDX-License-Identifier: AGPL-3.0-or-later
// Links (src/share.js, over native's codec compiled to wasm: app/sharelink): a program or a whole set, as text
// (base64url) or as a QR code's digits, back as it went. Needs the codec built (scripts/build-runtime.sh --wasm).
import { test } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import { encodeCode, encodeDigits, decodeCode, toDigits, fromDigits, loadShareCodec, shareTable, makeCodec, qrCode } from "../src/share.js";
import jsQR from "jsqr";
import { serialise, deserialise, isSet } from "../src/set-bundle.js";
import { createHash } from "node:crypto";
await loadShareCodec().catch((e) => { throw new Error(`the share codec is not built (scripts/build-runtime.sh --wasm): ${e.message}`); });

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
    assert.match(encodeDigits(text), /^N[12]\d+$/);
  }
  assert.ok(isSet(decodeCode(encodeCode(set))) && !isSet(decodeCode(encodeCode(code))));
  assert.equal(deserialise(decodeCode(encodeDigits(set))).buffers[2], "#-- tricky\nplay 3\n");
});

test("a link from a newer format is refused, naming it", () => {
  assert.throws(() => decodeCode("9abc"), /newer Sonic Pi \(format "9"\)/);
  assert.throws(() => decodeCode("N9123"), /newer Sonic Pi/);
});

test("a link that lost its end says it was cut short; one that was changed, that it can't be read", () => {
  const link = encodeCode("live_loop :drums do\n  sample :bd_haus, amp: 2\n  sleep 0.5\nend\n");
  for (let n = 2; n < link.length; n++) assert.throws(() => decodeCode(link.slice(0, n)), /cut short/, link.slice(0, n));
  const digits = encodeDigits("play 60");
  assert.throws(() => decodeCode(digits.slice(0, -1)), /cut short/);
  assert.throws(() => decodeCode("1A_V*pAA"), /characters no link has/);
  assert.throws(() => decodeCode(""), /no code/);
});

test("a whole link reads as its code does, wherever it points and however it was pasted", () => {
  const code = "play 60";
  for (const link of [`https://sonic-pi.net/code.html#code=${encodeCode(code)}`, `http://localhost:8460/web/#code=${encodeCode(code)}\n`, `  ${encodeCode(code)} `])
    assert.equal(decodeCode(link), code);
});

// Format 1, for good: links made with it are out in the world, so these must read back as they are, whatever changes
// in the codec (docs/share-link-format.md has them as its test vectors). They were written by its first codec (fflate);
// the codec now (app/sharelink, zlib) writes other bytes for the same programs (WRITTEN, below), and reads both.
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

test("every link format 1's first codec wrote for the programs Sonic Pi ships reads back (fixtures/share-links-1.json)", () => {
  const { cases } = JSON.parse(fs.readFileSync(new URL("./fixtures/share-links-1.json", import.meta.url), "utf8"));
  assert.ok(cases.length > 500);
  for (const { key, code, link, digits } of cases) {
    assert.equal(decodeCode(link), code, key);
    assert.equal(decodeCode(digits), code, key);
  }
});

// Format 2 (zstd), for good too: its first codec's link for a program Sonic Pi ships, as docs/share-link-format.md
// has it.
const DNB = "# Coded by Sam Aaron\n\nuse_sample_bpm :loop_amen\n\nwith_fx :rlpf, cutoff: 10, cutoff_slide: 4 do |c|\n  live_loop :dnb do\n    sample :bass_dnb_f, amp: 5\n    sample :loop_amen, amp: 5\n    sleep 1\n    control c, cutoff: rrand(40, 120), cutoff_slide: rrand(1, 4)\n  end\nend\n";
const GOLDEN2 = [
  [DNB, "2AAB9AQAolaQx4mMM_HWRC8wZN-xykxW9zSfz-wxdNN4ZQyA5WhjCAe3k2ChNRnfecnD-FA", "N20000053688769960546216744574712949408268428473744504140072651141631503480181366407968161431360531286766084290732935537807405076"],
];

test("format 2's links read back as they always have", () => {
  for (const [program, link, digits] of GOLDEN2) {
    assert.equal(decodeCode(link), program);
    assert.equal(decodeCode(digits), program);
  }
});

test("every link format 2's first codec wrote for the programs Sonic Pi ships reads back (fixtures/share-links-2.json)", () => {
  const { cases } = JSON.parse(fs.readFileSync(new URL("./fixtures/share-links-2.json", import.meta.url), "utf8"));
  assert.ok(cases.length > 500);
  for (const { key, code, link, digits } of cases) {
    assert.equal(decodeCode(link), code, key);
    assert.equal(decodeCode(digits), code, key);
  }
});

// What the codec writes for GOLDEN's and GOLDEN2's programs: native's gui-tests (sharelink.test.cpp) hold native to
// these same links, so the two apps write one link for one program. A new zlib or zstd may choose other matches one
// day; any of them reads back, and then these are written afresh.
const WRITTEN = [
  ["1A_VpAA", "N10066414848"],
  ["1QypH1sGKIlARAgA", "N1189053114736665941343291904"],
  ["1m5oBOj4nJycfNEbTwDxFAcjdkwck1oPqDB0F5g_M85n7mHcCs18iaGccJ6hd9QB8lSgvaKgGAA", null],
  ["1Y2ZkZmJmVqhQqAcA", "N1279786040948424540722907694848"],
  ["1I9YdRhBrQZYpVSjFIqw2xWt1B2iIB9neOhABOuMmuxIoafzgAQA", null],
];

test("links are written as native writes them", () => {
  GOLDEN.forEach(([program], i) => {
    const [link, digits] = WRITTEN[i];
    assert.equal(encodeCode(program), link);
    if (digits) assert.equal(encodeDigits(program), digits);
  });
  for (const [program, link, digits] of GOLDEN2) {
    assert.equal(encodeCode(program), link);
    assert.equal(encodeDigits(program), digits);
  }
});

test("a link goes in whichever format makes it shorter: format 1 for a line, format 2 for a program", () => {
  const one = makeCodec(shareTable("1")), two = makeCodec(shareTable("2"));
  const inFormat = (format, codec, program) => format + Buffer.from(codec.encode(program)).toString("base64url");
  for (const program of ["play 60", DNB, GOLDEN[1][0]]) {
    const link = encodeCode(program), asOne = inFormat("1", one, program), asTwo = inFormat("2", two, program);
    assert.equal(link, asTwo.length <= asOne.length ? asTwo : asOne, program);
  }
  assert.equal(encodeCode("play 60")[0], "1");
  assert.equal(encodeCode(DNB)[0], "2");
  one.free();
  two.free();
});

test("the tables are the ones each format shipped with (a new table is a new format: scripts/build-share-table.mjs)", () => {
  const one = shareTable("1");
  assert.deepEqual([one.version, one.compression, one.vocab.length], ["1", "deflate", 836]);
  assert.equal(createHash("sha256").update(JSON.stringify({ version: one.version, vocab: one.vocab, dictionary: one.dictionary })).digest("hex"),
    "6dff3b3f31e0aaad2772c70c33883d2dd23e2ff1897eb3d3162d543f9af5e8e3");
  const two = shareTable("2");
  assert.deepEqual([two.version, two.compression, two.vocab.length, Buffer.from(two.dictionary, "base64").length], ["2", "zstd", 836, 524288]);
  assert.equal(createHash("sha256").update(JSON.stringify({ version: two.version, vocab: two.vocab, compression: two.compression, dictionary: two.dictionary })).digest("hex"),
    "7d5d50207087766f7e7ae31a32d1fd814659d03a830b5ef6362ecb29e72542a2");
});

// ── QR codes ──────────────────────────────────────────────────────────────
// Read back by a decoder of their own (jsQR, as a phone's camera would), so a code is known to scan, not only to
// have been made. Native draws the same code for the same link (sharelink.test.cpp pins this matrix too).

const HOME = "https://sonic-pi.net/code.html#code=";
// a code drawn as a screen shows it: px pixels a module, dark on white, with its quiet zone of 4 modules
function scanned(qr, px = 4) {
  const quiet = 4, n = (qr.size + 2 * quiet) * px, d = new Uint8ClampedArray(n * n * 4).fill(255);
  for (let r = 0; r < qr.size; r++) for (let c = 0; c < qr.size; c++) {
    if (!qr.isDark(r, c)) continue;
    for (let y = 0; y < px; y++) for (let x = 0; x < px; x++) { const i = (((r + quiet) * px + y) * n + (c + quiet) * px + x) * 4; d[i] = d[i + 1] = d[i + 2] = 0; }
  }
  return jsQR(d, n, n)?.data ?? null;
}
let seed = 1;
const words = (n) => { let t = ""; while (t.length < n) t += String.fromCharCode(33 + (seed = (seed * 1103515245 + 12345) % 2147483648) % 90); return t; };

test("a link's QR code scans back as the link: a program's, a set's, as digits or text", () => {
  const set = serialise(["play 1\n", "", "live_loop :x do\n  sleep 1\nend\n"], 2, [], { name: "Gig" });
  for (const code of ["play 60", "live_loop :drums do\n  sample :bd_haus, amp: 2\n  sleep 0.5\nend\n", set, words(1200)])
    for (const url of [HOME + encodeDigits(code), HOME + encodeCode(code)]) {
      const qr = qrCode(url);
      assert.ok(qr, url.slice(0, 60));
      assert.equal(scanned(qr), url);
    }
});

test("medium error correction while it fits, low when only that holds it, no code past version 25", () => {
  const at = (n) => qrCode(HOME + encodeDigits(words(n)));
  seed = 1;
  assert.equal(at(1000).level, "M");
  seed = 1;
  const low = at(1500);
  assert.equal(low.level, "L");
  assert.ok(low.version <= 25 && low.size === 17 + 4 * low.version);
  seed = 1;
  assert.equal(at(3000), null);
});

test("native draws the same code for the same link", () => {
  const qr = qrCode(HOME + encodeDigits(GOLDEN[1][0]));
  let rows = "";
  for (let r = 0; r < qr.size; r++) { for (let c = 0; c < qr.size; c++) rows += qr.isDark(r, c) ? "1" : "0"; rows += "\n"; }
  assert.deepEqual([qr.version, qr.level], [4, "M"]);
  assert.equal(createHash("sha256").update(rows).digest("hex"), "2bb02469f4a34d0d387b3de24ac93fb0069bd360d3abdf35230baef5fc10049d");
});
