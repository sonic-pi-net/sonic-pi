// SPDX-License-Identifier: AGPL-3.0-or-later
// The web's sets (src/set-bundle.js) against native Sonic Pi's own: every case in fixtures/set-bundle.json is what
// native's SetBundle wrote or read (scripts/gen-set-fixtures.mjs), and the web must write the same bytes and read
// the same buffers, zooms, current buffer, meta and verdict.
import { test } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import { serialise, deserialise, isSet, VERSION } from "../src/set-bundle.js";

const { cases } = JSON.parse(fs.readFileSync(new URL("./fixtures/set-bundle.json", import.meta.url), "utf8"));

test("the web writes every set byte for byte as native does", () => {
  const writes = cases.filter((c) => !("text" in c.in));
  assert.ok(writes.length > 100);
  for (const { in: c, out } of writes) assert.equal(serialise(c.buffers, c.current, c.zooms, c.meta), out.text, JSON.stringify(c));
});

test("the web reads every file as native does", () => {
  const reads = cases.filter((c) => "text" in c.in);
  assert.ok(reads.length > 100);
  for (const { in: c, out } of reads) {
    const { ok, error, buffers, zooms, current, meta } = deserialise(c.text);
    assert.deepEqual({ ok, error, buffers, zooms, current, meta }, out, JSON.stringify(c.text));
  }
});

test("a set from a newer version is refused, one from this version or before read", () => {
  assert.equal(deserialise(`#-- Sonic Pi Set v${VERSION + 1}\n#-- buffer 0\nx`).ok, false);
  assert.match(deserialise(`#-- Sonic Pi Set v${VERSION + 1}\n#-- buffer 0\nx`).error, /newer version of Sonic Pi/);
  assert.equal(deserialise(`#-- Sonic Pi Set v${VERSION}\n#-- buffer 0\nx`).ok, true);
});

test("meta the web does not know survives a load and a save", () => {
  const load = deserialise('#-- Sonic Pi Set v1\n#-- meta {"current":1,"names":["drums"],"zooms":[4]}\n#-- buffer 1\nplay 60\n');
  const again = deserialise(serialise(load.buffers, 3, load.zooms, load.meta));
  assert.deepEqual(again.meta.names, ["drums"]);
  assert.equal(again.current, 3);
  assert.equal(again.zooms[0], 4);
});

test("a set is told from a buffer's code by its header", () => {
  assert.ok(isSet("#-- Sonic Pi Set v1\n#-- buffer 0\nx"));
  assert.ok(isSet("﻿#-- Sonic Pi Set v1\n"));
  assert.ok(!isSet("play 60\n#-- Sonic Pi Set v1"));
});
