// SPDX-License-Identifier: AGPL-3.0-or-later
// The page reads the runtime's audio stream with web/osc.js: OSC decoded as
// SuperSonic decodes it, and the outbox's frames. Packets are built by hand
// here, byte by byte, big-endian as OSC is.
import { test } from "node:test";
import assert from "node:assert/strict";
import { decode, forEachFrame, FRAME_SOUND, FRAME_HOST } from "../../web/osc.js";

const cat = (...parts) => {
  const out = new Uint8Array(parts.reduce((n, p) => n + p.length, 0));
  let at = 0;
  for (const p of parts) { out.set(p, at); at += p.length; }
  return out;
};
const str = (s) => { const b = new Uint8Array(s.length + 4 - (s.length % 4)); for (let i = 0; i < s.length; i++) b[i] = s.charCodeAt(i); return b; };
const num = (set, size) => (v) => { const b = new Uint8Array(size); new DataView(b.buffer)[set](0, v); return b; };
const i32 = num("setInt32", 4), u32 = num("setUint32", 4), f32 = num("setFloat32", 4), f64 = num("setFloat64", 8);

test("strings pad to a multiple of four, with at least one NUL, whatever their length", () => {
  for (const s of ["", "a", "abc", "abcd", "abcde"]) {
    assert.deepEqual(decode(cat(str("/x"), str(",si"), str(s), i32(-7))), ["/x", s, -7]);
  }
});

test("ints, float32s, doubles and blobs, each at its size and padding", () => {
  const blob = new Uint8Array([1, 2, 3, 4, 5]);
  const m = decode(cat(str("/s_new"), str(",sifdbi"), str("sonic-pi-beep"), i32(10001), f32(0.1), f64(0.1), u32(5), blob, new Uint8Array(3), i32(9)));
  assert.equal(m[0], "/s_new");
  assert.equal(m[1], "sonic-pi-beep");
  assert.equal(m[2], 10001);
  assert.equal(m[3], Math.fround(0.1));
  assert.equal(m[4], 0.1);
  assert.deepEqual([...m[5]], [1, 2, 3, 4, 5]);
  assert.equal(m[6], 9);
});

test("OSC arrays nest, and T, F and N take no bytes", () => {
  assert.deepEqual(decode(cat(str("/x"), str(",i[s[id]]TFN"), i32(1), str("a"), i32(2), f64(0.5))), ["/x", 1, ["a", [2, 0.5]], true, false, null]);
});

test("a bundle's time tag is NTP seconds and a fraction of 2^32", () => {
  const msg = cat(str("/n_free"), str(",i"), i32(10002));
  const b = decode(cat(str("#bundle"), u32(3913056000), u32(2 ** 31), u32(msg.length), msg));
  assert.equal(b.timeTag, 3913056000.5);
  assert.deepEqual(b.packets, [["/n_free", 10002]]);
});

test("the outbox is frames of size and kind, little-endian, each an OSC packet", () => {
  const a = cat(str("/sonic-pi/synthdef"), str(",is"), i32(0), str("sonic-pi-beep"));
  const b = cat(str("/sonic-pi/sample"), str(",is"), i32(100), str("bd_haus.flac"));
  const head = (size, kind) => { const h = new Uint8Array(8); const v = new DataView(h.buffer); v.setUint32(0, size, true); v.setUint32(4, kind, true); return h; };
  const heap = cat(new Uint8Array(12), head(a.length, FRAME_HOST), a, head(b.length, FRAME_SOUND), b);
  const seen = [];
  forEachFrame(heap, 12, heap.length - 12, (kind, start, size) => seen.push([kind, decode(heap.subarray(start, start + size))]));
  assert.deepEqual(seen, [[FRAME_HOST, ["/sonic-pi/synthdef", 0, "sonic-pi-beep"]], [FRAME_SOUND, ["/sonic-pi/sample", 100, "bd_haus.flac"]]]);
});
