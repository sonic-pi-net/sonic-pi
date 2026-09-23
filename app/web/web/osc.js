// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
// OSC as the runtime sends it, for the page and for Node: the framing of the
// runtime's outbox (runtime/host/sp_host.c, "The audio stream") and a
// decoder. Messages decode as [address, ...args] and bundles as
// {timeTag, packets}, as SuperSonic's own decoder has them.

export const FRAME_SOUND = 1;   // /sonic-pi/sound ,iib synthdef buffer bundle
export const FRAME_HOST = 2;    // /sonic-pi/synthdef, /sonic-pi/sample, /sonic-pi/sample_free ,is
export const FRAME_GUI = 3;     // /sonic-pi/<kind>: a record for the page (web/gui-stream.js)

/**
 * Each frame of the outbox at ptr on the heap: [u32 size, u32 kind]
 * (little-endian) and an OSC packet. fn(kind, start, size, view, offset):
 * start is the packet's place on the heap, offset its place in view.
 */
export function forEachFrame(heap, ptr, len, fn) {
  const view = new DataView(heap.buffer, heap.byteOffset + ptr, len);
  for (let off = 0; off + 8 <= len;) {
    const size = view.getUint32(off, true);
    fn(view.getUint32(off + 4, true), ptr + off + 8, size, view, off + 8);
    off += 8 + size;
  }
}

const text = new TextDecoder();
const TWO_POW_32 = 4294967296;

/** An OSC packet, a message or a bundle. A blob is a view into bytes. */
export function decode(bytes) {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  return packetAt(bytes, view, 0, bytes.length);
}

// an OSC string: its bytes, a NUL, padding to the next multiple of four
function stringAt(bytes, pos, end) {
  let e = pos;
  while (e < end && bytes[e] !== 0) e++;
  return [text.decode(bytes.subarray(pos, e)), (e + 4) & ~3];
}

function packetAt(bytes, view, pos, end) {
  if (end - pos >= 16 && bytes[pos] === 0x23 && text.decode(bytes.subarray(pos, pos + 8)) === "#bundle\0") {
    const timeTag = view.getUint32(pos + 8) + view.getUint32(pos + 12) / TWO_POW_32;
    const packets = [];
    for (let p = pos + 16; p + 4 <= end;) {
      const size = view.getUint32(p);
      packets.push(packetAt(bytes, view, p + 4, p + 4 + size));
      p += 4 + size;
    }
    return { timeTag, packets };
  }
  const [address, afterAddress] = stringAt(bytes, pos, end);
  const [tags, afterTags] = stringAt(bytes, afterAddress, end);
  const msg = [address];
  const open = [msg];              // OSC arrays, [ to ], nest
  let out = msg;
  let p = afterTags;
  for (const tag of tags.slice(1)) {
    switch (tag) {
      case "i": out.push(view.getInt32(p)); p += 4; break;
      case "f": out.push(view.getFloat32(p)); p += 4; break;
      case "d": out.push(view.getFloat64(p)); p += 8; break;
      case "h": out.push(Number(view.getBigInt64(p))); p += 8; break;
      case "t": out.push(view.getUint32(p) + view.getUint32(p + 4) / TWO_POW_32); p += 8; break;
      case "s": case "S": { const [s, next] = stringAt(bytes, p, end); out.push(s); p = next; break; }
      case "b": { const size = view.getUint32(p); out.push(bytes.subarray(p + 4, p + 4 + size)); p = (p + 4 + size + 3) & ~3; break; }
      case "T": out.push(true); break;
      case "F": out.push(false); break;
      case "N": out.push(null); break;
      case "[": { const a = []; out.push(a); open.push(a); out = a; break; }
      case "]": if (open.length > 1) { open.pop(); out = open[open.length - 1]; } break;
      default: throw new Error(`OSC type tag ${tag} is not decoded here`);
    }
  }
  return msg;
}
