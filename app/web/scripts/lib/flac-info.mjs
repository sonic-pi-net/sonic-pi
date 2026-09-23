// SPDX-License-Identifier: AGPL-3.0-or-later
// Frame count, channels and rate from a FLAC file's STREAMINFO block, the
// facts the runtime needs about a sample without decoding it.
export function flacInfo(buf) {          // rate 20 | chans-1 3 | bps-1 5 | total samples 36
  if (buf.toString("latin1", 0, 4) !== "fLaC") throw new Error("not a FLAC file");
  const len = buf.readUIntBE(5, 3), info = buf.subarray(8, 8 + len);
  const hi = info.readUInt32BE(10), lo = info.readUInt32BE(14);
  return { rate: hi >>> 12, chans: ((hi >>> 9) & 7) + 1, frames: ((hi & 0xf) * 4294967296) + lo };
}
