// SPDX-License-Identifier: AGPL-3.0-or-later
// A program in a link: #code= then one character naming the format, then the
// program compressed and base64url'd — as the app's Share button writes it,
// as its page reads it back, and as sonic-pi.net's live cards hand a card
// into the app (site.js). The link is the whole of it: nothing is stored.
//
// The codec is native Sonic Pi's own (app/sharelink), compiled to wasm
// (scripts/build-runtime.sh: build/runtime/sp_share.mjs): one codec, so a link
// either app writes the other reads, and both write the same link. It packs the
// source as a token stream (Sonic Pi's vocabulary as one, two or three bytes
// each; a newline with its indent as one) and compresses that against a preset
// dictionary, the pieces Sonic Pi programs share most (scripts/build-share-table.mjs):
// format 1 with deflate, format 2 with zstd and a far bigger dictionary.
// The table and the dictionary are fixed for the format character; a new table
// is a new character, and every link stays readable. The whole format is
// written down in docs/share-link-format.md.
//
// The module is fetched the first time a link is made or read: most visits do
// neither. loadShareCodec() starts it (the Share button, as a pointer reaches
// it; a page opened on a link); the rest below need it loaded.

let codec = null, loading = null;
const where = () => (typeof document === "undefined"
  ? new URL("../../build/runtime/sp_share.mjs", import.meta.url)   // Node: the tests and scripts, from the checkout
  : new URL("runtime/sp_share.mjs", document.baseURI));            // a page: beside it, as the runtime is (web/runtime.js)
export const loadShareCodec = () => (loading ??= import(where().href).then(({ default: load }) => load()).then((m) => { codec = m; }));

const utf8 = new TextEncoder(), text = new TextDecoder();
const loaded = () => {
  if (!codec) throw new Error("the share codec is not loaded (share.js loadShareCodec)");
  return codec;
};
const answer = (m) => m.HEAPU8.slice(m._sl_out_ptr(), m._sl_out_ptr() + m._sl_out_len());
// a call into the codec with these bytes (and its arguments before and after them), its answer's bytes back; a call
// that can fail and did throws its reason
function call(fn, bytes, before = [], after = []) {
  const m = loaded(), at = m._malloc(Math.max(1, bytes.length));
  try {
    m.HEAPU8.set(bytes, at);
    const ok = m[fn](...before, at, bytes.length, ...after);
    if (ok === 0) throw new Error(text.decode(answer(m)));
    return answer(m);
  } finally { m._free(at); }
}

/** A link's code for a program (or a set's file): the format, then the program in base64url. */
export const encodeCode = (code) => text.decode(call("_sl_encode", utf8.encode(code), [], [0]));
/** The same for a QR code: N, the format, then the program's bytes as digits, for the code's numeric mode. */
export const encodeDigits = (code) => text.decode(call("_sl_encode", utf8.encode(code), [], [1]));
/** A link's code (or a whole link) read back as its program; what was wrong with it thrown. */
export const decodeCode = (s) => text.decode(call("_sl_decode", utf8.encode(s)));

/**
 * A link as a QR code (native's, the same code for the same link): { version, level ("M" or "L"), size (modules a
 * side), isDark(row, col) }, or null when it is too big for a code a phone can read (past version 25).
 */
export function qrCode(link) {
  const m = loaded(), bytes = utf8.encode(link), at = m._malloc(Math.max(1, bytes.length));
  try {
    m.HEAPU8.set(bytes, at);
    const version = m._sl_qr(at, bytes.length);
    if (!version) return null;
    const out = answer(m), size = out[1];
    return { version, level: String.fromCharCode(out[0]), size, isDark: (row, col) => out[2 + row * size + col] === 1 };
  } finally { m._free(at); }
}

/** Bytes as the digits a QR code's numeric mode holds, and back. */
export const toDigits = (bytes) => text.decode(call("_sl_to_digits", bytes));
export const fromDigits = (s) => call("_sl_from_digits", utf8.encode(s));

/** A format's table, as the codec has it compiled in: { version, vocab, compression ("deflate" or "zstd"), dictionary (base64) }. */
export function shareTable(format) {
  const m = loaded();
  return m._sl_table_json(format.charCodeAt(0)) ? JSON.parse(text.decode(answer(m))) : null;
}

/** A codec from any table, { vocab, dictionary (base64, or none), compression ("deflate" or "zstd") }, for making a new one (scripts/build-share-table.mjs). */
export function makeCodec({ vocab, dictionary = "", compression = "deflate" }) {
  const m = loaded();
  const v = utf8.encode(vocab.join("\n")), d = Uint8Array.from(atob(dictionary), (c) => c.charCodeAt(0));
  const at = m._malloc(v.length + d.length + 1);
  m.HEAPU8.set(v, at);
  m.HEAPU8.set(d, at + v.length);
  const h = m._sl_codec_new(at, v.length, at + v.length, d.length, compression === "zstd" ? 1 : 0);
  m._free(at);
  return {
    pack: (src) => call("_sl_codec_pack", utf8.encode(src), [h]),
    encode: (src) => call("_sl_codec_encode", utf8.encode(src), [h]),
    unpack: (bytes) => text.decode(call("_sl_codec_unpack", bytes, [h])),
    decode: (bytes) => text.decode(call("_sl_codec_decode", bytes, [h])),
    free: () => m._sl_codec_free(h),
  };
}
