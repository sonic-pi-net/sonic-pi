// SPDX-License-Identifier: AGPL-3.0-or-later
// A program in a link: #code= then one character naming the format, then the
// program compressed and base64url'd — as the app's Share button writes it,
// as its page reads it back, and as sonic-pi.net's live cards hand a card
// into the app (site.js). The link is the whole of it: nothing is stored.
//
// Format 1 packs the source as a token stream (share-table.js: Sonic Pi's
// vocabulary — its functions, synths, fx, samples, opts and Ruby's words — as
// one, two or three bytes each; a newline with its indent as one) and
// deflates that (raw DEFLATE, RFC 1951) against a preset dictionary: 32 KB of
// that token stream, the pieces Sonic Pi programs share most, trained on the
// language's own examples and thousands of its players' programs
// (scripts/build-share-table.mjs). The table and the dictionary are fixed for
// the format character; a new table is a new character, and every link stays
// readable. The whole format is written down in docs/share-link-format.md.
import { deflateSync, inflateSync } from "fflate";

const unb64 = (s) => Uint8Array.from(atob(s), (c) => c.charCodeAt(0));
export const b64url = (bytes) => btoa(String.fromCharCode(...bytes)).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
export const unb64url = (s) => Uint8Array.from(atob(s.replace(/-/g, "+").replace(/_/g, "/")), (c) => c.charCodeAt(0));

// The token stream. Bytes 0x80–0xDF and 0xF0–0xFF are the 112 one-byte tokens,
// 0xE0–0xEF a newline followed by 0–15 indents of two spaces, 0x01 n a
// two-byte token, 0x02 hi lo a three-byte one, 0x03 b a source byte that
// would otherwise read as one of these; every other byte is the source's own.
const ONE = 112, TWO = 256;
const isWord = (c) => /[A-Za-z0-9_]/.test(c);
// A codec from a table: { vocab, dictionary (the dictionary's bytes, base64) } as share-table.js has it, or, while a
// table is being made (scripts/build-share-table.mjs), { vocab, dict (text, packed here) }.
export function makeCodec({ vocab, dict = "", dictionary: dictionaryB64 = null }) {
  const byFirst = new Map();   // first character → its tokens, longest first
  vocab.forEach((t, k) => { const l = byFirst.get(t[0]) ?? []; l.push([t, k]); byFirst.set(t[0], l); });
  for (const l of byFirst.values()) l.sort((a, b) => b[0].length - a[0].length);
  const utf8 = new TextEncoder(), text = new TextDecoder();

  function pack(src) {
    const out = [];
    let i = 0;
    while (i < src.length) {
      if (src[i] === "\n") {
        let n = 0;
        while (src[i + 1 + n] === " " && n < 31) n++;
        if (n % 2 === 0 && n / 2 < 16) { out.push(0xE0 + n / 2); i += 1 + n; continue; }
        out.push(10); i++; continue;
      }
      let hit = -1, len = 0;
      for (const [t, k] of byFirst.get(src[i]) ?? []) {
        if (!src.startsWith(t, i)) continue;
        if (isWord(t[0]) && i > 0 && isWord(src[i - 1])) continue;
        if (isWord(t[t.length - 1]) && i + t.length < src.length && isWord(src[i + t.length])) continue;
        hit = k; len = t.length; break;
      }
      if (hit >= 0) {
        if (hit < 96) out.push(0x80 + hit);
        else if (hit < ONE) out.push(0xF0 + hit - 96);
        else if (hit < ONE + TWO) out.push(0x01, hit - ONE);
        else out.push(0x02, (hit - ONE - TWO) >> 8, (hit - ONE - TWO) & 255);
        i += len; continue;
      }
      const ch = String.fromCodePoint(src.codePointAt(i));
      for (const b of utf8.encode(ch)) { if (b >= 0x80 || b < 0x09) out.push(0x03, b); else out.push(b); }
      i += ch.length;
    }
    return Uint8Array.from(out);
  }

  function unpack(buf) {
    let s = "", i = 0;
    const raw = [];
    const flush = () => { if (raw.length) { s += text.decode(Uint8Array.from(raw)); raw.length = 0; } };
    const token = (k) => { const t = vocab[k]; if (t === undefined) throw new Error("token out of range"); flush(); s += t; };
    while (i < buf.length) {
      const b = buf[i];
      if (b >= 0xF0) { token(96 + b - 0xF0); i++; }
      else if (b >= 0xE0) { flush(); s += "\n" + "  ".repeat(b - 0xE0); i++; }
      else if (b >= 0x80) { token(b - 0x80); i++; }
      else if (b === 0x01) { token(ONE + buf[i + 1]); i += 2; }
      else if (b === 0x02) { token(ONE + TWO + (buf[i + 1] << 8) + buf[i + 2]); i += 3; }
      else if (b === 0x03) { raw.push(buf[i + 1]); i += 2; }
      else { raw.push(b); i++; }
    }
    flush();
    return s;
  }

  const dictionary = dictionaryB64 != null ? unb64(dictionaryB64) : pack(dict);
  return {
    pack, unpack,
    encode: (src) => deflateSync(pack(src), { level: 9, mem: 12, dictionary }),
    decode: (bytes) => unpack(inflateSync(bytes, { dictionary })),
  };
}

// The bytes as decimal digits, for a QR code: its numeric mode holds a third more than the byte mode base64url
// needs (Sonic Pi's example sets: 56% fit a code as base64url, 85% as digits). Seven bytes to seventeen digits, the
// last few bytes to as few digits as hold them; a length of digits names its bytes, so the tail needs no marker.
const TAIL = [0, 3, 5, 8, 10, 13, 15, 17];   // k bytes → digits
export function toDigits(bytes) {
  let out = "";
  for (let i = 0; i < bytes.length; i += 7) {
    const chunk = bytes.subarray(i, i + 7);
    let n = 0n;
    for (const b of chunk) n = (n << 8n) | BigInt(b);
    out += n.toString().padStart(TAIL[chunk.length], "0");
  }
  return out;
}
export function fromDigits(s) {
  const tail = s.length % 17, k = TAIL.indexOf(tail);
  if (!/^\d*$/.test(s) || k < 0) throw new Error("the link's digits are cut short");
  const out = [];
  for (let i = 0; i < s.length; ) {
    const len = s.length - i >= 17 ? 17 : tail, bytes = TAIL.indexOf(len);
    let n = BigInt(s.slice(i, i + len));
    if (n >> BigInt(8 * bytes)) throw new Error("the link's digits are not a program");
    const chunk = new Array(bytes);
    for (let j = bytes - 1; j >= 0; j--) { chunk[j] = Number(n & 255n); n >>= 8n; }
    out.push(...chunk);
    i += len;
  }
  return Uint8Array.from(out);
}

// A link's program: a buffer's code or a whole set (set-bundle.js: the .sonicpi file, whose header tells it
// apart), packed the same way. The format character first; a link for a QR code puts N before it and the bytes in
// digits after, so the digits can go in the code's numeric mode.
// The table (share-table.js: the vocabulary and a dictionary that fills most of deflate's window) is fetched the
// first time a link is made or read: most visits do neither, and making the codec costs a moment at startup too.
// loadShareCodec() starts it (the Share button, as a pointer reaches it; a page opened on a link); the rest below
// need it loaded.
export const FORMAT = "1";
let formats = null, loading = null;
export const loadShareCodec = () => (loading ??= import("./share-table.js").then(({ default: table }) => {
  if (table.version !== FORMAT) throw new Error(`share-table.js is format ${table.version}, the codec writes ${FORMAT}`);
  formats = { [table.version]: makeCodec(table) };
}));
const codec = (f) => {
  if (!formats) throw new Error("the share codec is not loaded (share.js loadShareCodec)");
  return formats[f];
};
export const encodeCode = (code) => FORMAT + b64url(codec(FORMAT).encode(code));
export const encodeDigits = (code) => "N" + FORMAT + toDigits(codec(FORMAT).encode(code));
export function decodeCode(s) {
  const digits = s[0] === "N", f = digits ? s[1] : s[0];
  const c = codec(f);
  if (!c) throw new Error(`a link from a newer Sonic Pi (format ${JSON.stringify(f ?? "")})`);
  return c.decode(digits ? fromDigits(s.slice(2)) : unb64url(s.slice(1)));
}
