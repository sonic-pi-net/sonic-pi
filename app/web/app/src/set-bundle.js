// SPDX-License-Identifier: AGPL-3.0-or-later
// A set: all ten buffers saved together as one .sonicpi file, native Sonic Pi's
// format (app/gui/utils/setbundle.cpp, SetBundle), line for line, so a set
// saved by either opens in the other. Plain text with #-- marker lines: a
// header, the meta (the buffer showing, each buffer's zoom) as JSON, then one
// marker per buffer that has code, its code after it. The markers are Ruby
// comments, so the file stays readable and diffable. A line of code that
// itself starts #-- is written behind "#-- ~" and read back without it.
//
// Room to grow, as native's: the header names the format's version, and a file
// from a newer version is refused rather than misread. Anything new goes in
// the meta, whose keys this version does not know are kept (load.meta, handed
// back to serialise), so a set passed through either app loses none of them.
// The meta is written as Qt writes JSON, its keys sorted, byte for byte.
//
// The web's code has one size for every buffer, where native's buffers each
// have a zoom: a set's zooms are carried through as they came (setZooms), so a
// set passed through the web keeps them.

export const MAX_BUFFERS = 10;
export const VERSION = 1;
export const DEFAULT_ZOOM = 2, MIN_ZOOM = -5, MAX_ZOOM = 20;   // native's SonicPiScintilla::kDefaultZoom and bounds

const HEADER = `#-- Sonic Pi Set v${VERSION}`;
const HEADER_ANY = /^#-- Sonic Pi Set v(\d+)$/;
const META = "#-- meta ";
const ESCAPE = "#-- ~";
const BUFFER = /^#-- buffer (\d+)$/;

const clampZoom = (z) => Math.min(MAX_ZOOM, Math.max(MIN_ZOOM, z));
// Qt's QJsonValue::toInt(default): a whole number, else the default
const toInt = (v, d) => (typeof v === "number" && Number.isInteger(v) ? v : d);
// QJsonDocument's compact JSON, byte for byte: an object's keys in order, at every depth, and a number's exponent
// in at least two digits (1e-07, where JSON.stringify writes 1e-7)
const qtJSON = (v) => Array.isArray(v) ? `[${v.map(qtJSON).join(",")}]`
  : v && typeof v === "object" ? `{${Object.keys(v).sort().map((k) => `${JSON.stringify(k)}:${qtJSON(v[k])}`).join(",")}}`
  : typeof v === "number" ? JSON.stringify(v).replace(/e([+-])(\d)$/, "e$10$2")
  : JSON.stringify(v);

/** The set's text: buffers (strings, up to ten), the index of the buffer showing, each one's zoom, and meta keys to keep. */
export function serialise(buffers, current, zooms = [], keep = {}) {
  const meta = { ...keep, current, zooms: Array.from({ length: MAX_BUFFERS }, (_, i) => clampZoom(i < zooms.length ? zooms[i] : DEFAULT_ZOOM)) };
  let out = `${HEADER}\n${META}${qtJSON(meta)}\n`;
  for (let i = 0; i < MAX_BUFFERS && i < buffers.length; i++) {
    if (!buffers[i].trim()) continue;
    out += `#-- buffer ${i}\n`;
    for (const line of buffers[i].split("\n")) out += (line.startsWith("#--") ? ESCAPE : "") + line + "\n";
  }
  return out;
}

/** A set's text read: { ok, error, buffers (ten strings), zooms (ten), current, meta (as read, every key) }. */
export function deserialise(text) {
  const load = { ok: false, error: "", buffers: new Array(MAX_BUFFERS).fill(""), zooms: new Array(MAX_BUFFERS).fill(DEFAULT_ZOOM), current: 0, meta: {} };
  let body = text.replace(/\r\n/g, "\n");
  if (body.endsWith("\n")) body = body.slice(0, -1);   // every section ends with one newline: the last buffer grows no line of its own
  let at = -1, any = false;
  const sections = Array.from({ length: MAX_BUFFERS }, () => null);
  for (const line of body.split("\n")) {
    if (line.startsWith(ESCAPE) && at >= 0) { sections[at].push(line.slice(ESCAPE.length)); continue; }
    const m = BUFFER.exec(line);
    if (m) {
      const i = parseInt(m[1], 10);
      at = i >= 0 && i < MAX_BUFFERS ? i : -1;
      if (at >= 0) { any = true; sections[at] ??= []; }
      continue;
    }
    if (at < 0) {
      const h = HEADER_ANY.exec(line);
      if (h && parseInt(h[1], 10) > VERSION) {
        load.error = `it was made by a newer version of Sonic Pi (set format v${h[1]}, where this one reads up to v${VERSION}): update Sonic Pi to open it`;
        return load;
      }
      if (line.startsWith(META)) {
        let meta = null;
        try { meta = JSON.parse(line.slice(META.length)); } catch {}
        if (meta && typeof meta === "object" && !Array.isArray(meta)) {
          load.meta = meta;
          const c = toInt(meta.current, 0);
          load.current = c >= 0 && c < MAX_BUFFERS ? c : 0;
          const z = Array.isArray(meta.zooms) ? meta.zooms : [];
          for (let i = 0; i < MAX_BUFFERS && i < z.length; i++) load.zooms[i] = clampZoom(toInt(z[i], DEFAULT_ZOOM));
        }
      }
      continue;
    }
    sections[at].push(line);
  }
  if (!any) { load.error = "not a Sonic Pi set file"; return load; }
  sections.forEach((s, i) => { if (s?.length) load.buffers[i] = s.join("\n"); });
  load.ok = true;
  return load;
}

/** Whether a file's text is a set rather than a buffer's code. */
export const isSet = (text) => text.replace(/^﻿/, "").startsWith(HEADER);
