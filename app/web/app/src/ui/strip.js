// SPDX-License-Identifier: AGPL-3.0-or-later
// The event strip: a card's own sounds, scrolling by in its footer — a small, quiet piano roll of just this
// card's run (the Threads pane's roll, ../piano-roll.js, keeps the model). Time runs right to left with now at
// the right edge; a note is a bar on its pitch's height, solid while held and faint through its release, bent
// where it slides; a sample hit is a tick standing across the strip, tall as it is loud, glowing as it lands and
// fading as it passes. It draws while the card plays and for a window after, then clears.
//
//   const strip = createEventStrip(canvas, { now, defaults });
//   strip.record(r); strip.start(); strip.stop(time); strip.clear()
import { createRollModel } from "../piano-roll.js";
import { css } from "../theme.js";

const WINDOW = 6;        // seconds across the strip
const LANE = 0;          // the notes take the whole height; hits stand across it
const TOP = 4;
const MIN_SPAN = 12;     // semitones shown at least

/**
 * @param canvas   the strip's canvas (the card sizes it; this draws at the device's resolution)
 * @param o.now       () → the session's clock in seconds, or null before the engine runs
 * @param o.defaults  (synth) → the synth's opt defaults (envelopes), or null
 */
export function createEventStrip(canvas, { now, defaults = () => null } = {}) {
  const model = createRollModel({ defaults });
  const reduceMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
  let raf = 0, live = false, lo = 60, hi = 72, last = -Infinity;
  const ctx = canvas.getContext("2d");

  function frame() {
    raf = 0;
    const t = now?.();
    if (t == null) { if (live) raf = requestAnimationFrame(frame); return; }
    const dpr = window.devicePixelRatio || 1;
    const w = Math.round(canvas.clientWidth * dpr), h = Math.round(canvas.clientHeight * dpr);
    if (!w || !h) { if (live) raf = requestAnimationFrame(frame); return; }
    if (canvas.width !== w || canvas.height !== h) { canvas.width = w; canvas.height = h; }
    ctx.clearRect(0, 0, w, h);
    const notes = model.notes(), hits = model.hits();
    const from = t - WINDOW;
    // the rows: the pitches in view, eased toward, never fewer than a span
    let nlo = Infinity, nhi = -Infinity;
    for (const n of notes) { if (model.span(n, t).end < from) continue; for (const s of n.segs) { nlo = Math.min(nlo, s.note); nhi = Math.max(nhi, s.note); } }
    if (nlo <= nhi) {
      let tlo = nlo - 2, thi = nhi + 2;
      if (thi - tlo < MIN_SPAN) { const mid = (tlo + thi) / 2; tlo = mid - MIN_SPAN / 2; thi = mid + MIN_SPAN / 2; }
      lo += (tlo - lo) * 0.15; hi += (thi - hi) * 0.15;
    }
    const x = (time) => w * (1 - (t - time) / WINDOW);
    const y = (n) => (TOP + ((hi - n) / (hi - lo)) * (h / dpr - TOP - LANE)) * dpr;
    const accent = css("HighlightedBackground");
    ctx.lineCap = "round"; ctx.lineJoin = "round";
    // the glow: a wide faint stroke under the line (a canvas shadow is drawn by the pixel and stutters on some browsers)
    const glow = (path, width, alpha) => { path(); ctx.strokeStyle = accent; ctx.globalAlpha = alpha * 0.22; ctx.lineWidth = (width + 6) * dpr; ctx.stroke(); path(); ctx.globalAlpha = alpha; ctx.lineWidth = width * dpr; ctx.stroke(); };
    for (const n of notes) {
      const { hold, end } = model.span(n, t);
      if (end < from) continue;
      const pts = model.path(n, end);
      const amp = Math.min(1, n.amp ?? 1);
      const seg = (a, b, alpha, width) => glow(() => {
        ctx.beginPath();
        let first = true;
        for (let i = 0; i < pts.length; i++) {
          const p = pts[i];
          const pt = Math.min(b, Math.max(a, p.t));
          if (p.t < a && i < pts.length - 1 && pts[i + 1].t <= a) continue;
          if (first) { ctx.moveTo(x(pt), y(p.n)); first = false; } else ctx.lineTo(x(pt), y(p.n));
          if (p.t >= b) break;
        }
      }, width, alpha);
      seg(n.start, hold, 0.45 + 0.55 * amp, 3);               // held: solid
      if (end > hold) seg(hold, end, 0.12 + 0.2 * amp, 2);     // the release: fading away
    }
    // a hit: a tick standing across the strip, as tall as it is loud, bright and glowing as it lands, thinning as it goes
    for (const hit of hits) {
      if (hit.time < from) continue;
      const age = Math.max(0, t - hit.time);
      const gone = age / WINDOW;
      const amp = Math.min(1, Math.max(0.2, hit.amp ?? 1));
      const tall = (0.45 + 0.55 * amp) * h, cy = h / 2, hx = x(hit.time);
      const fresh = Math.max(0, 1 - age / 0.35);   // the landing: a flare that is gone within a beat
      const path = () => { ctx.beginPath(); ctx.moveTo(hx, cy - tall / 2); ctx.lineTo(hx, cy + tall / 2); };
      const alpha = Math.max(0.1, 1 - gone) * (0.55 + 0.45 * fresh);
      if (fresh > 0) { path(); ctx.strokeStyle = accent; ctx.globalAlpha = alpha * 0.35 * fresh; ctx.lineWidth = (6 + 10 * fresh) * dpr; ctx.stroke(); }   // the landing's flare
      glow(path, 1.5 + 1.5 * fresh, alpha);
    }
    ctx.globalAlpha = 1;
    if (live || t < last + WINDOW) raf = requestAnimationFrame(frame);
    else ctx.clearRect(0, 0, w, h);
  }
  const run = () => { if (!raf && !reduceMotion.matches) raf = requestAnimationFrame(frame); };

  return {
    /** A record of the card's run (a sound, a control, a kill, a sleep telling the tempo). */
    record(r) { model.record(r); if (r.time != null) last = Math.max(last, r.time); run(); },
    /** The card plays: the strip runs. */
    start() { live = true; run(); },
    /** The card stopped at this time: what was to come never sounds; the strip runs on until the window has passed. */
    stop(time) { live = false; if (time != null) model.stop(time); run(); },
    clear() { model.clear(); last = -Infinity; live = false; cancelAnimationFrame(raf); raf = 0; ctx.clearRect(0, 0, canvas.width, canvas.height); },
  };
}
