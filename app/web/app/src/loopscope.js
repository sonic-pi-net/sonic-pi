// SPDX-License-Identifier: AGPL-3.0-or-later
// A live loop's own scope, as native's LiveLoopScopeWidget draws it
// (widgets/sonicpiscintilla.cpp): the loop's scope_out stream, read out of the
// engine's shared memory (SuperSonic's getScope), as a short sweep locked to a
// rising zero crossing (a tiny stationary oscilloscope) or, in scroll mode, the
// last ~250ms rolling by. Dormant grey when silent, warming to the accent as
// signal arrives, on the line itself. The editor's inline scopes draw
// with this; the heat model is native's, mapped in dB so colour means
// "playing", not "loud".
import { colour } from "./theme.js";

// how far a quiet loop's wave is lifted to be seen: -18 dBFS fills the strip, anything quieter is drawn at its own
// scale against that (a silent loop's noise floor stays a flat line)
const MAX_GAIN = 8;

export const SWEEP_WINDOW = 1200;    // ~25ms at 48k
export const SWEEP_SEARCH = 1200;    // the trigger search before it: periods down to ~40Hz
export const SCROLL_WINDOW = 12000;  // ~250ms at 48k
const QUIET_FLOOR = 0.002;

/** One scope's running state: its display window, and the colour level's fast attack and ~0.5s decay. */
export class LoopScopeState {
  constructor() {
    this.level = 0;
    this.samples = null;
    this.mix = null;
    this.lastCursor = null;
    this.settled = false;
  }

  /**
   * Feeds the newest frames of the stream ({ frames, channels, interleaved, writePosition }
   * from getScope, or null for a slot that has gone). Returns true when there is
   * something new to draw.
   */
  feed(frame, scroll) {
    if (!frame || (this.lastCursor !== null && frame.writePosition === this.lastCursor)) {
      // the stream stalled (silent, or stopped) or the slot was released: cool down
      return this.decay();
    }
    this.lastCursor = frame.writePosition;
    const { interleaved, channels, frames: n } = frame;
    const mix = this.mix?.length === n ? this.mix : (this.mix = new Float32Array(n));
    for (let i = 0; i < n; i++) {
      let s = 0;
      for (let c = 0; c < channels; c++) s += interleaved[i * channels + c];
      mix[i] = s / channels;
    }
    let start = 0, len = n;
    if (!scroll) {
      // native's copy_triggered_window: the window is the newest ~25ms, slid back
      // to the nearest rising zero crossing of the mix within the search span
      len = Math.min(SWEEP_WINDOW, n);
      start = n - len;
      const lo = Math.max(1, start - SWEEP_SEARCH);
      for (let i = start; i > lo; i--) {
        if (mix[i - 1] < 0 && mix[i] >= 0) { start = i; break; }
      }
    }
    this.samples = mix.subarray(start, start + len);
    // the level follows only the newest 1024 frames, so it tracks fresh hits
    let peak = 0;
    for (let i = Math.max(0, n - 1024); i < n; i++) peak = Math.max(peak, Math.abs(mix[i]));
    this.level = Math.max(peak, this.level * 0.86);
    if (peak >= QUIET_FLOOR) this.settled = false;
    if (this.settled) return false;                 // a silent flatline rolling by: nothing to repaint
    if (this.level < 0.001 && peak < QUIET_FLOOR) { this.level = 0; this.settled = true; }   // one last flat paint, then rest
    return true;
  }

  decay() {
    if (this.settled) return false;
    this.level = this.level < 0.001 ? 0 : this.level * 0.86;
    if (this.level === 0) { this.settled = true; this.samples = null; }
    return true;
  }

  /** 0 = dormant grey, 1 = full accent: silence (< -60dB) stays grey, anything above -40dB is fully lit. */
  get heat() {
    const db = 20 * Math.log10(Math.max(this.level, 1e-6));
    return Math.max(0, Math.min(1, (db + 60) / 20));
  }
}

const lerp = (a, b, t) => ({ r: a.r + (b.r - a.r) * t, g: a.g + (b.g - a.g) * t, b: a.b + (b.b - a.b) * t, a: a.a + (b.a - a.a) * t });
const rgba = (c, a = c.a) => `rgba(${Math.round(c.r)}, ${Math.round(c.g)}, ${Math.round(c.b)}, ${a})`;
const roundRect = (ctx, x, y, w, h, r) => {
  ctx.beginPath();
  ctx.moveTo(x + r, y);
  ctx.arcTo(x + w, y, x + w, y + h, r);
  ctx.arcTo(x + w, y + h, x, y + h, r);
  ctx.arcTo(x, y + h, x, y, r);
  ctx.arcTo(x, y, x + w, y, r);
  ctx.closePath();
};

/** Paints a scope's state into its canvas, as native paints: the panel, the midline, the trace as a filled body under a line. */
export function drawLoopScope(canvas, state, { scroll = false } = {}) {
  const w = canvas.clientWidth, h = canvas.clientHeight;
  if (!w || !h) return;
  const dpr = window.devicePixelRatio || 1;
  const W = Math.round(w * dpr), H = Math.round(h * dpr);
  if (canvas.width !== W || canvas.height !== H) { canvas.width = W; canvas.height = H; }
  const ctx = canvas.getContext("2d");
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  ctx.clearRect(0, 0, w, h);

  // native's colours: the theme foreground faded right down until signal warms it to the Scope colour
  const quiet = { ...colour("Foreground"), a: 120 / 255 };
  const wave = lerp(quiet, { ...colour("Scope"), a: 1 }, state.heat);
  const mid = h / 2, radius = 3;

  // no panel of its own: the trace sits on the line as the code does, over whatever the line shows (a running loop's
  // wash); only clipped to its rounded box
  roundRect(ctx, 0.5, 0.5, w - 1, h - 1, radius);
  ctx.save();
  ctx.clip();

  ctx.strokeStyle = rgba(wave, 60 / 255);
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.moveTo(0, mid);
  ctx.lineTo(w, mid);
  ctx.stroke();

  const s = state.samples;
  if (s && s.length >= 2 && w >= 2) {
    // A strip this short maps a quiet loop to nothing: at 19 px tall, a pluck through an FX (-29 dBFS) is a third of
    // a pixel, which reads as "the scope is broken" rather than "this is quiet". The wave is lifted towards the
    // strip's height, capped so noise in a silent loop stays flat, and the colour (heat) remains the loudness cue —
    // nothing here says the loop is louder than it is, it only makes its shape legible.
    const gain = Math.min(MAX_GAIN, 1 / Math.max(state.level, 1 / MAX_GAIN));
    const amp = mid * 0.9, n = s.length, cols = Math.max(2, Math.floor(w));
    const clamp = (v) => Math.max(-1, Math.min(1, v * gain));
    if (n > cols * 2) {
      // scroll: ~50 samples a pixel, so a min/max envelope per column, or short hits alias away
      const mins = new Float32Array(cols);
      ctx.beginPath();
      for (let x = 0; x < cols; x++) {
        const i0 = Math.floor((x / cols) * n), i1 = Math.max(Math.floor(((x + 1) / cols) * n), i0 + 1);
        let mn = 1, mx = -1;
        for (let i = i0; i < i1 && i < n; i++) { mn = Math.min(mn, s[i]); mx = Math.max(mx, s[i]); }
        const px = (x / (cols - 1)) * w;
        mins[x] = mid - clamp(mn) * amp;
        if (x === 0) ctx.moveTo(px, mid - clamp(mx) * amp); else ctx.lineTo(px, mid - clamp(mx) * amp);
      }
      for (let x = cols - 1; x >= 0; x--) ctx.lineTo((x / (cols - 1)) * w, mins[x]);
      ctx.closePath();
      ctx.fillStyle = rgba(wave, 150 / 255);
      ctx.fill();
      ctx.strokeStyle = rgba(wave, 1);
      ctx.lineWidth = 1;
      ctx.stroke();
    } else {
      // sweep: a line through the window, its body filled down to the midline
      ctx.beginPath();
      for (let x = 0; x < cols; x++) {
        const v = clamp(s[Math.floor((x / (cols - 1)) * (n - 1))]);
        const px = (x / (cols - 1)) * w, y = mid - v * amp;
        if (x === 0) ctx.moveTo(px, y); else ctx.lineTo(px, y);
      }
      ctx.save();
      ctx.lineTo(w, mid);
      ctx.lineTo(0, mid);
      ctx.closePath();
      ctx.fillStyle = rgba(wave, 60 / 255);
      ctx.fill();
      ctx.restore();
      ctx.beginPath();
      for (let x = 0; x < cols; x++) {
        const v = clamp(s[Math.floor((x / (cols - 1)) * (n - 1))]);
        const px = (x / (cols - 1)) * w, y = mid - v * amp;
        if (x === 0) ctx.moveTo(px, y); else ctx.lineTo(px, y);
      }
      ctx.strokeStyle = rgba(wave, 1);
      ctx.lineWidth = 1.6;
      ctx.lineJoin = "round";
      ctx.stroke();
    }
  }
  ctx.restore();
}
