// SPDX-License-Identifier: AGPL-3.0-or-later
// Scopes: the engine's output drawn with a glow and a trail,
// in the theme's Scope colours. Three views, as native has them: the mono
// wave, both channels, and Lissajous (left across, right up), which the
// quickstart "Scope Art" cards draw with. The main scope reads what the engine
// sent to the audio output (its device scope), on the web as in the plugin;
// a card's scope mirrors it, reading the same.
import { css } from "./theme.js";
import { perfAdd } from "./perf.js";

// The frames a scope draws: about 43 ms at 48 kHz, as the analysers it once read gave
const WINDOW = 2048;

export class Scope {
  constructor(canvas, { mode = "wave", lineWidth = 2, glow = 12, trail = 0.25 } = {}) {
    this.canvas = canvas;
    this.ctx = canvas.getContext("2d");
    Object.assign(this, { mode, lineWidth, glow, trail });
    this.engine = null;
    this.source = null;
    this.w = 0;
    this.h = 0;
    this.running = false;
    this.resize = this.resize.bind(this);
    this.observer = new ResizeObserver(this.resize);
    this.observer.observe(canvas);
    this.resize();
  }

  resize() {
    const dpr = window.devicePixelRatio || 1;
    this.w = this.canvas.clientWidth;
    this.h = this.canvas.clientHeight;
    this.canvas.width = Math.max(1, Math.round(this.w * dpr));
    this.canvas.height = Math.max(1, Math.round(this.h * dpr));
    this.ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    if (!this.running) this.drawSilent();
  }

  /** Reads the engine's output: the newest WINDOW frames of what it sent to the device, each frame drawn. */
  attach(engine) {
    this.engine = engine;
    this.start();
  }

  /** Draws what another scope taps. */
  mirror(source) {
    this.source = source;
    this.start();
  }

  start() {
    if (this.running) return;
    this.running = true;
    const frame = () => {
      if (!this.running) return;
      const t0 = performance.now();
      if (!this.paused) this.draw();          // native's ScopePaused: the last frame stays
      perfAdd(this.source ? "cardScope" : "scope", performance.now() - t0);
      requestAnimationFrame(frame);
    };
    requestAnimationFrame(frame);
  }

  stop() {
    this.running = false;
    this.drawSilent();
  }

  destroy() {
    this.running = false;
    this.observer.disconnect();
  }

  drawSilent() {
    const { ctx, w, h } = this;
    ctx.globalAlpha = 1;
    ctx.fillStyle = css("PaneBackground");
    ctx.fillRect(0, 0, w, h);
    ctx.strokeStyle = css("WindowBorder");
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(0, h / 2);
    ctx.lineTo(w, h / 2);
    ctx.stroke();
  }

  stroke(colour) {
    const { ctx } = this;
    ctx.strokeStyle = colour;
    ctx.lineJoin = "round";
    if (this.glow > 0) {
      ctx.lineWidth = this.lineWidth + this.glow;
      ctx.globalAlpha = 0.12;
      ctx.stroke();
      ctx.lineWidth = this.lineWidth + this.glow / 3;
      ctx.globalAlpha = 0.25;
      ctx.stroke();
      ctx.globalAlpha = 1;
    }
    ctx.lineWidth = this.lineWidth;
    ctx.stroke();
  }

  // The engine's output into L and R: a mono output is drawn as both, and nothing yet as silence
  read(engine) {
    const L = (this.L ??= new Float32Array(WINDOW)), R = (this.R ??= new Float32Array(WINDOW));
    const got = engine.getDeviceScope(0, WINDOW);
    if (!got) { L.fill(0); R.fill(0); return; }
    const { frames, channels, interleaved } = got, lead = WINDOW - frames;
    L.fill(0, 0, lead);
    R.fill(0, 0, lead);
    for (let i = 0; i < frames; i++) {
      L[lead + i] = interleaved[i * channels];
      R[lead + i] = interleaved[i * channels + (channels > 1 ? 1 : 0)];
    }
  }

  draw() {
    const engine = this.engine ?? this.source?.engine;
    const { ctx, w, h } = this;
    if (!engine || !w || !h) return;
    this.read(engine);   // a mirror reads for itself, so it moves while the main scope is paused
    const { L, R } = this;
    ctx.globalAlpha = this.trail > 0 && this.mode !== "bars" ? 1 - this.trail : 1;
    ctx.fillStyle = css("PaneBackground");
    ctx.fillRect(0, 0, w, h);
    ctx.globalAlpha = 1;
    if (this.mode === "bars") return this.drawBars(L, R);
    const n = L.length;
    const clamp = (x) => Math.max(-1, Math.min(1, x));
    const plot = (data, y0, span) => {
      ctx.beginPath();
      for (let i = 0; i < n; i++) {
        const x = (i / (n - 1)) * w, y = y0 - clamp(data[i]) * span;
        i === 0 ? ctx.moveTo(x, y) : ctx.lineTo(x, y);
      }
    };
    if (this.mode === "lissajous") {
      const size = Math.min(w, h) * 0.45;
      ctx.beginPath();
      for (let i = 0; i < n; i++) {
        const x = w / 2 + clamp(L[i]) * size, y = h / 2 - clamp(R[i]) * size;
        i === 0 ? ctx.moveTo(x, y) : ctx.lineTo(x, y);
      }
      this.stroke(css("Scope"));
    } else if (this.mode === "stereo") {
      plot(L, h / 4, h / 4.4);
      this.stroke(css("Scope"));
      plot(R, (3 * h) / 4, h / 4.4);
      this.stroke(css("Scope_2"));
    } else {
      const mix = (this.mix ??= new Float32Array(n));
      for (let i = 0; i < n; i++) mix[i] = (L[i] + R[i]) / 2;
      plot(mix, h / 2, h / 2.2);
      this.stroke(css("Scope"));
    }
  }

  // Native's scope panel: a level meter per channel along the top, then each
  // channel's peaks as bars, the left above the centre line and the right
  // below it. A bar falls back slowly, so the picture holds still enough to read.
  drawBars(L, R) {
    const { ctx, w, h } = this;
    const n = L.length, BAR = 6, GAP = 2;
    const count = Math.max(1, Math.floor(w / (BAR + GAP)));
    const per = Math.max(1, Math.floor(n / count));
    if (this.peaks?.length !== count * 2) this.peaks = new Float32Array(count * 2);
    const peaks = this.peaks;
    let sumL = 0, sumR = 0;
    for (let b = 0; b < count; b++) {
      let pl = 0, pr = 0;
      for (let i = b * per, end = Math.min(n, i + per); i < end; i++) {
        const l = L[i], r = R[i];
        pl = Math.max(pl, Math.abs(l));
        pr = Math.max(pr, Math.abs(r));
        sumL += l * l;
        sumR += r * r;
      }
      peaks[2 * b] = Math.max(Math.min(1, pl), peaks[2 * b] * 0.82);
      peaks[2 * b + 1] = Math.max(Math.min(1, pr), peaks[2 * b + 1] * 0.82);
    }
    const meterH = Math.min(24, Math.round(h * 0.14));
    const mid = meterH + (h - meterH) / 2, span = (h - meterH) / 2 - 3;
    const left = css("Scope"), right = css("Scope_2");
    ctx.fillStyle = left;
    for (let b = 0; b < count; b++) {
      const y = Math.max(1, peaks[2 * b] * span);
      ctx.fillRect(b * (BAR + GAP), mid - y, BAR, y);
    }
    ctx.fillStyle = right;
    for (let b = 0; b < count; b++) {
      ctx.fillRect(b * (BAR + GAP), mid, BAR, Math.max(1, peaks[2 * b + 1] * span));
    }
    // the meters: RMS in dB over the last 60, a lit segment per step
    const SEG = 4, row = Math.max(2, (meterH - 8) / 2), segs = Math.floor(w / (SEG + 2));
    const level = (sum) => Math.max(0, Math.min(1, (20 * Math.log10(Math.sqrt(sum / (count * per)) || 1e-9) + 60) / 60));
    [level(sumL), level(sumR)].forEach((lv, ch) => {
      const y = 3 + ch * (row + 2), lit = Math.round(lv * segs);
      for (let s = 0; s < segs; s++) {
        ctx.globalAlpha = s < lit ? 1 : 0.2;
        ctx.fillStyle = right;
        ctx.fillRect(s * (SEG + 2), y, SEG, row);
      }
    });
    ctx.globalAlpha = 1;
  }
}
