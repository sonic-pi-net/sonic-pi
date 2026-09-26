// SPDX-License-Identifier: AGPL-3.0-or-later
// The bar's stop while a run waits on what it needs: the audio engine and the runtime on the first Run (the first
// press of Play on a shared link, say), then the synths and samples a program asks for. A comet circles the stop's
// ring, its tail growing with the load, and the stop's square fills with the accent from the bottom. When the sound
// starts the stop's own rings take the canvas (main.js stopRings); pressed while it loads, the stop stops the run.
//
// Drawn on the stop's canvas, frame by frame, and never in the page itself: the page changes only as a load starts
// and ends (body.loading), so a page-wide watcher (an ad blocker's) has nothing to hear (shadow.js).
//
// How far along it is: each part in flight has a weight (the engine and the runtime large, a sample by its size, a
// synth small) and counts as done on arrival; on its way, it counts for a share that grows as the time it has been
// loading does, so the tail keeps growing through a slow file rather than standing still. Shown, it never goes back:
// a part asked for late holds the tail where it is until the rest catches up.
//
//   const loading = createLoadingStop({ box, canvas, playing, after });   // playing(): its rings live; after(sounding): the canvas handed back
//   loading.begin(key, weight)   a part on its way (the engine, the runtime, SuperSonic's loading:start)
//   loading.end(key)             it arrived (loading:complete)
//   loading.expect()             a run waits for its first sound: the comet holds, whole, until it comes
//   loading.cancel()             Stop: nothing more to wait for
//   loading.sounding()           the sound started: the rings take the canvas
//   loading.showing              the comet is on the stop (its rings wait: main.js showJobs)
import { css } from "./theme.js";

const SHOW_AFTER = 200;   // ms: a load quicker than this (the files cached) shows nothing
// ms after the last part arrives: the whole comet holds until the sound starts (the runtime schedules a run's first
// note a moment after its loads), or the stop goes back to rest; a load no run waits on (the engine warmed up) at once
const SETTLE = { run: 3000, none: 400 };
const GIVE_UP = 30000;    // ms: a part that never says it arrived (a fetch that failed) stops counting
const TAU = Math.PI * 2, TOP = -Math.PI / 2;

export function createLoadingStop({ box, canvas, playing = () => false, after = () => {} }) {
  const reduceMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
  let parts = null;         // key → { weight, at, done }: this load's, from its first part to its last
  let waiting = 0;          // until when a run waits for its first sound (expect): a load ending before it holds, whole
  let shown = 0, raf = 0, showTimer = 0, settleTimer = 0, visible = false, last = 0;
  const label = box.getAttribute("aria-label");

  const progress = (now) => {
    let sum = 0, got = 0;
    for (const p of parts.values()) {
      const age = now - p.at, tau = 600 + p.weight * 500;
      const share = p.done || age > GIVE_UP ? 1 : 0.9 * (1 - Math.exp(-age / tau));
      sum += p.weight;
      got += p.weight * share;
    }
    return sum ? got / sum : 0;
  };

  function draw(now) {
    raf = requestAnimationFrame(draw);
    const dt = Math.min(0.1, (now - (last || now)) / 1000);
    last = now;
    shown = Math.max(shown, shown + (progress(now) - shown) * Math.min(1, dt * 8));   // eased toward the estimate, never back
    const dpr = window.devicePixelRatio || 1, side = Math.round((box.clientWidth || 24) * dpr);
    if (!side) return;
    if (canvas.width !== side) canvas.width = canvas.height = side;
    const c = canvas.getContext("2d"), accent = css("HighlightedBackground"), still = css("faintText");   // the stop at rest's grey, whatever it is lit as
    // the stop's own proportions (style.css .sn-stop): its ring inset 6.6% and 1.5px wide, its square 10 of 24 in the middle 60%
    const w = 1.5 * dpr, r = side * (0.5 - 0.066) - w / 2, mid = side / 2;
    c.clearRect(0, 0, side, side);
    c.strokeStyle = accent;
    c.lineWidth = w * 1.35;
    c.lineCap = "round";
    if (reduceMotion.matches || document.body.classList.contains("reduce-motion")) {
      c.beginPath(); c.arc(mid, mid, r, TOP, TOP + Math.max(0.05, TAU * shown)); c.stroke();   // still: the arc as far as it has got
    } else {
      // the comet: its head going round once a second or so, its tail from half a radian to the whole way round
      const head = (now / 1000) * TAU * 0.9, tail = 0.5 + (TAU - 0.7) * shown, n = 36;
      for (let i = 0; i < n; i++) {
        c.globalAlpha = Math.pow(1 - i / n, 1.6);
        c.beginPath(); c.arc(mid, mid, r, TOP + head - (tail * (i + 1)) / n, TOP + head - (tail * i) / n + 0.01); c.stroke();
      }
      c.globalAlpha = 1;
      c.fillStyle = accent;
      c.beginPath(); c.arc(mid + r * Math.cos(TOP + head), mid + r * Math.sin(TOP + head), w * 1.15, 0, TAU); c.fill();
    }
    // the square, at rest in the stop's colour, filling with the accent from the bottom
    const sq = side * 0.25, x = mid - sq / 2, y = mid - sq / 2, round = side * 0.0375;
    const square = (fill) => { c.fillStyle = fill; c.beginPath(); c.roundRect ? c.roundRect(x, y, sq, sq, round) : c.rect(x, y, sq, sq); c.fill(); };
    square(still);
    c.save(); c.beginPath(); c.rect(x - 1, y + sq * (1 - shown), sq + 2, sq * shown + 1); c.clip(); square(accent); c.restore();
  }

  function show() {
    // something already sounding: the stop is its rings, and says so; a load under it (a second run's samples) shows nothing more
    if (visible || !parts || playing()) return;
    visible = true;
    document.body.classList.add("loading");   // the stop shows, its square the canvas's (style.css)
    box.setAttribute("aria-label", "Loading: press to stop");
    last = 0;
    raf = requestAnimationFrame(draw);
  }
  // the load over: the sound's rings take the canvas (sounding), or the stop goes back to how it rests
  function finish(sounding = false) {
    clearTimeout(showTimer); clearTimeout(settleTimer);
    cancelAnimationFrame(raf);
    const was = visible;
    parts = null; shown = 0; visible = false;
    if (sounding) waiting = 0;
    if (!was) return;
    document.body.classList.remove("loading");
    box.setAttribute("aria-label", label);
    canvas.getContext("2d").clearRect(0, 0, canvas.width, canvas.height);
    after(sounding);   // the rings, live or at rest, draw from here
  }

  return {
    begin(key, weight = 1) {
      clearTimeout(settleTimer);
      if (!parts) { parts = new Map(); showTimer = setTimeout(show, SHOW_AFTER); }
      if (!parts.has(key)) parts.set(key, { weight, at: performance.now(), done: false });
    },
    end(key) {
      const p = parts?.get(key);
      if (!p) return;
      p.done = true;
      if ([...parts.values()].every((q) => q.done)) settleTimer = setTimeout(() => finish(false), performance.now() < waiting ? SETTLE.run : SETTLE.none);
    },
    /** A run waits for its first sound, from now, whatever loads under it (the engine's boot, then its samples). */
    expect() { waiting = performance.now() + 8000; },
    cancel: () => { waiting = 0; finish(false); },
    sounding: () => finish(true),
    get showing() { return visible; },
  };
}
