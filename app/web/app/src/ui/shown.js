// SPDX-License-Identifier: AGPL-3.0-or-later
// A drawing loop that runs only while what it draws can be seen: its element on the screen (not display:none, not
// scrolled out of its viewport) and the page itself visible. The browser says when that changes (an
// IntersectionObserver, visibilitychange), so nothing is asked each frame; out of sight, the loop takes no frames at
// all. A view hidden behind the site's pages, a pane closed, a phone's tab in the background: none of them costs.
//
//   const loop = animateWhileShown(canvas, (t) => draw(t), { onStop: () => … });
//   loop.wake();   // something to draw now (new data) while shown: the loop is running already, or starts
//   loop.shown     // whether it is on screen now
//   loop.rest()    // no frames until wake(), or until it comes back into view; loop.stop(): none ever again

export function animateWhileShown(el, frame, { onStop = null } = {}) {
  let raf = 0, onScreen = false;
  const tick = (t) => { raf = requestAnimationFrame(tick); frame(t); };
  const update = () => {
    const on = onScreen && !document.hidden;
    if (on && !raf) raf = requestAnimationFrame(tick);
    else if (!on && raf) { cancelAnimationFrame(raf); raf = 0; onStop?.(); }
  };
  const seen = new IntersectionObserver((entries) => { onScreen = entries[entries.length - 1].isIntersecting; update(); });
  seen.observe(el);
  document.addEventListener("visibilitychange", update);
  return {
    wake: update,
    get shown() { return onScreen && !document.hidden; },
    /** Stop until wake(): a loop with nothing more to draw (a scope gone quiet). */
    rest() { if (raf) { cancelAnimationFrame(raf); raf = 0; } },
    /** Done with for good: no frames, nothing watched. */
    stop() { this.rest(); onScreen = false; seen.disconnect(); document.removeEventListener("visibilitychange", update); },
  };
}
