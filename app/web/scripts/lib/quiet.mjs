// SPDX-License-Identifier: AGPL-3.0-or-later
// The checks make no sound. Chromium can be launched muted (--mute-audio); WebKit cannot, and every run would play
// through the speakers. So each page the checks open has, before its own script runs, anything connected to the
// audio output go through a gain of 0 on its way there: the engine, its scopes and meters all still run on the
// signal (they read it before the output), and the output itself is the real one, its channel count and all.
//
//   import { quiet } from "./lib/quiet.mjs";
//   const browser = quiet(await webkit.launch());
export function silenceOutput() {
  if (typeof AudioNode === "undefined") return;
  const connect = AudioNode.prototype.connect, disconnect = AudioNode.prototype.disconnect;
  const hush = new WeakMap();   // the output → its gain of 0
  const muted = (out) => {
    let g = hush.get(out);
    if (!g) { g = new GainNode(out.context, { gain: 0 }); connect.call(g, out); hush.set(out, g); }
    return g;
  };
  AudioNode.prototype.connect = function (to, ...rest) {
    if (to instanceof AudioDestinationNode) { connect.call(this, muted(to), ...rest); return to; }
    return connect.call(this, to, ...rest);
  };
  AudioNode.prototype.disconnect = function (to, ...rest) {
    if (to instanceof AudioDestinationNode) return disconnect.call(this, muted(to), ...rest);
    return disconnect.call(this, to, ...rest);
  };
}

/** The browser, every page and context it opens silent (silenceOutput runs before each page's own script). */
export function quiet(browser) {
  const newContext = browser.newContext.bind(browser);
  browser.newContext = async (opts) => {
    const c = await newContext(opts);
    await c.addInitScript(silenceOutput);
    return c;
  };
  browser.newPage = async (opts) => {
    const c = await browser.newContext(opts);
    const p = await c.newPage();
    p.on("close", () => c.close().catch(() => {}));   // as a page of its own context, closed with it
    return p;
  };
  return browser;
}
