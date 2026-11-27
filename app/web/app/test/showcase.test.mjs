// SPDX-License-Identifier: AGPL-3.0-or-later
// The showcase (app/src/showcase.js): the parts of it that need no browser. The whole of it, the app playing in a
// frame on the plugin page, is in scripts/browser-check.mjs.
import test from "node:test";
import assert from "node:assert/strict";
import { request, memoryStorage, pacedContext, quietInput, VIEWS, begin, dressAsPlugin, RENDER_MOST } from "../src/showcase.js";

// An offline context as the pacing needs one: it renders up to the next place it was asked to stop, and no further
// until it is resumed.
class FakeOffline {
  currentTime = 0;
  resumes = 0;
  #stops = [];
  #ended = null;
  constructor(options) { this.options = options; }
  suspend(at) { return new Promise((resolve) => this.#stops.push([at, resolve])); }
  resume() { this.resumes++; this.#render(); return Promise.resolve(); }
  startRendering() { this.#render(); return new Promise((resolve) => (this.#ended = resolve)); }
  end() { this.#ended?.(); }
  #render() { const stop = this.#stops.shift(); if (stop) { this.currentTime = stop[0]; stop[1](); } }
}

// A clock in the test's hands, and timers that fire as it is moved on.
function clock() {
  let now = 0;
  const waiting = [];
  const settle = async () => { for (let i = 0; i < 20; i++) await Promise.resolve(); };
  return {
    now: () => now,
    later: (fn, ms) => waiting.push({ at: now + Math.max(0, ms), fn }),
    /** The clock moved on by `ms`, a millisecond at a time, each due timer run as it comes due. */
    async advance(ms) {
      await settle();
      for (const end = now + ms; now < end;) {
        now += 1;
        for (let due; (due = waiting.findIndex((w) => w.at <= now)) >= 0;) { waiting.splice(due, 1)[0].fn(); await settle(); }
      }
    },
  };
}

test("a page not framed for a showcase is not one", () => {
  assert.equal(request({ frameElement: null }), null, "the app opened as itself");
  assert.equal(request({ frameElement: {} }), null, "framed, with nothing asked");
  assert.equal(request({ frameElement: { showcase: { view: "roll" } } }), null, "asked, with no code to play");
  assert.equal(request({ get frameElement() { throw new Error("another origin's frame"); } }), null, "a frame it may not read");
});

test("a page framed with code to play is a showcase of it", () => {
  const asked = { code: "play 60", view: "timeline" };
  assert.equal(request({ frameElement: { showcase: asked } }), asked);
});

test("the storage keeps what it is given, as text, and nothing of it leaves", () => {
  const s = memoryStorage({ a: "1" });
  assert.equal(s.getItem("a"), "1");
  assert.equal(s.getItem("b"), null);
  s.setItem("b", 2);
  assert.equal(s.getItem("b"), "2", "kept as text, as a browser's storage keeps it");
  assert.equal(s.length, 2);
  assert.deepEqual([s.key(0), s.key(1), s.key(2)], ["a", "b", null]);
  s.removeItem("a");
  assert.equal(s.getItem("a"), null);
  s.clear();
  assert.equal(s.length, 0);
  assert.equal(memoryStorage().length, 0, "another one starts empty: they share nothing");
});

test("the silent context renders at the pace of the clock", async () => {
  const c = clock();
  const { context } = pacedContext({ OfflineAudioContext: FakeOffline, now: c.now, later: c.later, rate: 48000, block: 2400, seconds: 60 });
  assert.deepEqual(context.options, { numberOfChannels: 1, length: 48000 * 60, sampleRate: 48000 });
  assert.equal(context.state, "running", "the app sees a context that is running");
  await c.advance(1000);
  assert.ok(Math.abs(context.currentTime - 1) <= 0.1, `a second of the clock, a second rendered: ${context.currentTime}`);
  await c.advance(4000);
  assert.ok(Math.abs(context.currentTime - 5) <= 0.1, `five seconds: ${context.currentTime}`);
});

test("the app's own resume does not hurry it", async () => {
  const c = clock();
  const { context } = pacedContext({ OfflineAudioContext: FakeOffline, now: c.now, later: c.later, block: 2400 });
  await c.advance(500);
  const at = context.currentTime;
  for (let i = 0; i < 50; i++) await context.resume();   // the app resumes a context it thinks is waiting for a gesture
  assert.equal(context.currentTime, at, "it rendered no further for being asked");
  await c.advance(500);
  assert.ok(Math.abs(context.currentTime - 1) <= 0.1, `and kept its pace afterwards: ${context.currentTime}`);
});

test("held, it renders nothing; let go, it carries on from where it was", async () => {
  const c = clock();
  const paced = pacedContext({ OfflineAudioContext: FakeOffline, now: c.now, later: c.later, block: 2400 });
  await c.advance(1000);
  paced.hold(true);
  await c.advance(200);   // the block under way when it was held finishes
  const at = paced.context.currentTime;
  await c.advance(10000);
  assert.equal(paced.context.currentTime, at, "ten seconds held: nothing rendered");
  paced.hold(false);
  await c.advance(1000);
  assert.ok(Math.abs(paced.context.currentTime - (at + 1)) <= 0.1, `a second on from where it was, not eleven: ${paced.context.currentTime - at}`);
});

test("it says when it has rendered all it can", async () => {
  const c = clock();
  let ended = 0;
  const { context } = pacedContext({ OfflineAudioContext: FakeOffline, now: c.now, later: c.later, onEnd: () => ended++ });
  context.end();
  await c.advance(1);
  assert.equal(ended, 1);
});

test("what a person does to it is stopped before the app hears it, and what a script does is not", () => {
  const heard = new Map();
  const target = { addEventListener: (type, fn, opts) => heard.set(type, { fn, opts }) };
  quietInput(target);
  for (const type of ["pointerdown", "click", "keydown", "wheel", "paste", "drop"]) assert.ok(heard.has(type), `${type} is listened for`);
  assert.equal(heard.get("click").opts.capture, true, "before the app's own listeners");
  const event = (isTrusted) => ({ isTrusted, cancelable: true, stopped: false, prevented: false, stopImmediatePropagation() { this.stopped = true; }, preventDefault() { this.prevented = true; } });
  const person = event(true), script = event(false);
  heard.get("click").fn(person);
  heard.get("click").fn(script);
  assert.deepEqual([person.stopped, person.prevented], [true, true], "a person's click goes no further");
  assert.deepEqual([script.stopped, script.prevented], [false, false], "the app's own click (a button it presses) does");
});

test("every view names a pane of the app", () => {
  assert.deepEqual(Object.keys(VIEWS), ["processes", "timeline", "roll", "metrics", "logs"]);
  for (const [pane, view] of Object.values(VIEWS)) {
    assert.ok(["insight", "debug", "logs"].includes(pane));
    if (pane === "insight") assert.ok(["processes", "timeline", "roll"].includes(view));
  }
});

test("a showcase asked to look like the plugin says which format, and otherwise is the web app it is", () => {
  const win = (showcase) => ({ frameElement: { showcase }, document: { addEventListener() {}, documentElement: {} }, addEventListener() {}, OfflineAudioContext: class { constructor() { this.currentTime = 0; } suspend() { return new Promise(() => {}); } resume() { return Promise.resolve(); } startRendering() { return new Promise(() => {}); } } });
  assert.equal(begin(win({ code: "play 60", plugin: "VST" })).plugin, "VST");
  assert.equal(begin(win({ code: "play 60" })).plugin, "");
  assert.equal(begin(win({ code: "play 60", plugin: "Nonsense" })).plugin, "", "only a format the plugin comes in");
  assert.equal(begin(win({ code: "play 60" })).line, 1, "from its first line, unless said");
  assert.equal(begin(win({ code: "play 60", line: 15 })).line, 15);
  assert.equal(begin(win({ code: "play 60", line: "15; drop" })).line, 1, "a line is a whole number");
});

test("dressed as the plugin, the bar has the plugin's name and only the plugin's tabs", () => {
  const tab = (key) => ({ dataset: { tab: key }, removed: false, remove() { this.removed = true; } });
  const tabs = ["about", "examples", "learn", "tutorial", "support", "plugin", "code"].map(tab);
  const brand = { textContent: "sonic-pi.net" }, nav = { attrs: {}, setAttribute(k, v) { this.attrs[k] = v; } };
  const document = { querySelectorAll: (q) => (q.includes(".ic-tab") ? tabs : []), querySelector: (q) => (q.includes("sn-brand") ? brand : q.includes("site-nav") ? nav : null) };
  dressAsPlugin(document);
  assert.deepEqual(tabs.filter((t) => !t.removed).map((t) => t.dataset.tab), ["examples", "tutorial", "code"]);
  assert.equal(brand.textContent, "Sonic Pi");
  assert.equal(nav.attrs["aria-label"], "Sonic Pi");
});

test("a change the framing page keeps under a key it handed over reaches the showcase; other keys do not", () => {
  const win = { frameElement: { showcase: { code: "play 60", storage: { "sp-theme": "{}" }, follows: ["sp-later"] } }, document: { addEventListener() {}, documentElement: {} }, addEventListener() {},
    OfflineAudioContext: class { constructor() { this.currentTime = 0; } suspend() { return new Promise(() => {}); } resume() { return Promise.resolve(); } startRendering() { return new Promise(() => {}); } } };
  const made = begin(win);
  const told = [];
  made.kept((key, text) => told.push([key, text]));
  made.keep("sp-theme", '{"scheme":"light"}');
  made.keep("sp-buffers", "someone's code");
  made.keep("sp-theme", null);
  made.keep("sp-later", "x");
  assert.deepEqual(told, [["sp-theme", '{"scheme":"light"}'], ["sp-later", "x"]], "and a key it said it may keep under later");
  assert.equal(win.localStorage.getItem("sp-theme"), '{"scheme":"light"}', "and it is what the showcase finds kept");
  assert.equal(win.localStorage.getItem("sp-buffers"), null);
});

test("a silent showcase's render holds little: what it holds is taken at once, and WebKit keeps it when the page goes", () => {
  let asked = null;
  class Offline { constructor(o) { asked = o; this.currentTime = 0; } suspend() { return new Promise(() => {}); } resume() { return Promise.resolve(); } startRendering() { return new Promise(() => {}); } }
  pacedContext({ OfflineAudioContext: Offline, now: () => 0, later: () => {} });
  const bytes = asked.numberOfChannels * asked.length * 4;   // 32-bit floats, a channel each
  assert.ok(bytes <= RENDER_MOST, `${(bytes / 1048576).toFixed(1)} MB held, at most ${RENDER_MOST / 1048576} MB`);
  assert.ok(asked.length / asked.sampleRate >= 120, "and still plays a good two minutes before it starts again");
  assert.equal(asked.sampleRate, 48000, "at the rate the engine's sounds are made for");
});
