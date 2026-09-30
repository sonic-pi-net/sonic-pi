#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// The deck (app/src/ui/deck.js) and the runs the status calls live (web/runtime.js liveRuns), under Node, with
// cards and a session made up here: no browser, no engine, a second to run.
//
// A card is its runs. Each Play of a card is a run of the runtime's, there from its start to its end as any run
// is, and the card has no group of its own to outlive them: Stop stops each run the card started, as a subtree
// (Scheduler#stop_run), and the card is over when none of its runs is live. A loop follows the run that last
// redefined it (Scheduler#move_loop), so whose a thread is is read from the tree, not from the run it was born in.
//
//   node scripts/deck-check.mjs
import { createDeck } from "../app/src/ui/deck.js";
import * as runtime from "../web/runtime.js";

let pass = 0, fail = 0;
const test = async (name, body) => {
  const problems = [];
  const expect = (ok, what) => { if (!ok) problems.push(what); };
  try { await body(expect); } catch (e) { problems.push(`threw ${e?.message ?? e}`); }
  console.log(`${problems.length ? "FAIL" : "pass"} ${name}${problems.length ? ": " + problems.join("; ") : ""}`);
  if (problems.length) fail++; else pass++;
};
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);

// ── the runs that are live, from the process table ──────────────────────────────────────────────────────────
const F = runtime.PROCESS_FIELDS, KIND = { run: 0, main: 1, loop: 2, fx: 6, synth: 7, group: 9 };
const table = (...rows) => rows.flatMap((r) => F.map((f) => ({ parent: -1, job: -1, state: 0, line: -1, wake: -1, beat: 0, bpm: 60, active: -1, events: 0, redefs: 0, ended: -1, node: -1, group: 0, ...r })[f]));
const liveRuns = (t) => runtime.liveRuns?.(t);

await test("a run is live while a loop of it sleeps, its main thread long done", (expect) => {
  const t = table({ uid: 1, kind: KIND.run, job: 3, state: 0 }, { uid: 2, parent: 1, kind: KIND.main, job: 3, state: 3 }, { uid: 3, parent: 2, kind: KIND.loop, job: 3, state: 1 });
  expect(same(liveRuns(t), [3]), `live runs ${JSON.stringify(liveRuns(t))}, expected [3]`);
});
await test("a loop is the run's that last redefined it: born in run 5, hanging from run 7, it keeps 7 live and not 5", (expect) => {
  const t = table(
    { uid: 1, kind: KIND.run, job: 5, state: 3 }, { uid: 2, parent: 1, kind: KIND.main, job: 5, state: 3 },
    { uid: 10, kind: KIND.run, job: 7, state: 0 }, { uid: 11, parent: 10, kind: KIND.main, job: 7, state: 3 },
    { uid: 3, parent: 11, kind: KIND.loop, job: 5, state: 1 });   // its job column is the run it was born in
  expect(same(liveRuns(t), [7]), `live runs ${JSON.stringify(liveRuns(t))}, expected [7]`);
});
await test("a one-shot's run is live while its note sounds in its fx, and over once that has ended", (expect) => {
  const rows = (sound, fx) => table({ uid: 1, kind: KIND.run, job: 2 }, { uid: 2, parent: 1, kind: KIND.main, job: 2, state: 3 }, { uid: 4, parent: 2, kind: KIND.fx, job: 2, state: fx }, { uid: 5, parent: 4, kind: KIND.synth, job: 2, state: sound });
  expect(same(liveRuns(rows(0, 1)), [2]), `sounding: ${JSON.stringify(liveRuns(rows(0, 1)))}, expected [2]`);
  expect(same(liveRuns(rows(3, 1)), [2]), `its fx still open: ${JSON.stringify(liveRuns(rows(3, 1)))}, expected [2]`);
  expect(same(liveRuns(rows(3, 3)), []), `all ended: ${JSON.stringify(liveRuns(rows(3, 3)))}, expected []`);
});
await test("runs under a group's row are found, the group is no run, and a row whose parent is gone is nobody's", (expect) => {
  const t = table({ uid: 20, kind: KIND.group, group: 2000 }, { uid: 1, parent: 20, kind: KIND.run, job: 4, group: 2000 }, { uid: 2, parent: 1, kind: KIND.main, job: 4, state: 1, group: 2000 },
    { uid: 9, parent: 77, kind: KIND.loop, job: 6, state: 1 });
  expect(same(liveRuns(t), [4]), `live runs ${JSON.stringify(liveRuns(t))}, expected [4]`);
});

// ── the deck ────────────────────────────────────────────────────────────────────────────────────────────────
const FADE = 0.25;
const makeCard = (key) => ({ key, job: null, playing: false, errors: [], el: { querySelector: () => ({ textContent: "" }) }, code: () => `play 60 # ${key}`,
  setError(e) { if (e) this.errors.push(e); }, setBooting() {}, clearOutput() {}, setPlaying(on) { this.playing = on; }, detach() {}, flash() {}, record() {} });
const makeSession = () => {
  const s = { plays: [], stops: [], groupsAsked: 0, next: 1, held: null };
  s.hooks = {
    play: (code, opts) => { s.plays.push(opts); if (s.held) return new Promise((resolve) => { s.release = () => resolve(s.next++); }); return Promise.resolve(s.next++); },
    stopRun: (job, fade) => s.stops.push([job, fade]),
    stopGroup: (group, fade) => s.stops.push([`group ${group}`, fade]),   // what a card stopped by before: never again
    scopeFrame: () => null,
  };
  return s;
};

await test("a card's Play is a run of its own: the deck asks for no group and names none", async (expect) => {
  const s = makeSession(), deck = createDeck(s.hooks), card = deck.add(makeCard("a"));
  await card.onRun();
  expect(s.plays.length === 1 && typeof s.plays[0].scopeSlot === "number", `played with ${JSON.stringify(s.plays)}`);
  expect(!("group" in (s.plays[0] ?? {})), `the run names a group: ${JSON.stringify(s.plays[0])}`);
  expect(card.playing && deck.playing === 1 && deck.owns(1), "the card should be playing its run");
});
await test("Stop stops the card's run, with the fade", async (expect) => {
  const s = makeSession(), deck = createDeck(s.hooks), card = deck.add(makeCard("a"));
  await card.onRun();
  card.onStop();
  expect(same(s.stops, [[1, FADE]]), `stopped ${JSON.stringify(s.stops)}, expected [[1,${FADE}]]`);
  expect(!card.playing && deck.playing === null, "the card should be at rest");
});
await test("a card played again while playing is two runs, and Stop stops them both", async (expect) => {
  const s = makeSession(), deck = createDeck(s.hooks), card = deck.add(makeCard("a"));
  await card.onRun();
  await card.onRun();
  expect(same(s.plays.map((p) => p.scopeSlot), [s.plays[0].scopeSlot, s.plays[0].scopeSlot]), "both runs draw on the card's one scope slot");
  card.onStop();
  expect(same(s.stops, [[1, FADE], [2, FADE]]), `stopped ${JSON.stringify(s.stops)}, expected both runs`);
});
await test("a card stopped while its run's head is still going: the run is stopped as it arrives, at once", async (expect) => {
  const s = makeSession(), deck = createDeck(s.hooks), card = deck.add(makeCard("a"));
  s.held = true;
  const played = card.onRun();
  deck.stop();
  expect(same(s.stops, []), `nothing to stop yet: ${JSON.stringify(s.stops)}`);
  s.release();
  await played;
  expect(same(s.stops, [[1, 0]]), `stopped ${JSON.stringify(s.stops)}, expected [[1,0]]`);
  expect(!card.playing && deck.playing === null, "the card should be at rest");
});
await test("another card's Play stops the one playing: its runs, and nothing else", async (expect) => {
  const s = makeSession(), deck = createDeck(s.hooks), a = deck.add(makeCard("a")), b = deck.add(makeCard("b"));
  await a.onRun();
  await b.onRun();
  expect(same(s.stops, [[1, FADE]]), `stopped ${JSON.stringify(s.stops)}, expected a's run alone`);
  expect(!a.playing && b.playing && deck.owns(2) && !deck.owns(1), "b plays, a rests");
});
await test("a card is over when none of its runs is live, and playing while any is", async (expect) => {
  const s = makeSession(), deck = createDeck(s.hooks), card = deck.add(makeCard("a"));
  await card.onRun();
  await card.onRun();
  deck.runs?.([9, 2]);
  expect(card.playing, "its second run is live: the card plays on");
  deck.runs?.([1]);
  expect(card.playing, "its first run is live: the card plays on");
  deck.runs?.([9]);
  expect(!card.playing && deck.playing === null, "none of its runs is live: the card is over");
  expect(same(s.stops, []), `a card that ends by itself stops nothing: ${JSON.stringify(s.stops)}`);
});
await test("a card whose run is still starting is not over, whatever is live", async (expect) => {
  const s = makeSession(), deck = createDeck(s.hooks), card = deck.add(makeCard("a"));
  s.held = true;
  const played = card.onRun();
  deck.runs?.([]);
  s.release();
  await played;
  expect(card.playing && deck.playing === 1, "the card should be playing the run that arrived");
});
await test("a run the editor takes over is let go, playing on: the card rests and stops nothing", async (expect) => {
  const s = makeSession(), deck = createDeck(s.hooks), card = deck.add(makeCard("a"));
  await card.onRun();
  deck.release(1);
  expect(!card.playing && deck.playing === null && same(s.stops, []), `released: stops ${JSON.stringify(s.stops)}`);
});

console.log(`${pass} pass, ${fail} fail`);
process.exit(fail ? 1 : 0);
