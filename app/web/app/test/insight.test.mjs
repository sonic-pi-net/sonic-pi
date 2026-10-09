// SPDX-License-Identifier: AGPL-3.0-or-later
// The Threads timeline, as a musician reads it: a thread named by whose it is
// and which, a lane per place in the program a thread is started from, a
// state that says when a thread has ended but still sounds, and a grid in the
// program's own beats.
import { test } from "node:test";
import assert from "node:assert/strict";
import { threadLabel, laneKey, threadState, beatTicks, easeWindow, foldsInto, flashAt, cueLabel, cueIsNamed, placeCueNames, endLanesAt, laneRetired } from "../src/insight.js";

// The threads of the conductor example (a live loop :song starting four
// in_threads a phrase): 0.0 is the run's main thread, 0.0.0 the loop, and its
// children count up for as long as it runs.
const known = new Map([
  ["0.0", { id: "0.0", name: "" }],
  ["0.0.0", { id: "0.0.0", name: "live_loop_song" }],
  ["0.0.0.256", { id: "0.0.0.256", name: "" }],
  ["0.0.0.256.1", { id: "0.0.0.256.1", name: "" }],
  ["0.0.1", { id: "0.0.1", name: "drums" }],
  ["0.0.1.4", { id: "0.0.1.4", name: "" }],
]);
const lookup = (id) => known.get(id) ?? null;

test("a named thread is called by its name, a live loop by its loop name", () => {
  assert.equal(threadLabel("0.0.0", "live_loop_song", lookup), ":song");
  assert.equal(threadLabel("0.0.1", "drums", lookup), ":drums");
});

test("a run's main thread is main, and a thread of it is main › its number", () => {
  assert.equal(threadLabel("0.0", "", lookup), "main");
  assert.equal(threadLabel("0.0.7", "", lookup), "main › 7");
});

test("a thread started inside a live loop is the loop's name and its number: no path, however deep the id", () => {
  assert.equal(threadLabel("0.0.0.256", "", lookup), ":song › 256");
  assert.equal(threadLabel("0.0.0.259", "", lookup), ":song › 259");
  assert.equal(threadLabel("0.0.1.4", "", lookup), ":drums › 4");
});

test("a thread started inside an unnamed thread shows that thread's number, not its lineage", () => {
  assert.equal(threadLabel("0.0.0.256.1", "", lookup), "256 › 1");
});

test("a lane is the place a thread was started from: the parent and the line, so the next phrase's thread takes the same lane", () => {
  const a = laneKey({ kind: "thread", event: "start", thread: "0.0.0.256", parent: "0.0.0", line: 111 });
  const b = laneKey({ kind: "thread", event: "start", thread: "0.0.0.260", parent: "0.0.0", line: 111 });
  const c = laneKey({ kind: "thread", event: "start", thread: "0.0.0.257", parent: "0.0.0", line: 144 });
  assert.equal(a, b);
  assert.notEqual(a, c);
  // a thread whose start was not seen (a run already going when the pane opened) is its own lane
  assert.equal(laneKey({ kind: "synth", thread: "0.0.0.256" }), "0.0.0.256");
});

test("a thread's state says when it has ended but its sounds have not", () => {
  const now = 100;
  const lane = { ended: 99, error: null, events: [{ time: 99.5, dur: 2 }] };
  assert.equal(threadState(lane, null, now), "ended · sounding");
  assert.equal(threadState({ ended: 90, error: null, events: [{ time: 90, dur: 1 }] }, null, now), "ended");
  assert.equal(threadState({ ended: null, error: null, events: [] }, { state: "sleeping", wake: 100.54 }, now), "sleeping 0.54s");
  assert.equal(threadState({ ended: null, error: null, events: [] }, { state: "waiting", on: ":tick" }, now), "sync :tick");
  assert.equal(threadState({ ended: null, error: null, events: [] }, null, now), "running");
  assert.equal(threadState({ ended: null, error: "Boom", events: [] }, null, now), "error");
});

test("the beat grid is anchored on a sleep the program made: beats from there at its tempo, bars every four", () => {
  // at 165 bpm a beat is 0.3636 s; a sleep 4 from beat 1024 at t=10 pins the grid
  const anchor = { time: 10, beat: 1024, bpm: 165 };
  const ticks = beatTicks(anchor, 9, 2);   // a 2 s window from t=9
  assert.ok(ticks.length >= 5 && ticks.length <= 6, String(ticks.length));
  const b1024 = ticks.find((t) => t.beat === 1024);
  assert.equal(b1024.time, 10);
  assert.equal(b1024.bar, true);
  assert.equal(ticks.find((t) => t.beat === 1025).bar, false);
  assert.ok(Math.abs(ticks.find((t) => t.beat === 1025).time - (10 + 60 / 165)) < 1e-9);
  assert.equal(ticks.find((t) => t.beat === 1023).time, 10 - 60 / 165);
  // no anchor, no grid
  assert.deepEqual(beatTicks(null, 9, 2), []);
});

test("a change of window eases over a few frames rather than jumping", () => {
  let w = 8;
  w = easeWindow(w, 16);
  assert.ok(w > 8 && w < 16);
  for (let i = 0; i < 60; i++) w = easeWindow(w, 16);
  assert.equal(w, 16);   // and settles exactly
});

test("a thread the runtime starts for itself folds into its parent's lane; a thread the program wrote does not", () => {
  assert.equal(foldsInto({ kind: "thread", event: "start", thread: "0.0.0.4.0", parent: "0.0.0.4", internal: true }), "0.0.0.4");
  assert.equal(foldsInto({ kind: "thread", event: "start", thread: "0.0.0.4", parent: "0.0.0", internal: false, line: 111 }), null);
  assert.equal(foldsInto({ kind: "synth", thread: "0.0.0.4.0" }), null);
});

test("a sound is lit at its onset and fades within 150 ms", () => {
  const e = { time: 10 };
  assert.equal(flashAt(e, 9.9), 0);
  assert.equal(flashAt(e, 10), 1);
  assert.ok(flashAt(e, 10.075) > 0.4 && flashAt(e, 10.075) < 0.6);
  assert.equal(flashAt(e, 10.2), 0);
  assert.equal(flashAt(e, null), 0);
});

test("a live loop's own cue is named as the loop; any other cue keeps its address", () => {
  assert.equal(cueLabel("/live_loop/tick"), ":tick");
  assert.equal(cueLabel("/drop"), "/drop");
});

// a monospace measure, as the canvas's 11px code font: 6 px a character
const mono = (text) => text.length * 6;

test("only a cue the program sends is named: not a loop's own each time round, nor one from no thread of the program", () => {
  assert.equal(cueIsNamed("/cue/chorus", true), true);
  assert.equal(cueIsNamed("/live_loop/kick", true), false);                 // its lane names it
  assert.equal(cueIsNamed("/clockwork/gamepad/devices", false), false);     // Sonic Pi's own, or from outside
});

test("a cue's name stands whole at its line, before the now line, or is left off", () => {
  const x = (t) => t * 100;   // 100 px a second
  const cues = [
    { time: 1, name: ":drop" },        // at 103
    { time: 1.2, name: ":verse" },     // 123: inside :drop's name (103 + 30 + 6), left off
    { time: 2, name: ":chorus" },      // 203
    { time: 4, name: ":outro" },       // 403: would cross the now line at 420, left off
    { time: 5, name: ":soon" },        // still to come, left off
  ];
  assert.deepEqual(placeCueNames(cues, x, 420, mono), [
    { x: 103, text: ":drop" }, { x: 123, text: null }, { x: 203, text: ":chorus" },
    { x: 403, text: null }, { x: 503, text: null },
  ]);
});

test("Stop ends every lane still going and drops the sounds it had scheduled past that moment", () => {
  const a = { job: 0, ended: null, events: [{ time: 9, dur: 1 }, { time: 11, dur: 1 }], waits: [{ from: 8, to: null }], sleeps: [{ from: 9, to: 12 }] };
  const b = { job: 0, ended: 7, events: [], waits: [], sleeps: [] };
  endLanesAt([a, b], 10);
  assert.equal(a.ended, 10);
  assert.deepEqual(a.events, [{ time: 9, dur: 1 }]);
  assert.equal(a.waits[0].to, 10);
  assert.equal(a.sleeps[0].to, 10);
  assert.equal(b.ended, 7);   // one already over keeps its own end
  endLanesAt([a], null);      // no clock: nothing to do
  assert.equal(a.ended, 10);
});

test("when a new run starts, an earlier run's lane leaves once it has ended and fallen silent, and a live lane of another run stays", () => {
  const now = 20;
  assert.equal(laneRetired({ job: 0, ended: 10, events: [{ time: 9, dur: 1 }] }, 1, now), true);
  assert.equal(laneRetired({ job: 0, ended: 19.9, events: [{ time: 19.5, dur: 2 }] }, 1, now), false);   // still sounding
  assert.equal(laneRetired({ job: 0, ended: null, events: [] }, 1, now), false);                        // a second run beside a first
  assert.equal(laneRetired({ job: 1, ended: 10, events: [] }, 1, now), false);                          // the new run's own
});
