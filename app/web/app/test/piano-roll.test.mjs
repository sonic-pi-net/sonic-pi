// SPDX-License-Identifier: AGPL-3.0-or-later
// The piano roll's model: notes from live records, as long as their
// envelopes at their thread's tempo, bent by slides, cut by kills; MIDI
// note_on and note_off paired; the rows and beats it draws.
import { test } from "node:test";
import assert from "node:assert/strict";
import { createRollModel, noteName, pitchRange, holdRange, beatGrid, isBlack } from "../src/piano-roll.js";

const BEEP = { note: 52, amp: 1, attack: 0, decay: 0, sustain: 0, release: 1, note_slide: 0 };
const model = () => createRollModel({ defaults: (s) => (s === "sonic-pi-beep" ? BEEP : null) });
const synth = (time, args, extra = {}) => ({ kind: "synth", thread: "0.0", name: "", job: 0, t: time - 100, beat: 0, synth: "sonic-pi-beep", args, time, node: 1, line: 2, ...extra });
const near = (a, b) => assert.ok(Math.abs(a - b) < 1e-9, `${a} ≠ ${b}`);

test("notes are named as musicians name them", () => {
  assert.equal(noteName(60), "C4");
  assert.equal(noteName(61), "C♯4");
  assert.equal(noteName(21), "A0");
  assert.equal(noteName(60.25), "C4+25¢");
  assert.ok(isBlack(61) && isBlack(70) && !isBlack(64) && !isBlack(59));
});

test("a note lasts its given envelope, in seconds as the record carries it", () => {
  const m = model();
  m.record(synth(100, { note: 60, attack: 0.1, sustain: 0.2, release: 0.3 }));
  const s = m.span(m.notes()[0]);
  near(s.hold, 100.3);
  near(s.end, 100.6);
});

test("the synth's default envelope is in beats, at the tempo the thread's next sleep tells", () => {
  const m = model();
  m.record(synth(100, { note: 60 }));
  near(m.span(m.notes()[0]).end, 101);                // no tempo known yet: 60 bpm
  m.record({ kind: "sleep", thread: "0.0", name: "", job: 0, t: 0, beat: 0.5, beats: 0.5, until: 0.25, time: 100 });
  near(m.span(m.notes()[0]).end, 100.5);              // 120 bpm: a beat's release is half a second
});

test("a note without a note arg plays its synth's default note", () => {
  const m = model();
  m.record(synth(100, {}));
  assert.equal(m.notes()[0].segs[0].note, 52);
});

test("control bends a note to its new pitch over the slide, and kill cuts it short", () => {
  const m = model();
  m.record(synth(100, { note: 60, release: 2, note_slide: 0.25 }));
  m.record({ kind: "control", thread: "0.0", name: "", job: 0, time: 100.5, node: 1, args: { note: 64 } });
  m.record({ kind: "kill", thread: "0.0", name: "", job: 0, time: 101, node: 1 });
  const n = m.notes()[0];
  const s = m.span(n);
  near(s.end, 101);
  assert.deepEqual(m.path(n, s.end), [{ t: 100, n: 60 }, { t: 100.5, n: 60 }, { t: 100.75, n: 64 }, { t: 101, n: 64 }]);
});

test("a note that ends mid-slide ends part way to its new pitch", () => {
  const m = model();
  m.record(synth(100, { note: 60, release: 1, note_slide: 1 }));
  m.record({ kind: "control", thread: "0.0", name: "", job: 0, time: 100.5, node: 1, args: { note: 64 } });
  const n = m.notes()[0];
  const pts = m.path(n, m.span(n).end);
  assert.deepEqual(pts[pts.length - 1], { t: 101, n: 62 });
});

test("Stop drops what was scheduled after it and ends what was sounding", () => {
  const m = model();
  m.record(synth(100, { note: 60, release: 2 }));
  m.record(synth(100.5, { note: 62, release: 1 }, { node: 2 }));
  m.record(synth(100.5, { buf: "bd_haus.flac" }, { synth: "sonic-pi-basic_stereo_player" }));
  m.stop(100.25);
  assert.deepEqual(m.notes().map((n) => n.segs[0].note), [60]);
  near(m.span(m.notes()[0]).end, 100.25);
  assert.equal(m.hits().length, 0);
});

test("a sample's first sound, from the helper thread that loads it, belongs to the loop that asked", () => {
  const m = model();
  const rec = (r) => m.record({ job: 0, time: 100, ...r });
  rec({ kind: "thread", event: "start", thread: "0.0.0", name: "live_loop_drums", parent: "0.0" });
  rec({ kind: "thread", event: "start", thread: "0.0.0.0", name: "", parent: "0.0.0" });
  rec({ kind: "sample_load", thread: "0.0.0.0", name: "" });
  rec({ kind: "synth", thread: "0.0.0.0", name: "", synth: "sonic-pi-basic_stereo_player", args: { buf: "bd_haus.flac" } });
  rec({ kind: "thread", event: "end", thread: "0.0.0.0", name: "" });
  rec({ kind: "synth", thread: "0.0.0", name: "live_loop_drums", synth: "sonic-pi-basic_stereo_player", args: { buf: "bd_haus.flac" }, time: 100.5 });
  assert.deepEqual(m.hits().map((h) => [h.thread, h.name]), [["0.0.0", "live_loop_drums"], ["0.0.0", "live_loop_drums"]]);
});

test("samples are hits on their own rows, not notes", () => {
  const m = model();
  m.record(synth(100, { buf: "bd_haus.flac", amp: 0.5 }, { synth: "sonic-pi-basic_stereo_player" }));
  assert.equal(m.notes().length, 0);
  assert.deepEqual(m.hits().map((h) => [h.sample, h.amp]), [["bd_haus", 0.5]]);
});

test("MIDI note_on lasts until its note_off on the same channel and note", () => {
  const m = model();
  const midi = (time, path, args) => ({ kind: "midi", thread: "0.0", name: "", job: 0, time, path, args });
  m.record(midi(100, "/note_on", ["*", 1, 60, 100]));
  m.record(midi(100, "/note_on", ["*", 2, 60, 100]));
  near(m.span(m.notes()[0], 100.4).end, 100.4);       // still held: it reaches now
  m.record(midi(100.5, "/note_off", ["*", 1, 60, 0]));
  near(m.span(m.notes()[0], 102).end, 100.5);
  assert.equal(m.notes()[1].off, null);
});

test("the rows cover the notes with room around them, at least an octave and a half", () => {
  assert.deepEqual(pitchRange([60, 64]), [53, 71]);
  assert.deepEqual(pitchRange([30, 90]), [28, 92]);
  assert.deepEqual(pitchRange([1]), [0, 18]);
  assert.deepEqual(pitchRange([]), [48, 72]);
});

test("the beat grid falls on a thread's whole beats, with a bar every four", () => {
  const lines = beatGrid({ time: 10, beat: 2.5, bpm: 120 }, 10, 12);
  assert.deepEqual(lines.map((l) => [l.time, l.beat, l.bar]), [[10.25, 3, false], [10.75, 4, true], [11.25, 5, false], [11.75, 6, false]]);
  assert.deepEqual(beatGrid(null, 0, 1), []);
});

test("a sound of a stopped job that arrives after the Stop, due after it, is not drawn: it will never sound", () => {
  const m = model();
  m.record(synth(100, { note: 60 }, { job: 1, node: 1 }));
  m.stop(100.2);
  m.record(synth(100.5, { note: 62 }, { job: 1, node: 2 }));   // sent before the Stop, arrived after it
  m.record(synth(100.6, { note: 64 }, { job: 2, node: 3 }));   // a run started after the Stop
  assert.deepEqual(m.notes().map((n) => n.segs[0].note), [60, 64]);
});

test("the roll's range grows at once and shrinks only after the room has gone unused a while", () => {
  let h = holdRange(null, [50, 70], 0);
  h = holdRange(h, [40, 70], 1);   // a low note: in at once
  assert.deepEqual([h.lo, h.hi], [40, 70]);
  h = holdRange(h, [50, 70], 2);   // it has scrolled off: held
  assert.deepEqual([h.lo, h.hi], [40, 70]);
  h = holdRange(h, [50, 70], 6.5);
  assert.deepEqual([h.lo, h.hi], [40, 70]);
  h = holdRange(h, [50, 70], 7.5);   // unused for longer than the hold: let go
  assert.deepEqual([h.lo, h.hi], [50, 70]);
  h = holdRange(h, [50, 80], 8);   // a high note: in at once
  assert.equal(h.hi, 80);
});
