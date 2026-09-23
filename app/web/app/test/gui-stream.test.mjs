// SPDX-License-Identifier: AGPL-3.0-or-later
// The GUI stream's messages, read back into records: a thread named once by
// its start, and each kind's keys in a trace's order (live-check compares
// records as JSON, so the order is part of the record).
import { test } from "node:test";
import assert from "node:assert/strict";
import { createRecordReader } from "../../web/gui-stream.js";

test("a thread is named by its start, and its records carry its id and name after", () => {
  const r = createRecordReader();
  const start = r.read(["/sonic-pi/thread_start", 3, 100.5, 0, 2, 0, 0, "0.0.1", "live_loop_kick", "0.0"]);
  assert.equal(JSON.stringify(start), '{"kind":"thread","t":0,"beat":0,"thread":"0.0.1","name":"live_loop_kick","event":"start","parent":"0.0","time":100.5,"job":0,"line":2}');
  const synth = r.read(["/sonic-pi/synth", 3, 100.75, 0, 4, 0.25, 0.5, "sonic-pi-beep", false, 7, ["note", 60, "release", 0.2]]);
  assert.equal(JSON.stringify(synth), '{"kind":"synth","t":0.25,"beat":0.5,"thread":"0.0.1","name":"live_loop_kick","synth":"sonic-pi-beep","args":{"note":60,"release":0.2},"now":false,"time":100.75,"job":0,"node":7,"line":4}');
  assert.deepEqual(r.thread(3), { id: "0.0.1", name: "live_loop_kick", parent: "0.0", on: null, ended: null });
});

test("no line leaves no line key; an error's line is always there, even -1", () => {
  const r = createRecordReader();
  r.read(["/sonic-pi/thread_start", 1, 1, 0, null, 0, 0, "0.0", null, "0"]);
  assert.equal(JSON.stringify(r.read(["/sonic-pi/output", 1, 1, 0, null, 0, "hi"])), '{"kind":"output","t":0,"thread":"0.0","name":"","text":"hi","time":1,"job":0}');
  assert.equal(JSON.stringify(r.read(["/sonic-pi/error", 1, 1, 0, -1, "RuntimeError", "oops"])), '{"class":"RuntimeError","message":"oops","line":-1,"thread":"0.0","name":"","kind":"error","time":1,"job":0}');
});

test("a sync remembers what it waits on; a studio record keeps its nested opts", () => {
  const r = createRecordReader();
  r.read(["/sonic-pi/thread_start", 2, 1, 0, null, 0, 0, "0.0", null, "0"]);
  r.read(["/sonic-pi/sync", 2, 1, 0, 3, 0, 0, ["/cue/tick"]]);
  assert.equal(r.thread(2).on, "/cue/tick");
  const studio = r.read(["/sonic-pi/record", 2, 1, 0, 5, 0, 0, ["kind", "studio", "op", "mixer", "value", ["amp", 0.5]]]);
  assert.equal(JSON.stringify(studio), '{"kind":"studio","t":0,"beat":0,"thread":"0.0","name":"","op":"mixer","value":{"amp":0.5},"time":1,"job":0,"line":5}');
});
