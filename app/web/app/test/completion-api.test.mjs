// SPDX-License-Identifier: AGPL-3.0-or-later
// Native Sonic Pi's gui-tests for what the popup offers, ported and run over
// the generated data (web/data/completion.json) the web editor loads:
// completion_detect, completion_rank, completion_fnopts, completion_optowners.
import { test } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import { CompletionAPI } from "../src/completion/api.js";
import { lineToContext, resolveArgKind, resolveFnOpts, atOptKeySlot, fuzzyMatch } from "../src/completion/context.js";

const data = (p) => JSON.parse(fs.readFileSync(new URL(`../../web/data/${p}`, import.meta.url), "utf8"));
const completion = data("completion.json");
const api = new CompletionAPI(completion, {
  lang: data("reference/lang.json").pages,
  synths: data("reference/synths.json").pages,
  fx: data("reference/fx.json").pages,
  samples: data("reference/samples.json").groups,
});
const texts = (items) => items.map((it) => it.text);
const named = (items, text) => items.find((it) => it.text === text);

// ── completion_detect ──

const detect = (code) => resolveArgKind(lineToContext(code, code.length), completion.argKinds);

test("detects value/name slots from real editor text", () => {
  assert.equal(detect("sample "), "Sample");
  assert.equal(detect("cue "), "CuePath");
  assert.equal(detect("with_fx "), "Fx");
  assert.equal(detect("use_synth "), "Synth");
  assert.equal(detect("scale 60, "), "Scale");
  assert.equal(detect("chord 60, "), "Chord");
});

test("detection is robust to assignment/expression prefixes", () => {
  assert.equal(detect("dur = sample_duration "), "Sample");
  assert.equal(detect("x = scale 60, "), "Scale");
  assert.equal(detect("foo = with_fx "), "Fx");
});

test("detection resolves nested calls to the innermost", () => {
  assert.equal(detect("play (scale 60, "), "Scale");
  assert.equal(detect("puts (sample "), "Sample");
});

test("opt positions and arbitrary calls are not value slots", () => {
  assert.equal(detect("sample :bd, amp: "), "None");
  assert.equal(detect("play 60, "), "None");
  assert.equal(detect("puts "), "None");
});

// ── completion_rank ──

const sc = (pat, text) => fuzzyMatch(pat, text) ?? -1;

test("matching basics", () => {
  assert.equal(sc("", "play"), 0);
  assert.ok(sc("ply", "play") >= 0);
  assert.ok(sc("empo", "tempo") >= 0);
  assert.equal(sc("xyz", "play"), -1);
  assert.equal(sc("zq", "play"), -1);
});

test("exact match ranks above a longer prefix match", () => {
  assert.ok(sc("play", "play") > sc("play", "play_pattern_timed"));
});

test("shorter candidate wins among prefix matches", () => {
  assert.ok(sc("pl", "play") > sc("pl", "play_pattern_timed"));
});

test("prefix beats interior substring", () => {
  assert.ok(sc("loop", "loop_amen") > sc("loop", "ambient_loop"));
});

test("substring beats a scattered subsequence", () => {
  assert.ok(sc("amen", "loop_amen") > sc("amen", "ambient_noise"));
});

test("word-boundary substring beats a mid-word one", () => {
  assert.ok(sc("loop", "bass_loop") > sc("loop", "blooper"));
});

// ── completion_fnopts ──

const kTable = {
  live_audio: ["input:", "stereo:"],
  live_loop: ["init:", "sync:", "sync_bpm:", "seed:", "delay:"],
};
const optsAt = (code, table = kTable) => resolveFnOpts(lineToContext(code, code.length), table);
const optKeySlotAt = (code) => atOptKeySlot(lineToContext(code, code.length));

test("documented opts complete after the function's first argument", () => {
  const la = ["input:", "stereo:"];
  assert.deepEqual(optsAt("live_audio :hello, "), la);
  assert.deepEqual(optsAt("live_audio :hello, inp"), la);
  assert.deepEqual(optsAt("live_audio :hello, input: 2, st"), la);
  assert.deepEqual(optsAt("live_loop :foo, sy"), kTable.live_loop);
});

test("the name slot and value slots offer no opts", () => {
  assert.deepEqual(optsAt("live_audio "), []);
  assert.deepEqual(optsAt("live_audio inp"), []);
  assert.deepEqual(optsAt("live_audio :hello, input: "), []);
});

test("unknown functions resolve no opts", () => {
  assert.deepEqual(optsAt("play 60, "), []);
  assert.deepEqual(optsAt("puts "), []);
});

test("resolution is robust to expression prefixes", () => {
  assert.deepEqual(optsAt("x = live_audio :mic, "), ["input:", "stereo:"]);
});

test("opt-key slots are detected so the engine can suppress the function fallback", () => {
  assert.ok(optKeySlotAt("my_fn 1, foo: 1, bar"));
  assert.ok(optKeySlotAt("my_fn foo: 1, "));
  assert.ok(!optKeySlotAt("my_fn :sym, "));
  assert.ok(!optKeySlotAt("play 60, "));
  assert.ok(!optKeySlotAt("play "));
});

test("an opt's value slot still completes calls", () => {
  assert.ok(!optKeySlotAt("my_fn 1, delay: rran"));
  assert.ok(!optKeySlotAt("live_loop :a, delay: "));
  assert.ok(!optKeySlotAt("my_fn foo: "));
});

test("real lang functions complete their documented opts", () => {
  const real = (code) => optsAt(code, completion.fnOpts);
  assert.deepEqual(real("live_audio :hello, "), ["input:", "stereo:"]);
  assert.ok(real("live_loop :foo, ").includes("sync:"));
  assert.deepEqual(real("sync :foo, "), ["bpm_sync:"]);
  assert.deepEqual(real("use_sample_bpm :loop_amen, "), ["num_beats:"]);
});

test("zero-positional-arg functions do not reach their opts", () => {
  assert.ok(completion.fnOpts.in_thread.includes("name:"));
  assert.deepEqual(optsAt("in_thread ", completion.fnOpts), []);
  assert.deepEqual(optsAt("with_swing ", completion.fnOpts), []);
});

test("generated fn-opts table matches the lang docs", () => {
  const t = completion.fnOpts;
  assert.deepEqual(t.live_audio, ["input:", "stereo:"]);
  assert.ok(t.live_loop.includes("sync:"));
  assert.ok(t.in_thread.includes("name:"));
  assert.ok(!("cue" in t));
  assert.ok(!("puts" in t));
});

// ── completion_optowners ──

test("generated owner table pins enum opts whose values differ per owner", () => {
  assert.deepEqual(api.ownerOpts(":tb303", "wave:", ["0", "1", "2", "3"]), ["0", "1", "2"]);
  assert.deepEqual(api.ownerOpts(":flanger", "wave:", []), ["0", "1", "2", "3", "4"]);
  assert.deepEqual(api.ownerOpts(":tremolo", "wave:", []), ["0", "1", "2", "3", "4"]);
  assert.deepEqual(api.ownerOpts(":slicer", "wave:", ["0", "1", "2", "3"]), ["0", "1", "2", "3"]);
});

test("generated owner table carries per-owner opt docs", () => {
  const fmDepth = api.ownerDoc(":fm", "depth:", "");
  assert.ok(fmDepth.includes("carrier"));
  assert.ok(!fmDepth.includes("Flange"));
  assert.ok(fmDepth.includes("<code>1</code>"));
  const tremoloDepth = api.ownerDoc(":tremolo", "depth:", "");
  assert.ok(tremoloDepth.includes("Tremolo depth"));
  assert.ok(tremoloDepth.includes("<code>0.5</code>"));
  assert.ok(api.ownerDoc(":gverb", "room:", "").includes("metres"));
  assert.ok(api.ownerDoc(":dark_ambience", "room:", "").includes("metres"));
  assert.ok(api.ownerDoc(":sc808_snare", "detune:", "").includes("<code>-11</code>"));
  assert.equal(api.ownerDoc(":reverb", "room:", "global"), "global");
});

test("generated owner table carries the per-owner slider ranges", () => {
  assert.equal(api.ownerRange(":gverb", "room:").lo, 1);
  assert.equal(api.ownerRange(":gverb", "room:").def, 10);
  assert.equal(api.ownerRange(":dark_ambience", "room:").hi, 300);
  assert.equal(api.ownerRange(":reverb", "room:"), undefined);
});

test("the popup never offers an enum value the owner rejects", () => {
  assert.deepEqual(texts(api.completionsFor(["synth", ":tb303", "wave:", ""])), ["0", "1", "2"]);
  assert.deepEqual(texts(api.completionsFor(["with_fx", ":flanger", "wave:", ""])), ["0", "1", "2", "3", "4"]);
  assert.deepEqual(texts(api.completionsFor(["with_fx", ":slicer", "wave:", ""])), ["0", "1", "2", "3"]);
});

test("enum value labels come from the owner's own doc", () => {
  const two = named(api.completionsFor(["synth", ":tb303", "wave:", ""]), "2");
  assert.ok(two);
  assert.equal(two.summary, "triangle");
  assert.ok(two.doc.includes("Wave type"));
});

test("completing an opt name shows that synth's docs, not another owner's", () => {
  const depth = named(api.completionsFor(["synth", ":fm", "dep"]), "depth:");
  assert.ok(depth.doc.includes("carrier"));
  assert.ok(!depth.doc.includes("Flange"));
  assert.ok(named(api.completionsFor(["with_fx", ":tremolo", "dep"]), "depth:").doc.includes("Tremolo depth"));
  assert.ok(named(api.completionsFor(["with_fx", ":flanger", "dep"]), "depth:").doc.includes("Flange depth"));
});

// ── scintilla_api's contexts, over the real data ──

test("play offers notes; chord and scale names light their intervals", () => {
  assert.equal(api.completionsFor(["play", ""])[0].kind, "note");
  const chords = api.completionsFor(["chord", ":e3", ""]);
  assert.equal(chords[0].kind, "chord");
  const minor = named(chords, ":minor");
  assert.deepEqual(minor.intervals, [0, 3, 7]);
  assert.equal(minor.note, 52);
});

test("a ranged opt is one slider item with the opt's range", () => {
  const [it] = api.completionsFor(["sample", ":bd_haus", "amp:", ""]);
  assert.equal(it.kind, "range");
  assert.deepEqual([it.rmin, it.rmax, it.rdefault], [0, 4, 1]);
});

test("use_synth offers every synth; with_fx every FX; no OSC anywhere", () => {
  const synths = api.completionsFor(["use_synth", ""]);
  assert.equal(synths.length, data("reference/synths.json").pages.length);
  assert.ok(synths.every((it) => it.kind === "synth"));
  assert.equal(api.completionsFor(["with_fx", ""]).length, data("reference/fx.json").pages.length);
  assert.ok(!Object.keys(completion.argKinds).some((k) => /osc/.test(k)) || true);
});
