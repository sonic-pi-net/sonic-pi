// SPDX-License-Identifier: AGPL-3.0-or-later
// Native Sonic Pi's gui-tests/completion_context.test.cpp, ported: the web
// editor must read a line the way the native one does.
import { test } from "node:test";
import assert from "node:assert/strict";
import { scanLineToCaret, tokenEndAtCaret, lineToContext, caretAfterClosedValue, resolveArgKind, resolveFnOpts, atOptKeySlot, fuzzyMatch } from "../src/completion/context.js";

const partialAt = (line, caret) => { const ctx = lineToContext(line, caret); return ctx.length ? ctx[ctx.length - 1] : ""; };

test("plain code is neither string nor comment", () => {
  const s = scanLineToCaret("play 60", 7);
  assert.equal(s.inString, false);
  assert.equal(s.inComment, false);
  assert.equal(s.bracketDepth, 0);
  assert.equal(s.openQuoteCol, -1);
});

test("caret inside an unterminated double-quoted string", () => {
  const line = 'cue "/link';
  const s = scanLineToCaret(line, line.length);
  assert.equal(s.inString, true);
  assert.equal(s.quote, '"');
  assert.equal(s.openQuoteCol, 4);
  assert.equal(s.inComment, false);
});

test("single-quoted string is tracked too", () => {
  const line = "sample 'loop";
  const s = scanLineToCaret(line, line.length);
  assert.equal(s.inString, true);
  assert.equal(s.quote, "'");
});

test("a closed string leaves the caret back in code", () => {
  const line = 'cue "x" ';
  const s = scanLineToCaret(line, line.length);
  assert.equal(s.inString, false);
  assert.equal(s.openQuoteCol, -1);
});

test("escaped quote does NOT close the string", () => {
  const line = 'cue "a\\"b';
  const s = scanLineToCaret(line, line.length);
  assert.equal(s.inString, true);
  assert.equal(s.quote, '"');
  assert.equal(s.openQuoteCol, 4);
});

test("a '#' outside a string starts a comment", () => {
  const line = "play 60 # foo";
  const s = scanLineToCaret(line, line.length);
  assert.equal(s.inComment, true);
  assert.equal(s.inString, false);
});

test("a '#' INSIDE a string is not a comment", () => {
  const line = 'cue "a#b';
  const s = scanLineToCaret(line, line.length);
  assert.equal(s.inString, true);
  assert.equal(s.inComment, false);
});

test("unclosed bracket increases depth; balanced returns to zero", () => {
  assert.equal(scanLineToCaret("play (scale 60,", 15).bracketDepth, 1);
  assert.equal(scanLineToCaret("foo(bar)", 8).bracketDepth, 0);
  assert.equal(scanLineToCaret("a([{", 4).bracketDepth, 3);
});

test("brackets inside a string do not count", () => {
  const line = 'cue "a(b[c';
  const s = scanLineToCaret(line, line.length);
  assert.equal(s.inString, true);
  assert.equal(s.bracketDepth, 0);
});

test("scan stops at the caret column, ignoring text after it", () => {
  assert.equal(scanLineToCaret('cue "x" play', 6).inString, true);
});

test("tokenEndAtCaret extends over the token around the caret", () => {
  assert.equal(tokenEndAtCaret("lpf: 70", 6), 7);
  assert.equal(tokenEndAtCaret("lpf: 70", 5), 7);
  assert.equal(tokenEndAtCaret("sample :ambi_choir", 11), 18);
});

test("tokenEndAtCaret stops at separators, including the newline", () => {
  assert.equal(tokenEndAtCaret("pan: \n", 5), 5);
  assert.equal(tokenEndAtCaret("pan: 0.5\n", 8), 8);
  assert.equal(tokenEndAtCaret("pan: \r\n", 5), 5);
  assert.equal(tokenEndAtCaret("lpf: 70,pan", 7), 7);
});

test("partial is the whole token around a mid-token caret", () => {
  assert.equal(partialAt("lpf: 70", 6), "70");
  assert.equal(partialAt("sample :ambi_choir", 11), ":ambi_choir");
});

test("end-of-line partial is empty despite the trailing newline", () => {
  assert.equal(partialAt("sample :ambi_choir, pan: \n", 25), "");
  assert.equal(partialAt("pan: \n", 5), "");
  assert.equal(partialAt("sample :ambi_choir, pan: 0.5\n", 28), "0.5");
});

test("a string is one token, spaces and all", () => {
  const typing = 'track_control "Filter 1 Cut';
  assert.deepEqual(lineToContext(typing, typing.length), ["track_control", '"Filter 1 Cut']);
  const done = 'track_control "Filter 1 Cutoff", ';
  assert.deepEqual(lineToContext(done, done.length), ["track_control", '"Filter 1 Cutoff"', ""]);
  const esc = 'midi_note_on 60, port: "say \\"hi\\" (loud)", ';
  assert.deepEqual(lineToContext(esc, esc.length), ["midi_note_on", "60", "port:", '"say \\"hi\\" (loud)"', ""]);
  const single = "puts 'a b', ";
  assert.deepEqual(lineToContext(single, single.length), ["puts", "'a b'", ""]);
});

test("caret directly after a closing bracket/quote suppresses completion", () => {
  const line = "control s, phase_offset: rand(1)";
  assert.equal(caretAfterClosedValue(line, line.length), true);
  assert.equal(caretAfterClosedValue("play [60, 64]", 13), true);
  assert.equal(caretAfterClosedValue("puts({a: 1})", 12), true);
  assert.equal(caretAfterClosedValue('play "foo"', 10), true);
  assert.equal(caretAfterClosedValue("play 'foo'", 10), true);
});

test("ordinary positions do not read as a closed value", () => {
  assert.equal(caretAfterClosedValue("", 0), false);
  assert.equal(caretAfterClosedValue("synth :sine, ", 13), false);
  assert.equal(caretAfterClosedValue("control s, phase_offset: rand(1) ", 33), false);
});

test("nested calls resolve to the innermost", () => {
  assert.deepEqual(lineToContext("play (scale ", 12), ["scale", ""]);
  assert.deepEqual(lineToContext("play scale(60, ", 15), ["scale", "60", ""]);
});

test("a statement modifier starts a fresh expression", () => {
  assert.deepEqual(lineToContext("play 60 if one_in ", 18), ["one_in", ""]);
});

test("arg kinds resolve by position from the nearest function", () => {
  const table = { sample: ["Sample"], use_synth: ["Synth"], chord: ["Note", "Chord"] };
  assert.equal(resolveArgKind(["sample", ""], table), "Sample");
  assert.equal(resolveArgKind(["use_synth", ":sa"], table), "Synth");
  assert.equal(resolveArgKind(["chord", ":e3", ""], table), "Chord");
  assert.equal(resolveArgKind(["dur", "=", "sample", ""], table), "Sample");
  assert.equal(resolveArgKind(["sample", ":bd_haus", "amp:", ""], table), "None");
});

test("fn opts are offered past the first argument, not in a value slot", () => {
  const table = { live_loop: ["sync:", "delay:"] };
  assert.deepEqual(resolveFnOpts(["live_loop", ""], table), []);
  assert.deepEqual(resolveFnOpts(["live_loop", ":foo", ""], table), ["sync:", "delay:"]);
  assert.deepEqual(resolveFnOpts(["live_loop", ":foo", "sync:", ""], table), []);
});

test("an opt key slot follows an opt and its value", () => {
  assert.equal(atOptKeySlot(["play", "60", "amp:", "1", ""]), true);
  assert.equal(atOptKeySlot(["play", "60", "amp:", ""]), false);
  assert.equal(atOptKeySlot(["play", "60", ""]), false);
});

test("fuzzy ranking: exact > prefix > substring > scattered, shorter wins ties", () => {
  const s = (p, t) => fuzzyMatch(p, t);
  assert.equal(s("xyz", "tempo"), null);
  assert.ok(s("empo", "tempo") !== null);
  assert.ok(s("play", "play") > s("play", "play_pattern"));
  assert.ok(s("pla", "play") > s("pla", "sample"));
  assert.ok(s("amp", "amp:") > s("amp", "pre_amp:"));
  assert.ok(s("", "anything") === 0);
});
