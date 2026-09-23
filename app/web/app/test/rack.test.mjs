// SPDX-License-Identifier: AGPL-3.0-or-later
// The rack's row planner (app/src/rack.js): the part that decides where a rack's modules break into rows and how
// many knob columns each takes. It is given measurements, so it is tested with them.
import { test } from "node:test";
import assert from "node:assert/strict";
import { planRows, colsFor } from "../src/rack.js";

// a module of `n` knobs: its widths, widest first, as the dial grid gives them (68px a column, 20px of box)
const mod = (n) => ({ candidates: colsFor(n).map((cols) => ({ cols, width: 20 + cols * 68 })) });
// a module of one width, whatever it holds (In, a row of chips)
const fixed = (width) => ({ candidates: [{ cols: 1, width }] });
const widths = (rows, mods) => rows.map((line) => line.reduce((sum, { index, cols }) => sum + mods[index].candidates.find((c) => c.cols === cols).width, 0) + 8 * (line.length - 1));

test("modules that fit stay on one row", () => {
  const mods = [mod(2), mod(3), mod(2)];
  const rows = planRows(mods, 800, 8);
  assert.equal(rows.length, 1);
  assert.deepEqual(rows[0].map((m) => m.index), [0, 1, 2]);
});

test("a row is broken where the rows come out even, not where the last module happens to overflow", () => {
  // four modules of 156px: greedy fills 4 to a 700px row and strands the fifth; even rows are 3 and 2
  const mods = [mod(2), mod(2), mod(2), mod(2), mod(2)];
  const rows = planRows(mods, 500, 8);
  assert.deepEqual(rows.map((r) => r.length), [3, 2]);
});

test("a module folds its knobs onto a second line when that is what makes the row fit", () => {
  const mods = [fixed(300), mod(4)];          // 300 + 292 + 8 > 560, so the knobs fold to 2 × 2
  const rows = planRows(mods, 560, 8);
  assert.equal(rows.length, 1);
  assert.equal(rows[0][1].cols, 2);
  assert.ok(widths(rows, mods)[0] <= 560);
});

test("it would rather break the row than fold every module's knobs", () => {
  const mods = [mod(4), mod(4), mod(4)];
  const rows = planRows(mods, 640, 8);
  assert.ok(rows.length > 1, "two rows of unfolded knobs beats one row of folded ones");
  for (const line of rows) for (const { cols } of line) assert.equal(cols, 4);
});

test("the last row may end short for nothing", () => {
  const mods = [mod(3), mod(3), fixed(200)];
  const rows = planRows(mods, 460, 8);
  assert.deepEqual(rows.map((r) => r.map((m) => m.index)), [[0, 1], [2]]);
});

test("no row is wider than the room it has, unless one module alone is", () => {
  for (const width of [240, 380, 500, 760, 1200]) {
    const mods = [fixed(230), mod(3), mod(5), mod(4), fixed(340)];
    const rows = planRows(mods, width, 8);
    widths(rows, mods).forEach((line, i) => assert.ok(line <= width || rows[i].length === 1, `${line} > ${width} with ${rows[i].length} modules`));
  }
});

test("a module wider than the whole rack takes a row to itself", () => {
  const mods = [fixed(200), fixed(400), fixed(200)];
  const rows = planRows(mods, 300, 8);
  assert.deepEqual(rows.map((r) => r.map((m) => m.index)), [[0], [1], [2]]);
});

test("colsFor gives a module its knobs in one row, two, then three", () => {
  assert.deepEqual(colsFor(5), [5, 3, 2]);
  assert.deepEqual(colsFor(4), [4, 2]);
  assert.deepEqual(colsFor(2), [2, 1]);
  assert.deepEqual(colsFor(1), [1]);
  assert.deepEqual(colsFor(0), [1]);
});
