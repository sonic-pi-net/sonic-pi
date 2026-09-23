// SPDX-License-Identifier: AGPL-3.0-or-later
// The instrument rack's layout: where its modules break into rows, and how many knobs each module puts on a row.
//
// A wrapping flex row packs greedily — it fills a line until something does not fit and drops it to the next — which
// leaves a module stranded on a line of its own under a half-empty one. This is the same job a paragraph's line
// breaker does, and the same answer: look at every way of breaking the modules into rows and take the one whose rows
// are least ragged (Knuth's minimum raggedness, as a small dynamic program — a card has a handful of modules, so the
// naive O(n²) is instant).
//
// What makes it a balancer rather than a line breaker is that a module has more than one width: its knobs can sit in
// one row of five, or two of three, or three of two, each shorter and wider than the last. The plan chooses that as
// well, so a module folds its knobs only when folding is what lets the rack sit evenly — no hand-written rules about
// which module is allowed how many columns.
//
// planRows is pure and takes measurements, so it is tested on its own (test/rack.test.mjs); packRack does the
// measuring, applies the plan and is the only part that touches the DOM.

/** A row break in the flex row: an element of no height that fills the line, so what follows starts a new one. */
const BREAK = "pg-break";

// What the plan is scored on, in square pixels of slack: a row that ends short costs the square of what it leaves
// (so two half-empty rows cost more than one nearly-full one), and a module that folds its knobs onto another line
// costs FOLD, which is about what leaving 55px of a row costs. The last row is free to end short, as a paragraph's is.
const FOLD = 3000;

/**
 * Where to break the modules into rows, and how many knob columns each takes.
 * @param mods  [{ candidates: [{ cols, width }] }] — each module's widths, widest first
 * @param width the room the rack has
 * @param gap   the gap between modules
 * @returns [[{ index, cols }]] — the modules of each row, in order
 */
export function planRows(mods, width, gap = 8) {
  const n = mods.length;
  if (!n) return [];
  const best = new Array(n + 1).fill(Infinity), from = new Array(n + 1).fill(0), taken = new Array(n + 1).fill(null);
  best[0] = 0;
  for (let j = 0; j < n; j++) {
    for (let i = j; i >= 0; i--) {
      if (best[i] === Infinity) continue;
      const row = fitRow(mods, i, j, width, gap);
      if (!row) break;                      // this row is already too wide: starting it earlier only makes it wider
      const last = j === n - 1;
      const cost = best[i] + (last ? 0 : row.slack * row.slack) + row.folds * FOLD;
      if (cost < best[j + 1]) { best[j + 1] = cost; from[j + 1] = i; taken[j + 1] = row.pick; }
    }
  }
  if (best[n] === Infinity) return greedy(mods, width, gap);   // a module too wide for the room at any width: fill rows in order and give it one of its own
  const rows = [];
  for (let j = n; j > 0; j = from[j]) {
    const i = from[j];
    rows.unshift(taken[j].map((c, k) => ({ index: i + k, cols: mods[i + k].candidates[c].cols })));
  }
  return rows;
}

// When no plan fits — a module wider than the whole rack, whatever it folds to — the rows are filled in order, and
// a module that fits nowhere takes a row to itself rather than dragging the rest off the edge with it.
function greedy(mods, width, gap) {
  const rows = [];
  let line = [], used = 0;
  mods.forEach((m, index) => {
    const narrow = m.candidates[m.candidates.length - 1];
    const wide = m.candidates[0];
    const room = width - used - (line.length ? gap : 0);
    const cand = wide.width <= room ? wide : narrow.width <= room ? narrow : null;
    if (!cand) { if (line.length) { rows.push(line); line = []; used = 0; } rows.push([{ index, cols: narrow.cols }]); return; }
    used += cand.width + (line.length ? gap : 0);
    line.push({ index, cols: cand.cols });
  });
  if (line.length) rows.push(line);
  return rows;
}

// One row's modules, each as wide as it can be: start them all at their widest and fold the widest one again and
// again until the row fits, so what gives way is whatever is taking the most room.
function fitRow(mods, i, j, width, gap) {
  const pick = [];
  for (let k = i; k <= j; k++) pick.push(0);
  const total = () => pick.reduce((sum, c, k) => sum + mods[i + k].candidates[c].width, 0) + gap * (j - i);
  while (total() > width) {
    let widest = -1, widestAt = -1;
    for (let k = 0; k < pick.length; k++) {
      const m = mods[i + k];
      if (pick[k] + 1 >= m.candidates.length) continue;     // nothing narrower to fold to
      const w = m.candidates[pick[k]].width;
      if (w > widest) { widest = w; widestAt = k; }
    }
    if (widestAt < 0) return null;                          // every module is as narrow as it goes and it still overruns
    pick[widestAt]++;
  }
  return { pick, slack: width - total(), folds: pick.reduce((a, b) => a + b, 0) };
}

/** The knob counts a module can lay its dials out in, widest first: one row of them, two, then three. */
export function colsFor(dials) {
  if (dials <= 1) return [Math.max(1, dials)];
  return [...new Set([dials, Math.ceil(dials / 2), Math.ceil(dials / 3)])];
}

/**
 * Measures the rack's modules, plans its rows and applies the plan: each module's knob columns set, and a break
 * before every module that starts a row. Returns the rows as planned, or null when there is nothing to lay out.
 */
export function packRack(row, { gap = 8, width = null } = {}) {
  if (!row || !row.isConnected) return null;
  for (const b of [...row.children]) if (b.classList.contains(BREAK)) b.remove();
  const mods = [...row.children].filter((el) => !el.hidden && el.offsetParent !== null);
  // The room to plan into is the caller's to say: the rack shrinks to fit what is in it, so measuring the rack would
  // measure the last plan and fold the modules tighter and tighter each time it ran.
  width = width ?? row.clientWidth;
  if (!mods.length || width <= 0) return null;

  // every width each module can take, measured: the module is kept from growing while it is measured, so what is
  // read is what it wants to be rather than the share of the row flex has given it
  const measured = mods.map((el) => {
    // --cols belongs to the row of dials, which carries its own: setting it on the module would be inherited and then
    // overruled by that one, and every width would come back the same
    const grid = el.querySelector(".pg-dials");
    const dials = grid ? grid.querySelectorAll(":scope > .dial:not([hidden])").length : 0;
    const was = { flex: el.style.flex, cols: grid?.style.getPropertyValue("--cols") };
    el.style.flex = "0 0 auto";
    const candidates = colsFor(dials).map((cols) => {
      if (grid) grid.style.setProperty("--cols", String(cols));
      return { cols, width: el.offsetWidth };
    });
    el.style.flex = was.flex;
    if (grid) { if (was.cols) grid.style.setProperty("--cols", was.cols); else grid.style.removeProperty("--cols"); }
    return { el, grid, candidates };
  });

  const rows = planRows(measured, width, gap);
  for (const [r, line] of rows.entries()) {
    for (const { index, cols } of line) {
      const { el, grid } = measured[index];
      if (grid) grid.style.setProperty("--cols", String(cols));
      el.style.flex = "0 1 auto";                      // as wide as the plan measured it: growing them would spread
                                                       // the slack the plan was scored on, and hide why it folded
    }
    if (r > 0) row.insertBefore(Object.assign(document.createElement("div"), { className: BREAK }), measured[line[0].index].el);
  }
  return rows;
}
