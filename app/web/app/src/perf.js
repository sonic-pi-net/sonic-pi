// SPDX-License-Identifier: AGPL-3.0-or-later
// What the page's own drawing costs, per part, between samples of the flight
// recorder: the process tree, the timeline, the scope. The web build runs the
// language, its scheduler and the GUI on one thread, so a view that draws too
// long is a sound sent late.
const parts = new Map();

/** Adds one measurement (ms) to a part. */
export function perfAdd(name, ms) {
  let p = parts.get(name);
  if (!p) parts.set(name, (p = { count: 0, total: 0, max: 0 }));
  p.count++;
  p.total += ms;
  if (ms > p.max) p.max = ms;
}

/** Every part's {count, totalMs, maxMs} since the last call, and a fresh start. */
export function perfTake() {
  const out = {};
  for (const [name, p] of parts) out[name] = { count: p.count, totalMs: p.total, maxMs: p.max };
  parts.clear();
  return out;
}
