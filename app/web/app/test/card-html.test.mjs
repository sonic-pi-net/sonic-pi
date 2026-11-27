// SPDX-License-Identifier: AGPL-3.0-or-later
// A card as the site's build writes it (app/src/ui/card-html.js).
import test from "node:test";
import assert from "node:assert/strict";
import { cardHTML, SHRUNK_LINES, SHRUNK_SLACK, sizeOf } from "../src/ui/card-html.js";

test("a card is written with Open in Sonic Pi, and without it for a page its reader is to stay on", () => {
  const usual = cardHTML({ title: "Beep", code: "play 60" });
  assert.match(usual, /qs-card-open/);
  for (const name of ["edit", "reset", "copy"]) assert.match(usual, new RegExp(`qs-card-${name}`));
  const stays = cardHTML({ title: "Beep", code: "play 60", open: false });
  assert.doesNotMatch(stays, /qs-card-open|Open in Sonic Pi/);
  for (const name of ["edit", "reset", "copy"]) assert.match(stays, new RegExp(`qs-card-${name}`), `${name} is still there`);
});

const lines = (n) => Array.from({ length: n }, (_, i) => `play ${60 + i}`).join("\n");

test("a card on a page keeps to the screen; a long one starts shrunk, with a button to grow it, and a short one has none", () => {
  const short = cardHTML({ title: "Short", code: lines(SHRUNK_LINES), sizes: true });
  assert.match(short, /class="qs-card sp-card qs-sizes"/, "it keeps to the screen");
  assert.doesNotMatch(short, /qs-shrunk|qs-card-size/, "all of it shows: nothing to grow");
  const long = cardHTML({ title: "Long", code: lines(SHRUNK_LINES + SHRUNK_SLACK + 1), sizes: true });
  assert.match(long, /qs-sizes qs-long qs-shrunk/);
  assert.match(long, /<\/div><button class="qs-card-size"[^>]*aria-expanded="false"[^>]*aria-label="Show all the code"[^>]*><svg[^]*?<\/svg><\/button><footer/, "a grip on the seam under the code, named for what it does, not an icon in the bar");
  assert.doesNotMatch(long.slice(long.indexOf("<header"), long.indexOf("</header>")), /qs-card-size/, "nothing for it in the bar");
  const plain = cardHTML({ title: "Long", code: lines(40) });
  assert.doesNotMatch(plain, /qs-sizes|qs-card-size/, "only where it is asked for");
});

test("whether a card's code is long enough to shrink, and how it stands", () => {
  assert.deepEqual(sizeOf(lines(3), null), { long: false, shrunk: false });
  assert.deepEqual(sizeOf(lines(SHRUNK_LINES + 1), null), { long: false, shrunk: false }, "a line or two over is shown whole: nothing is hidden to save so little");
  assert.deepEqual(sizeOf(lines(SHRUNK_LINES + SHRUNK_SLACK), null), { long: false, shrunk: false });
  assert.deepEqual(sizeOf(lines(SHRUNK_LINES + SHRUNK_SLACK + 1), null), { long: true, shrunk: true }, "long: shrunk until grown");
  assert.deepEqual(sizeOf(lines(30), "grown"), { long: true, shrunk: false }, "as it was left");
  assert.deepEqual(sizeOf(lines(30), "shrunk"), { long: true, shrunk: true });
  assert.deepEqual(sizeOf(lines(3) + "\n\n\n", "grown"), { long: false, shrunk: false }, "blank lines at the end are not code");
});
