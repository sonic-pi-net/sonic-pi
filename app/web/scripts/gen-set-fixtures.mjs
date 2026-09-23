#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// Writes app/test/fixtures/set-bundle.json: sets as native Sonic Pi writes and reads them, from native's own code
// (oracle/set-bundle/oracle.cpp over app/gui/utils/setbundle.cpp), for app/test/set-bundle.test.mjs to hold the
// web's set-bundle.js to. Rerun it when native's SetBundle changes. It needs Qt 6 (its QtCore) and clang++:
//
//   node scripts/gen-set-fixtures.mjs                      # Qt from ~/Qt/<newest>/macos
//   QT=/path/to/Qt/6.x/macos node scripts/gen-set-fixtures.mjs
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";

const ROOT = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const GUI = path.resolve(ROOT, "../gui/utils");
const OUT = path.join(ROOT, "app/test/fixtures/set-bundle.json");

const qt = process.env.QT ?? (() => {
  const base = path.join(os.homedir(), "Qt");
  const v = fs.existsSync(base) ? fs.readdirSync(base).filter((d) => /^\d+\.\d+/.test(d)).sort((a, b) => a.localeCompare(b, undefined, { numeric: true })).pop() : null;
  if (!v) throw new Error("no Qt found: set QT to Qt's macos (or gcc_64) directory");
  return path.join(base, v, "macos");
})();
const bin = path.join(os.tmpdir(), "sonic-pi-set-oracle");
execFileSync("clang++", ["-std=c++17", "-O1", `-F${qt}/lib`, `-I${qt}/lib/QtCore.framework/Headers`, `-I${GUI}`,
  path.join(ROOT, "oracle/set-bundle/oracle.cpp"), path.join(GUI, "setbundle.cpp"), "-framework", "QtCore", `-Wl,-rpath,${qt}/lib`, "-o", bin], { stdio: "inherit" });

// the cases: the edges by hand, then a seeded spread of buffers and files built from the lines that matter
const bits = ["play 60", "", " ", "\t", "#--", "#-- buffer 3", "#-- ~x", "#-- meta {}", "# comment", "live_loop :a do", "  sleep 1", "end", "héllo ♪", "\r",
  "#-- Sonic Pi Set v1", "#-- Sonic Pi Set v2", "  #-- not at start", "#--~"];
let seed = 7;
const rnd = (n) => (seed = (seed * 1103515245 + 12345) % 2147483648) % n;
const buffer = () => {
  if (rnd(3) === 0) return "";
  let s = Array.from({ length: rnd(6) }, () => bits[rnd(bits.length)]).join("\n");
  if (rnd(3) === 0) s += "\n";
  if (rnd(5) === 0) s += "\n\n";
  return s;
};
const metas = [{}, { names: ["drums", "bass"], web: { fontSize: 20, b: [1, { z: 1, a: 2 }] } }, { zebra: 1, alpha: "é♪", current: 99, zooms: [0] },
  { s: "quote \" backslash \\ tab \t nl \n ctl \u0001 del \u007f" },
  { n: 1.5, big: 1e21, small: 1e-7, tiny: 5e-324, huge: 1.7976931348623157e308, third: 0.3333333333333333, m: -1e-9, neg: -0.25, t: true, f: false, z: null, e: [], o: {} }];
const writes = [
  { buffers: [], current: 0, zooms: [] }, { buffers: ["a"], current: 9, zooms: [99, -99, 3] }, { buffers: ["\n\n"], current: 0, zooms: [] },
  { buffers: Array(10).fill("x\n"), current: 5, zooms: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] }, { buffers: Array(12).fill("y"), current: 0, zooms: [] },
  ...metas.map((meta) => ({ buffers: ["a", "b"], current: 1, zooms: [3], meta })),
  ...Array.from({ length: 100 }, () => ({ buffers: Array.from({ length: 10 }, buffer), current: rnd(10), zooms: Array.from({ length: rnd(12) }, () => rnd(40) - 10) })),
];
const reads = [
  "", "hello", "#-- Sonic Pi Set v1\n", "#-- buffer 0", "#-- buffer 0\n", "#-- buffer 10\nx", "#-- buffer 3\r\nplay 1\r\n", "#-- meta {\"current\":4}\n#-- buffer 1\nx",
  "#-- meta {\"current\":12,\"zooms\":[1.5,\"a\",30,-30]}\n#-- buffer 2\nx", "#-- meta [1]\n#-- buffer 0\na", "#-- meta nonsense\n#-- buffer 0\na", "#-- ~orphan\n#-- buffer 0\na",
  "#-- buffer 1\na\n#-- buffer 1\nb", "#-- buffer 01\nx", "#-- buffer -1\nx", "#-- buffer 2 \nx", "#-- meta {\"current\":2.0}\n#-- buffer 0\nx", "#-- buffer 0\n\n\n",
  "#-- Sonic Pi Set v2\n#-- buffer 0\nx", "#-- Sonic Pi Set v0\n#-- buffer 0\nx", "#-- Sonic Pi Set v10\n#-- buffer 0\nx", "#-- buffer 0\nx\n#-- Sonic Pi Set v2",
  "#-- Sonic Pi Set v01\n#-- buffer 0\nx", "#-- Sonic Pi Set v1 \n#-- buffer 0\nx", "#-- meta {}\n#-- Sonic Pi Set v3\n#-- buffer 0\nx",
  ...metas.map((m) => "#-- Sonic Pi Set v1\n#-- meta " + JSON.stringify(m) + "\n#-- buffer 0\nx"),
  ...Array.from({ length: 100 }, () => Array.from({ length: rnd(10) }, () => (rnd(3) ? bits[rnd(bits.length)] : `#-- buffer ${rnd(12)}`)).join(rnd(4) ? "\n" : "\r\n") + (rnd(2) ? "\n" : "")),
].map((text) => ({ text }));

const cases = [...writes, ...reads];
const native = JSON.parse(execFileSync(bin, { input: JSON.stringify(cases), maxBuffer: 1 << 28 }).toString());
// every set native wrote, read back by native too: the round trip is native's own
const trips = native.slice(0, writes.length).map((n) => ({ text: n.text }));
const back = JSON.parse(execFileSync(bin, { input: JSON.stringify(trips), maxBuffer: 1 << 28 }).toString());
const fixture = {
  about: "Sets as native Sonic Pi writes and reads them (scripts/gen-set-fixtures.mjs): regenerate, never edit",
  cases: [...cases.map((c, i) => ({ in: c, out: native[i] })), ...trips.map((c, i) => ({ in: c, out: back[i] }))],
};
fs.writeFileSync(OUT, `{"about":${JSON.stringify(fixture.about)},"cases":[\n${fixture.cases.map((c) => JSON.stringify(c)).join(",\n")}\n]}\n`);   // a case a line
console.log(`${fixture.cases.length} cases → ${path.relative(ROOT, OUT)}`);
