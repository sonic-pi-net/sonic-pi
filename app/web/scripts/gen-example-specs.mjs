#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// Writes specs/examples/<name>.rb for every example in web/examples.js, so
// the app's examples are held to Sonic Pi like any other spec. Examples
// are live music and never end on their own; each spec gets a horizon
// (`# horizon: N`), which both the oracle and the runtime honour: a thread
// whose sleep takes it past N seconds of logical time stops there.
//
//   node scripts/gen-example-specs.mjs        # then: ruby scripts/gen-expected.rb specs/examples
//   node scripts/gen-example-specs.mjs --check   # exit 1 if a spec differs from its example
//
// The horizon is off any beat grid on purpose (2.001, not 2), so float
// rounding can never decide whether a thread stops on the boundary.
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { EXAMPLES } from "../web/examples.js";

const ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const DIR = path.join(ROOT, "specs/examples");
const HORIZON = "2.001";
const check = process.argv.includes("--check");

const slug = (name) => name.toLowerCase().replace(/[^a-z0-9]+/g, "_").replace(/^_|_$/g, "");
const wanted = new Map(EXAMPLES.map((ex) => [
  `${slug(ex.name)}.rb`,
  `# the app's example "${ex.name}" (web/examples.js), recorded up to its horizon\n# horizon: ${HORIZON}\n${ex.src}`,
]));

fs.mkdirSync(DIR, { recursive: true });
let stale = 0;
for (const [file, src] of wanted) {
  const p = path.join(DIR, file);
  const have = fs.existsSync(p) ? fs.readFileSync(p, "utf8") : null;
  if (have === src) continue;
  stale++;
  if (check) console.log(`differs from its example: specs/examples/${file}`);
  else { fs.writeFileSync(p, src); console.log(`wrote specs/examples/${file}`); }
}
for (const f of fs.readdirSync(DIR)) {
  if (!f.endsWith(".rb") || wanted.has(f)) continue;
  stale++;
  if (check) console.log(`no longer an example: specs/examples/${f}`);
  else { fs.rmSync(path.join(DIR, f)); fs.rmSync(path.join(DIR, f.replace(/\.rb$/, ".expected.json")), { force: true }); console.log(`removed specs/examples/${f}`); }
}
if (check) process.exit(stale ? 1 : 0);
console.log(`${wanted.size} example specs${stale ? "" : ", all current"}`);
