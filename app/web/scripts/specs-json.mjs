// SPDX-License-Identifier: AGPL-3.0-or-later
// Every spec as one JSON document: verb, name, title, program and expected
// trace. Shared by the static build and the dev server.
import fs from "node:fs";
import path from "node:path";

export function listSpecs(ROOT) {
  const out = [];
  const specsDir = path.join(ROOT, "specs");
  for (const verb of fs.readdirSync(specsDir).sort()) {
    const dir = path.join(specsDir, verb);
    if (!fs.statSync(dir).isDirectory()) continue;
    for (const f of fs.readdirSync(dir).sort()) {
      if (!f.endsWith(".rb")) continue;
      const source = fs.readFileSync(path.join(dir, f), "utf8");
      const title = source.split("\n").filter((l) => l.startsWith("#")).map((l) => l.replace(/^#\s*/, "")).join(" ");
      const exp = path.join(dir, f.replace(/\.rb$/, ".expected.json"));
      out.push({ verb, name: f.replace(/\.rb$/, ""), path: `specs/${verb}/${f}`, title, source,
                 expected: fs.existsSync(exp) ? JSON.parse(fs.readFileSync(exp, "utf8")) : null });
    }
  }
  return out;
}

export const specsJSON = (ROOT) => JSON.stringify(listSpecs(ROOT));
