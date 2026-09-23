// SPDX-License-Identifier: AGPL-3.0-or-later
// The Patreon supporters on the "in the credits" tier: native's CONTRIBUTORS.md, its "Patreon Supporters" section.
// Its thanks (markdown) and the names alone: a supporter's link in the file (a site, a profile) is not carried.
import fs from "node:fs";
import path from "node:path";

export function patreonSupporters(nativeDir) {
  const file = path.join(nativeDir, "CONTRIBUTORS.md");
  if (!fs.existsSync(file)) return null;
  const m = /^## Patreon Supporters\n([\s\S]*?)(?=^## )/m.exec(fs.readFileSync(file, "utf8"));
  if (!m) return null;
  const [intro, ...rest] = m[1].trim().split(/\n(?=\* )/);
  const names = rest.map((l) => l.replace(/^\*\s+/, "").replace(/\[([^\]]+)\]\([^)]*\)/g, "$1").trim()).filter(Boolean);
  return { intro: intro.trim().replace(/\s*\n\s*/g, " "), names };
}
