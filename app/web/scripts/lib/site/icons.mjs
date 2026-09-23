// SPDX-License-Identifier: AGPL-3.0-or-later
// Tabler's outline icons (@tabler/icons), the set the app draws its own from, inline so they take the theme's ink.
import fs from "node:fs";
import path from "node:path";

const TABLER = path.resolve(path.dirname(new URL(import.meta.url).pathname), "../../../node_modules/@tabler/icons/icons/outline");

// class: tb-icon for a list's icon, dl-mark for a row's action, dl-glyph for a mark that means something.
// label: what the icon says, for one that is not decoration.
export function icon(name, { class: cls = "tb-icon", label } = {}) {
  const f = path.join(TABLER, `${name}.svg`);
  if (!fs.existsSync(f)) throw new Error(`no Tabler icon ${name}`);
  const inner = fs.readFileSync(f, "utf8").replace(/<svg[^>]*>|<\/svg>/g, "").replace(/<path stroke="none" d="M0 0h24v24H0z" fill="none" \/>\s*/, "").trim();
  const says = label ? ` role="img" aria-label="${label}"` : "";
  const hidden = label ? "" : ' aria-hidden="true"';
  return `<svg class="${cls}"${says} viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"${hidden}>${inner}</svg>`;
}
