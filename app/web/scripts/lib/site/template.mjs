// SPDX-License-Identifier: AGPL-3.0-or-later
// The site's templates are plain HTML. A {{…}} fills in what the HTML cannot say itself:
//
//   {{version}}                            a value
//   {{icon check}}                         a helper, given its words
//   {{icon browser class="dl-mark"}}       …and its options
//   {{youtube sHeTmUyttjQ "A lesson"}}     a quoted word may have spaces
//
// An unknown name is an error, so a typo fails the build rather than shipping.

const TAG = /\{\{\s*([\w-]+)((?:\s+(?:[\w-]+="[^"]*"|"[^"]*"|[^\s"}]+))*)\s*\}\}/g;
const WORD = /([\w-]+)="([^"]*)"|"([^"]*)"|([^\s"}]+)/g;

export function render(src, helpers, where = "template") {
  return src.replace(TAG, (_, name, rest) => {
    if (!(name in helpers)) throw new Error(`${where}: {{${name}}} is not something the site knows`);
    const h = helpers[name];
    if (typeof h !== "function") return String(h);
    const args = [], opts = {};
    for (const [, key, value, quoted, bare] of rest.matchAll(WORD)) {
      if (key) opts[key] = value;
      else args.push(quoted ?? bare);
    }
    return h(...args, opts);
  });
}
