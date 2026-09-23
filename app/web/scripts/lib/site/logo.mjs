// SPDX-License-Identifier: AGPL-3.0-or-later
// Sonic Pi's mark, from native's square logo (app/gui/images/logo-square.svg), in the two forms the pages use:
//
//   punched  the hero's: native's splash treatment, the tile in the accent with the glyphs cut through it
//   icon     the Home entry in a page's list: the tile in currentColor, the π and arcs on it in --icon-ink,
//            the wordmark left out and the glyphs enlarged, as the app's icon has them
//   glyph    the bar's Home: the π and arcs alone in currentColor, no tile
//
// Made from native's file at each build, so a change to the logo reaches the pages.
import fs from "node:fs";

export function logo(file) {
  if (!fs.existsSync(file)) return null;
  const svg = fs.readFileSync(file, "utf8").replace(/<\?xml[^>]*>\s*/, "");
  const open = /<svg[^>]*>/.exec(svg)[0].replace(/ (width|height)="\d+"/g, "");
  const defs = /<defs>[\s\S]*?<\/defs>/.exec(svg)[0];
  const tile = /<path[^>]*fill="rgb\(0%, 0%, 0%\)"[^>]*\/>/.exec(svg)[0];                    // the black square
  const glyphs = svg.slice(svg.indexOf(tile) + tile.length, svg.lastIndexOf("</svg>"));     // the white π, waves and wordmark
  const WHITE = /fill="rgb\(100%, 100%, 100%\)"/g;

  const holes = `<mask id="logo-holes" maskUnits="userSpaceOnUse" x="0" y="0" width="323" height="311"><rect width="323" height="311" fill="white"/>${glyphs.replace(WHITE, 'fill="black"')}</mask>`;
  const punched = `${open.replace("<svg ", '<svg class="logo-punched" role="img" aria-label="Sonic Pi" ')}${defs.replace("</defs>", `${holes}</defs>`)}${tile.replace(/fill="rgb\(0%, 0%, 0%\)"/, 'fill="currentColor" mask="url(#logo-holes)"')}</svg>`;

  const noWordmark = glyphs.replace(/<g fill="rgb\(100%, 100%, 100%\)" fill-opacity="1">\s*<use xlink:href="#glyph-4[\s\S]*?<\/g>/g, "").replace(WHITE, 'fill="var(--icon-ink, white)"');
  const icon = `${open.replace("<svg ", '<svg class="tb-icon logo-icon" aria-hidden="true" ')}${defs}<rect x="0.4" y="0.9" width="322" height="310" rx="70" fill="currentColor"/><g transform="translate(161 156) scale(1.3) translate(-155 -94)">${noWordmark}</g></svg>`;

  // the mark alone, no tile: the π and its arcs in currentColor, cropped to them (the bar's Home, beside its line icons)
  const bare = glyphs.replace(/<g fill="rgb\(100%, 100%, 100%\)" fill-opacity="1">\s*<use xlink:href="#glyph-4[\s\S]*?<\/g>/g, "").replace(WHITE, 'fill="currentColor"');
  const glyph = `${open.replace("<svg ", '<svg class="tb-icon logo-glyph" aria-hidden="true" ').replace(/viewBox="[^"]*"/, 'viewBox="64 33 192 151"')}${defs}${bare}</svg>`;

  return { punched, icon, glyph };
}
