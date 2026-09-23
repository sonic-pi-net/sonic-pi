// SPDX-License-Identifier: AGPL-3.0-or-later
// Tabler's outline icons (the set native's toolbar draws from, @tabler/icons), the few the GUI draws inline
// where a glyph stands for an action: never a typed character or a CSS shape. viewBox 0 0 24 24, a 2px round stroke.
const PATHS = {
  x: '<path d="M18 6l-12 12"/><path d="M6 6l12 12"/>',
  "chevron-left": '<path d="M15 6l-6 6l6 6"/>',
  "chevron-right": '<path d="M9 6l6 6l-6 6"/>',
  "chevron-up": '<path d="M6 15l6 -6l6 6"/>',
  "chevron-down": '<path d="M6 9l6 6l6 -6"/>',
  copy: '<path d="M7 9.667a2.667 2.667 0 0 1 2.667 -2.667h8.666a2.667 2.667 0 0 1 2.667 2.667v8.666a2.667 2.667 0 0 1 -2.667 2.667h-8.666a2.667 2.667 0 0 1 -2.667 -2.667l0 -8.666"/><path d="M4.012 16.737a2.005 2.005 0 0 1 -1.012 -1.737v-10c0 -1.1 .9 -2 2 -2h10c.75 0 1.158 .385 1.5 1"/>',
  qrcode: '<path d="M4 5a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v4a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -4"/><path d="M7 17l0 .01"/><path d="M14 5a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v4a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -4"/><path d="M7 7l0 .01"/><path d="M4 15a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v4a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -4"/><path d="M17 7l0 .01"/><path d="M14 14l3 0"/><path d="M20 14l0 .01"/><path d="M14 14l0 3"/><path d="M14 20l3 0"/><path d="M17 17l3 0"/><path d="M20 17l0 3"/>',
  link: '<path d="M9 15l6 -6"/><path d="M11 6l.463 -.536a5 5 0 0 1 7.071 7.072l-.534 .464"/><path d="M13 18l-.397 .534a5.068 5.068 0 0 1 -7.127 0a4.972 4.972 0 0 1 0 -7.071l.524 -.463"/>',
  download: '<path d="M4 17v2a2 2 0 0 0 2 2h12a2 2 0 0 0 2 -2v-2"/><path d="M7 11l5 5l5 -5"/><path d="M12 4l0 12"/>',
  "file-code": '<path d="M14 3v4a1 1 0 0 0 1 1h4"/><path d="M17 21h-10a2 2 0 0 1 -2 -2v-14a2 2 0 0 1 2 -2h7l5 5v11a2 2 0 0 1 -2 2"/><path d="M10 13l-1 2l1 2"/><path d="M14 13l1 2l-1 2"/>',
  "stack-2": '<path d="M12 4l-8 4l8 4l8 -4l-8 -4"/><path d="M4 12l8 4l8 -4"/><path d="M4 16l8 4l8 -4"/>',
  check: '<path d="M5 12l5 5l10 -10"/>',
  "layout-grid": '<path d="M4 5a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v4a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -4"/><path d="M14 5a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v4a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -4"/><path d="M4 15a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v4a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -4"/><path d="M14 15a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v4a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -4"/>',
  pencil: '<path d="M4 20h4l10.5 -10.5a2.828 2.828 0 1 0 -4 -4l-10.5 10.5v4"/><path d="M13.5 6.5l4 4"/>',
  trash: '<path d="M4 7l16 0"/><path d="M10 11l0 6"/><path d="M14 11l0 6"/><path d="M5 7l1 12a2 2 0 0 0 2 2h8a2 2 0 0 0 2 -2l1 -12"/><path d="M9 7v-3a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v3"/>',
  "player-play": '<path d="M7 4v16l13 -8l-13 -8"/>',
  "player-stop": '<path d="M5 7a2 2 0 0 1 2 -2h10a2 2 0 0 1 2 2v10a2 2 0 0 1 -2 2h-10a2 2 0 0 1 -2 -2l0 -10"/>',
  "arrow-up-right": '<path d="M17 7l-10 10"/><path d="M8 7l9 0l0 9"/>',
  // the cards' and the docs pane's actions (native's tablericons.h: SquareChevronsUp, Copy, Check, Texture; Tabler's arrow-back-up, pencil, external-link)
  insert: '<path d="M9 16l3 -3l3 3"/><path d="M9 11l3 -3l3 3"/><path d="M3 5a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v14a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-14"/>',
  copy: '<path d="M7 9.667a2.667 2.667 0 0 1 2.667 -2.667h8.666a2.667 2.667 0 0 1 2.667 2.667v8.666a2.667 2.667 0 0 1 -2.667 2.667h-8.666a2.667 2.667 0 0 1 -2.667 -2.667l0 -8.666"/><path d="M4.012 16.737a2 2 0 0 1 -1.012 -1.737v-10c0 -1.1 .9 -2 2 -2h10c.75 0 1.158 .385 1.5 1"/>',
  check: '<path d="M5 12l5 5l10 -10"/>',
  drag: '<path d="M6 3l-3 3"/><path d="M21 18l-3 3"/><path d="M11 3l-8 8"/><path d="M16 3l-13 13"/><path d="M21 3l-18 18"/><path d="M21 8l-13 13"/><path d="M21 13l-8 8"/>',
  reset: '<path d="M4.5 12a7.5 7.5 0 1 0 2.2-5.3"/><path d="M4.5 3.5v4h4"/>',
  edit: '<path d="M4 20h4l10.5 -10.5a2.828 2.828 0 1 0 -4 -4l-10.5 10.5v4"/><path d="M13.5 6.5l4 4"/>',
  open: '<path d="M12 6h-6a2 2 0 0 0 -2 2v10a2 2 0 0 0 2 2h10a2 2 0 0 0 2 -2v-6"/><path d="M11 13l9 -9"/><path d="M15 4h5v5"/>',
  wave: '<path d="M2 9c3-5 6-5 9 0s6 5 9 0"/><path d="M2 15c3-5 6-5 9 0s6 5 9 0"/>',
  search: '<circle cx="10.5" cy="10.5" r="6.5"/><path d="m15.5 15.5 5 5"/>',
  // a step smaller or larger, lower or higher (the panes' zoom, the piano's octave): Tabler's circle-minus and circle-plus
  "circle-minus": '<path d="M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0"/><path d="M9 12l6 0"/>',
  dots: '<path d="M4 12a1 1 0 1 0 2 0a1 1 0 1 0 -2 0"/><path d="M11 12a1 1 0 1 0 2 0a1 1 0 1 0 -2 0"/><path d="M18 12a1 1 0 1 0 2 0a1 1 0 1 0 -2 0"/>',
  plus: '<path d="M12 5l0 14"/><path d="M5 12l14 0"/>',
  "circle-plus": '<path d="M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0"/><path d="M9 12l6 0"/><path d="M12 9l0 6"/>',
  // the preferences, one glyph each as native v5's have them (settingswidget.cpp's k…Svg, the same Tabler icons);
  // keyboard, alert-triangle and bolt for the web's own, which native has no pref for
  "pref-auto-indent": '<path d="M4 6l16 0"/><path d="M4 12l10 0"/><path d="M4 18l14 0"/>',
  "pref-context": '<path d="M3 12a9 9 0 1 0 18 0a9 9 0 0 0 -18 0"/><path d="M12 9h.01"/><path d="M11 12h1v4h1"/>',
  "pref-metro": '<path d="M14.153 8.188l-.72 -3.236a2.493 2.493 0 0 0 -4.867 0l-3.025 13.614a2 2 0 0 0 1.952 2.434h7.014a2 2 0 0 0 1.952 -2.434l-.524 -2.357m-4.935 1.791l9 -13"/><path d="M19 5a1 1 0 1 0 2 0a1 1 0 1 0 -2 0"/>',
  "pref-loop-scopes": '<path d="M3 12h4.5l1.5 -6l4 12l2 -9l1.5 3h4.5"/>',
  "pref-loop-scroll": '<path d="M7 7l5 5l-5 5"/><path d="M13 7l5 5l-5 5"/>',
  "pref-safe": '<path d="M12 3a12 12 0 0 0 8.5 3a12 12 0 0 1 -8.5 15a12 12 0 0 1 -8.5 -15a12 12 0 0 0 8.5 -3"/>',
  "pref-external": '<path d="M12 21l-8 -4.5v-9l8 -4.5l8 4.5v4.5"/><path d="M12 12l8 -4.5"/><path d="M12 12v9"/><path d="M12 12l-8 -4.5"/><path d="M22 18h-7"/><path d="M18 15l-3 3l3 3"/>',
  "pref-speak": '<path d="M15 8a5 5 0 0 1 0 8"/><path d="M17.7 5a9 9 0 0 1 0 14"/><path d="M6 15h-2a1 1 0 0 1 -1 -1v-4a1 1 0 0 1 1 -1h2l3.5 -4.5a.8 .8 0 0 1 1.5 .5v14a.8 .8 0 0 1 -1.5 .5l-3.5 -4.5"/>',
  "pref-motion": '<path d="M6 6a1 1 0 0 1 1 -1h2a1 1 0 0 1 1 1v12a1 1 0 0 1 -1 1h-2a1 1 0 0 1 -1 -1l0 -12"/><path d="M14 6a1 1 0 0 1 1 -1h2a1 1 0 0 1 1 1v12a1 1 0 0 1 -1 1h-2a1 1 0 0 1 -1 -1l0 -12"/>',
  "pref-gamepad": '<path d="M12 5h3.5a5 5 0 0 1 0 10h-5.5l-4.015 4.227a2.3 2.3 0 0 1 -3.923 -2.035l1.634 -8.173a5 5 0 0 1 4.904 -4.019h3.4"/><path d="M14 15l4.07 4.284a2.3 2.3 0 0 0 3.925 -2.023l-1.6 -8.232"/><path d="M8 9v2"/><path d="M7 10h2"/><path d="M14 10h2"/>',
  "pref-monochrome": '<path d="M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0"/><path d="M12 17a5 5 0 0 0 0 -10v10"/>',
  "pref-invert": '<path d="M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0"/><path d="M12 3v18"/><path d="M12 14l7 -7"/><path d="M12 19l8.5 -8.5"/><path d="M12 9l4.5 -4.5"/>',
  "pref-keyboard": '<path d="M2 8a2 2 0 0 1 2 -2h16a2 2 0 0 1 2 2v8a2 2 0 0 1 -2 2h-16a2 2 0 0 1 -2 -2l0 -8"/><path d="M6 10l0 .01"/><path d="M10 10l0 .01"/><path d="M14 10l0 .01"/><path d="M18 10l0 .01"/><path d="M6 14l0 .01"/><path d="M18 14l0 .01"/><path d="M10 14l4 .01"/>',
  "pref-warn": '<path d="M12 9v4"/><path d="M10.363 3.591l-8.106 13.534a1.914 1.914 0 0 0 1.636 2.871h16.214a1.914 1.914 0 0 0 1.636 -2.87l-8.106 -13.536a1.914 1.914 0 0 0 -3.274 0z"/><path d="M12 16h.01"/>',
  "pref-midi": '<path d="M3 7a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v10a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-10"/><path d="M9 19v-6"/><path d="M8 5v8h2v-8"/><path d="M15 19v-6"/><path d="M14 5v8h2v-8"/>',
  // the scope's views, as native's scope kinds draw them (settingswidget.cpp kScope…Svg): chart-bar for the level
  // meter and its bars, one wave for mono, zodiac-aquarius's two for stereo, whirl for Lissajous
  "scope-levels": '<path d="M3 13a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v6a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -6"/><path d="M15 9a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v10a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -10"/><path d="M9 5a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v14a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -14"/><path d="M4 20h14"/>',
  "scope-mono": '<path d="M3 13.5l3 -3l3 3l3 -3l3 3l3 -3l3 3"/>',
  "scope-line": '<path d="M3 12h5l1.5 -3l2 6l2 -6l1.5 3h6"/>',
  "scope-stereo": '<path d="M3 10l3 -3l3 3l3 -3l3 3l3 -3l3 3"/><path d="M3 17l3 -3l3 3l3 -3l3 3l3 -3l3 3"/>',
  "scope-lissajous": '<path d="M14 12a2 2 0 1 0 -4 0a2 2 0 0 0 4 0"/><path d="M12 21c-3.314 0 -6 -2.462 -6 -5.5s2.686 -5.5 6 -5.5"/><path d="M21 12c0 3.314 -2.462 6 -5.5 6s-5.5 -2.686 -5.5 -6"/><path d="M12 14c3.314 0 6 -2.462 6 -5.5s-2.686 -5.5 -6 -5.5"/><path d="M14 12c0 -3.314 -2.462 -6 -5.5 -6s-5.5 2.686 -5.5 6"/>',
  "pref-latency": '<path d="M13 3l0 7l6 0l-8 11l0 -7l-6 0l8 -11"/>',
};
/** An icon's paths alone, for an <svg> already on the page. */
export const paths = (name) => { const d = PATHS[name]; if (!d) throw new Error(`no icon ${name}`); return d; };

/** The icon as inline SVG markup (class tb-icon, sized by the CSS around it). */
export function icon(name, cls = "tb-icon") {
  const d = PATHS[name];
  if (!d) throw new Error(`no icon ${name}`);
  return `<svg class="${cls}" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">${d}</svg>`;
}
